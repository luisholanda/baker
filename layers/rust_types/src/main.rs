use std::{collections::HashMap, io};

use baker_ir_pb::{
    r#type::{Fundamental, Name},
    type_def::{record::Property, sum::Member, Definition, ImplBlock, Record, Sum},
    Attribute, Block, Function, FunctionCall, IdentifierPath, IrFile, Namespace, Type, TypeDef,
    Value, Visibility,
};
use baker_layer_pb::{LayerRequest, LayerResponse};
use baker_pkg_pb::{
    message::{field::Label, Field, OneOf},
    Enum, File, Message, PackageGraph,
};
use heck::CamelCase;

// TODO: Generate std::fmt::Display for enums
// TODO: Generate `all` and `count` static methods for enums

const DEFAULT_FIELD_VISIBILITY_OPTION: &str = "baker.default_field_visibility";
const FIELD_VISIBILITY_OPTION: &str = "baker.field_visibility";

fn layer_impl(req: LayerRequest) -> std::io::Result<LayerResponse> {
    let mut pkg = req.packages.expect("no packages received from baker");

    let mut resp = LayerResponse::default();

    let mut files = Vec::with_capacity(pkg.main_files.len());
    for file_id in &pkg.main_files {
        files.push(pkg.files.remove(file_id).expect("main file not found"));
    }

    resp.ir_files = files
        .into_iter()
        .map(|f| generate_file(f, &mut pkg))
        .collect::<io::Result<_>>()?;

    Ok(resp)
}

fn generate_file(file: File, graph: &mut PackageGraph) -> io::Result<IrFile> {
    let mut ir_file = IrFile {
        root: Some(Namespace::default()),
        ..Default::default()
    };
    let root = ir_file.root.as_mut().unwrap();

    for msg in file.root_messages {
        let mut msg = graph.messages.remove(&msg).expect("message not found");
        let namespace_name = msg.name.rsplit_once('.').unwrap().1.to_string();
        let nested = generate_msg_namespace(&msg, graph)?;

        if nested != Namespace::default() {
            root.nested_namespaces.insert(namespace_name, nested);
        }

        let def = generate_message_type(&mut msg, graph)?;

        root.types.push(def);

        graph.messages.insert(msg.id, msg);
    }

    for en in file.root_enums {
        let enum_ = graph.enums.remove(&en).expect("enum not found");
        let def = generate_enum_type(enum_);

        root.types.push(def);
    }

    Ok(ir_file)
}

fn generate_message_type(msg: &mut Message, graph: &PackageGraph) -> io::Result<TypeDef> {
    let default_visibility = if let Some(val) = msg.options.remove(DEFAULT_FIELD_VISIBILITY_OPTION)
    {
        translate_visibility_opt(val, DEFAULT_FIELD_VISIBILITY_OPTION)?
    } else {
        Visibility::Public
    };

    let mut properties = HashMap::with_capacity(msg.fields.len());
    for mut field in msg.fields.drain(..) {
        properties.insert(
            std::mem::take(&mut field.name),
            Property {
                r#type: Some(translate_field_type(&field, graph)),
                documentation: field.documentation.unwrap_or_default(),
                visibility: if let Some(val) = field.options.remove(FIELD_VISIBILITY_OPTION) {
                    translate_visibility_opt(val, FIELD_VISIBILITY_OPTION)?
                } else {
                    default_visibility
                } as i32,
                number: field.number as i32,
                ..Default::default()
            },
        );
    }

    for oneof in &msg.oneofs {
        properties.insert(
            oneof.name.clone(),
            Property {
                r#type: Some(Type {
                    name: Some(Name::Identifier({
                        let mut identifier = IdentifierPath::from_dotted_path(&msg.name);
                        identifier
                            .segments
                            .push(baker_ir_pb::identifier_path::Segment {
                                name: oneof.name.to_camel_case(),
                                ..Default::default()
                            });

                        Box::new(identifier)
                    })),
                    ..Default::default()
                }),
                documentation: oneof.documentation.clone().unwrap_or_default(),
                visibility: if let Some(val) = oneof.options.get(FIELD_VISIBILITY_OPTION) {
                    translate_visibility_opt(val.clone(), FIELD_VISIBILITY_OPTION)?
                } else {
                    default_visibility
                } as i32,
                number: oneof
                    .fields
                    .iter()
                    .map(|f| f.number as i32)
                    .min()
                    .unwrap_or_default(),
                ..Default::default()
            },
        );
    }

    // TODO: use the `default` option when generating `impl Default`.
    Ok(TypeDef {
        header: Some(Type::with_name(&msg.name)),
        definition: Some(Definition::Record(Record { properties })),
        documentation: msg.documentation.take().unwrap_or_default(),
        attributes: vec![derive_call(&["Clone", "Debug", "Default", "PartialEq"])],
        visibility: Visibility::Public as i32,
        ..Default::default()
    })
}

fn generate_enum_type(enum_: Enum) -> TypeDef {
    let mut members = HashMap::with_capacity(enum_.values.len());
    let mut default_member = String::new();

    let enum_type = Type::with_name(&enum_.name);

    for val in enum_.values {
        if val.value == 0 {
            default_member = format!("Self.{}", val.name.to_camel_case());
        }

        members.insert(
            val.name.to_camel_case(),
            Member {
                documentation: val.documentation.unwrap_or_default(),
                value: Some(baker_ir_pb::type_def::sum::member::Value::Fixed(val.value)),
                attributes: vec![],
                number: val.value,
            },
        );
    }

    let mut impl_blocks = vec![];

    if !default_member.is_empty() {
        let default_impl = ImplBlock {
            interface: Some(Type::with_path(
                IdentifierPath::from_dotted_path("std.default.Default").global(),
            )),
            methods: vec![Function {
                header: Some(Type::with_name("default")),
                implementation: Some(Block {
                    statements: vec![],
                    return_value: Some(Value::identifier(IdentifierPath::from_dotted_path(
                        &default_member,
                    ))),
                }),
                r#return: Some(Type::with_fundamental(Fundamental::Self_)),
                ..Default::default()
            }],
            ..Default::default()
        };

        impl_blocks.push(default_impl);
    }

    TypeDef {
        header: Some(enum_type),
        definition: Some(Definition::Sum(Sum { members })),
        attributes: vec![derive_call(&[
            "Debug",
            "Copy",
            "Clone",
            "PartialEq",
            "Eq",
            "Hash",
            "PartialOrd",
            "Ord",
        ])],
        blocks: impl_blocks,
        documentation: enum_.documentation.unwrap_or_default(),
        visibility: Visibility::Public as i32,
        ..Default::default()
    }
}

fn generate_msg_namespace(msg: &Message, graph: &mut PackageGraph) -> io::Result<Namespace> {
    let mut namespace = Namespace::default();

    for n_msg_id in &msg.messages {
        let mut msg = graph.messages.remove(n_msg_id).expect("message not found");
        let nested = generate_msg_namespace(&msg, graph)?;

        let namespace_name = msg.name.rsplit_once('.').unwrap().1.to_string();
        if nested != Namespace::default() {
            namespace.nested_namespaces.insert(namespace_name, nested);
        }

        let def = generate_message_type(&mut msg, &graph)?;
        namespace.types.push(def);

        graph.messages.insert(*n_msg_id, msg);
    }

    for en in &msg.enums {
        let enum_ = graph.enums.get(en).expect("enum not found");
        let def = generate_enum_type(enum_.clone());

        namespace.types.push(def);
    }

    for oneof in &msg.oneofs {
        let def = generate_oneof_type(oneof, msg, &graph);
        namespace.types.push(def);
    }

    Ok(namespace)
}

fn generate_oneof_type(oneof: &OneOf, msg: &Message, graph: &PackageGraph) -> TypeDef {
    let mut members = HashMap::with_capacity(oneof.fields.len());

    for f in &oneof.fields {
        members.insert(
            f.name.to_camel_case(),
            Member {
                attributes: vec![],
                documentation: f.documentation().to_string(),
                value: Some(baker_ir_pb::type_def::sum::member::Value::Type(
                    translate_field_type(f, graph),
                )),
                number: f.number as i32,
            },
        );
    }

    members.insert(
        "NotSet".to_string(),
        Member {
            attributes: vec![],
            documentation: r"Default value for the OneOf. Used when no field is set.".to_string(),
            value: None,
            number: oneof.fields.len() as i32,
        },
    );

    let not_set = Value::identifier(IdentifierPath::from_dotted_path("Self.NotSet"));

    let default_impl = ImplBlock {
        interface: Some(Type::with_path(
            IdentifierPath::from_dotted_path("std.default.Default").global(),
        )),
        methods: vec![Function {
            header: Some(Type::with_name("default")),
            r#return: Some(Type::with_fundamental(Fundamental::Self_)),
            implementation: Some(Block {
                statements: vec![],
                return_value: Some(not_set.clone()),
            }),
            ..Default::default()
        }],
        ..Default::default()
    };

    let self_ = Value::identifier(IdentifierPath::from_dotted_path("self"));
    let is_set_impl = ImplBlock {
        methods: vec![Function {
            header: Some(Type::with_name("is_set")),
            receiver: Some(Type::with_fundamental(Fundamental::ShrdRef)),
            r#return: Some(Type::with_fundamental(Fundamental::Bool)),
            visibility: Visibility::Public as i32,
            implementation: Some(Block {
                statements: vec![],
                return_value: Some(
                    self_.operate(baker_ir_pb::value::bin_op::Op::Ne, not_set.const_ref()),
                ),
            }),
            ..Default::default()
        }],
        ..Default::default()
    };

    TypeDef {
        header: Some(Type::with_name_and_scope(
            &msg.name,
            oneof.name.to_camel_case(),
        )),
        definition: Some(Definition::Sum(Sum { members })),
        documentation: oneof.documentation().to_string(),
        attributes: vec![derive_call(&["Debug", "Clone", "PartialEq"])],
        visibility: Visibility::Public as i32,
        blocks: vec![is_set_impl, default_impl],
        ..Default::default()
    }
}

fn translate_field_type(field: &Field, graph: &PackageGraph) -> Type {
    let value_type = field
        .r#type
        .clone()
        .map(|t| translate_type(t, graph))
        .unwrap();

    if let Some(key_type) = field.key_type {
        Type {
            generics: vec![
                translate_type(
                    baker_pkg_pb::Type {
                        value: Some(baker_pkg_pb::r#type::Value::Bultin(key_type)),
                    },
                    graph,
                ),
                value_type,
            ],
            ..Type::with_fundamental(Fundamental::Map)
        }
    } else {
        match field.label() {
            Label::Unset => value_type,
            Label::Optional => Type {
                generics: vec![value_type],
                ..Type::with_fundamental(Fundamental::Optional)
            },
            Label::Repeated => Type {
                generics: vec![value_type],
                ..Type::with_fundamental(Fundamental::Vec)
            },
        }
    }
}

fn translate_type(pkg_typ: baker_pkg_pb::Type, graph: &PackageGraph) -> Type {
    use baker_pkg_pb::r#type::*;

    let typ;

    match pkg_typ.value.unwrap() {
        Value::Bultin(b) => {
            typ = Type::with_fundamental(match BuiltIn::from_i32(b).unwrap() {
                BuiltIn::Unknown => Fundamental::Unknown,
                BuiltIn::Double => Fundamental::Double,
                BuiltIn::Float => Fundamental::Float,
                BuiltIn::SInt => Fundamental::SInt,
                BuiltIn::SLong => Fundamental::SLong,
                BuiltIn::UInt => Fundamental::UInt,
                BuiltIn::ULong => Fundamental::ULong,
                BuiltIn::Bool => Fundamental::Bool,
                BuiltIn::String => Fundamental::String,
                BuiltIn::Bytes => Fundamental::Bytes,
            });
        }
        Value::Custom(id) => {
            if let Some(msg) = graph.messages.get(&id) {
                typ = Type::with_name(&msg.name);
            } else if let Some(enum_) = graph.enums.get(&id) {
                typ = Type::with_name(&enum_.name);
            } else {
                panic!("could not find type with id {} in graph", id);
            }
        }
    }

    typ
}

fn translate_visibility_opt(val: baker_pkg_pb::option::Value, opt: &str) -> io::Result<Visibility> {
    let visibility = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "PUBLIC" => Visibility::Public,
            "PRIVATE" => Visibility::Private,
            "PROTECTED" => Visibility::Protected,
            "PACKAGE" => Visibility::Package,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Invalid value for option '{}': {}", opt, ident),
                ))
            }
        },
        v => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid value for option '{}': {:?}", opt, v),
            ))
        }
    };

    Ok(visibility)
}

fn derive_call(traits: &[&str]) -> Attribute {
    use baker_ir_pb::attribute::Value as AttrValue;
    Attribute {
        value: Some(AttrValue::Call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("derive")),
            args: traits
                .iter()
                .map(|t| IdentifierPath::from_dotted_path(&t))
                .map(Value::identifier)
                .collect(),
            ..Default::default()
        })),
    }
}

fn main() -> std::io::Result<()> {
    baker_layer_pb::execute_flow(layer_impl)
}
