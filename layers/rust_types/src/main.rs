use std::io;

use baker_ir_pb::{
    r#type::Fundamental,
    type_def::{record::Property, sum::Member, Definition, ImplBlock, Record, Sum},
    Block, Function, FunctionCall, IrFile, Namespace, Statement, Type, TypeDef, Value, Visibility,
};
use baker_layer_pb::{LayerRequest, LayerResponse};
use baker_pkg_pb::{
    message::{field::Label, Field, OneOf},
    Enum, File, Message, PackageGraph,
};
use heck::CamelCase;

const DEFAULT_FIELD_VISIBILITY_OPTION: &str = "baker.default_field_visibility";
const FIELD_VISIBILITY_OPTION: &str = "baker.visibility";

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
        let def = generate_message_type(&mut msg, graph)?;

        root.types.push(def);

        let nested = generate_msg_namespace(&msg, graph)?;
        let namespace_name = msg.name.rsplit_once('.').unwrap().1.to_string();
        root.nested_namespaces.insert(namespace_name, nested);

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

    let mut properties = Vec::with_capacity(msg.fields.len());
    for mut field in msg.fields.drain(..) {
        properties.push(Property {
            name: std::mem::take(&mut field.name),
            r#type: Some(translate_field_type(&field, graph)),
            documentation: field.documentation.unwrap_or_default(),
            visibility: if let Some(val) = field.options.remove(FIELD_VISIBILITY_OPTION) {
                translate_visibility_opt(val, FIELD_VISIBILITY_OPTION)?
            } else {
                default_visibility
            } as i32,
            ..Default::default()
        });
    }

    for oneof in &msg.oneofs {
        properties.push(Property {
            name: oneof.name.clone(),
            r#type: Some(Type {
                name: format!("{}.{}", msg.name, oneof.name.to_camel_case()),
                ..Default::default()
            }),
            documentation: oneof.documentation.clone().unwrap_or_default(),
            visibility: if let Some(val) = oneof.options.get(FIELD_VISIBILITY_OPTION) {
                translate_visibility_opt(val.clone(), FIELD_VISIBILITY_OPTION)?
            } else {
                default_visibility
            } as i32,
            ..Default::default()
        });
    }

    // Generated oneof enums don't implement Default.
    //
    // TODO: add an option to define the default variant.
    // TODO: use the `default` option when generating `impl Default`.
    let default_traits: &[&str] = if msg.oneofs.is_empty() {
        &["Clone", "Debug", "PartialEq"]
    } else {
        &["Clone", "Debug", "Default", "PartialEq"]
    };

    Ok(TypeDef {
        header: Some(Type {
            name: std::mem::take(&mut msg.name),
            ..Default::default()
        }),
        definition: Some(Definition::Record(Record { properties })),
        attributes: vec![derive_call(default_traits)],
        visibility: Visibility::Public as i32,
        ..Default::default()
    })
}

fn generate_enum_type(enum_: Enum) -> TypeDef {
    let mut members = Vec::with_capacity(enum_.values.len());
    let mut default_member = String::new();

    let enum_type = Type {
        name: enum_.name,
        ..Default::default()
    };

    for val in enum_.values {
        if val.value == 0 {
            default_member = format!("{}.{}", enum_type.name, val.name.to_camel_case());
        }

        members.push(Member {
            name: val.name.to_camel_case(),
            documentation: val.documentation.unwrap_or_default(),
            value: Some(baker_ir_pb::type_def::sum::member::Value::Fixed(val.value)),
            attributes: vec![],
        })
    }

    let mut impl_blocks = vec![];

    if default_member.is_empty() {
        let default_impl = ImplBlock {
            interface: Some(Type {
                name: "Default".to_owned(),
                ..Default::default()
            }),
            methods: vec![Function {
                header: Some(Type {
                    name: "Default".to_string(),
                    ..Default::default()
                }),
                implementation: Some(Block {
                    statements: vec![Statement {
                        statement: Some(baker_ir_pb::statement::Statement::Return(Value {
                            value: Some(baker_ir_pb::value::Value::Identifier(default_member)),
                        })),
                    }],
                }),
                r#return: Some(enum_type.clone()),
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
        visibility: Visibility::Public as i32,
        ..Default::default()
    }
}

fn generate_msg_namespace(msg: &Message, graph: &mut PackageGraph) -> io::Result<Namespace> {
    let mut namespace = Namespace::default();

    for n_msg_id in &msg.messages {
        let mut msg = graph.messages.remove(n_msg_id).expect("message not found");
        let def = generate_message_type(&mut msg, &graph)?;

        let nested = generate_msg_namespace(&msg, graph)?;

        let namespace_name = msg.name.rsplit_once('.').unwrap().1.to_string();
        namespace.nested_namespaces.insert(namespace_name, nested);
        namespace.types.push(def);

        graph.messages.insert(*n_msg_id, msg);
    }

    for en in &msg.enums {
        let enum_ = graph.enums.get(en).expect("enum not found");
        let def = generate_enum_type(enum_.clone());

        namespace.types.push(def);
    }

    for oneof in &msg.oneofs {
        let def = generate_oneof_type(oneof, &graph);
        namespace.types.push(def);
    }

    Ok(namespace)
}

fn generate_oneof_type(oneof: &OneOf, graph: &PackageGraph) -> TypeDef {
    let mut members = Vec::with_capacity(oneof.fields.len());

    for f in &oneof.fields {
        members.push(Member {
            name: f.name.to_camel_case(),
            attributes: vec![],
            documentation: f.documentation().to_string(),
            value: Some(baker_ir_pb::type_def::sum::member::Value::Type(
                translate_field_type(f, graph),
            )),
        });
    }

    TypeDef {
        header: Some(Type {
            name: oneof.name.to_camel_case(),
            ..Default::default()
        }),
        definition: Some(Definition::Sum(Sum { members })),
        documentation: oneof.documentation().to_string(),
        attributes: vec![derive_call(&["Debug", "Clone", "PartialEq"])],
        visibility: Visibility::Public as i32,
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
            fundamental: Fundamental::Map as i32,
            generics: vec![
                translate_type(
                    baker_pkg_pb::Type {
                        value: Some(baker_pkg_pb::r#type::Value::Bultin(key_type)),
                    },
                    graph,
                ),
                value_type,
            ],
            ..Default::default()
        }
    } else {
        match field.label() {
            Label::Unset => value_type,
            Label::Optional => Type {
                fundamental: Fundamental::Optional as i32,
                generics: vec![value_type],
                ..Default::default()
            },
            Label::Repeated => Type {
                fundamental: Fundamental::Vec as i32,
                generics: vec![value_type],
                ..Default::default()
            },
        }
    }
}

fn translate_type(pkg_typ: baker_pkg_pb::Type, graph: &PackageGraph) -> Type {
    use baker_pkg_pb::r#type::*;

    let mut typ = Type::default();

    match pkg_typ.value.unwrap() {
        Value::Bultin(b) => {
            typ.fundamental = match BuiltIn::from_i32(b).unwrap() {
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
            } as i32;
        }
        Value::Custom(id) => {
            if let Some(msg) = graph.messages.get(&id) {
                typ.name = msg.name.clone();
            } else if let Some(enum_) = graph.enums.get(&id) {
                typ.name = enum_.name.clone();
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

fn derive_call(traits: &[&str]) -> FunctionCall {
    FunctionCall {
        function: "derive".to_string(),
        args: traits
            .iter()
            .map(|t| Value {
                value: Some(baker_ir_pb::value::Value::Identifier(t.to_string())),
            })
            .collect(),
        ..Default::default()
    }
}

fn main() -> std::io::Result<()> {
    baker_layer_pb::execute_flow(layer_impl)
}
