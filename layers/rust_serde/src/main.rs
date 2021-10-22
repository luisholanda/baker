use std::{collections::HashMap, io};

use baker_api_pb::*;
use baker_ir_pb::{
    type_def::{record::Property, sum::Member, Definition, Record, Sum},
    Attribute, IdentifierPath, IrFile, Namespace, Type, TypeDef,
};
use baker_layer_pb::{LayerRequest, LayerResponse};
use baker_pkg_pb::{
    message::{field::Label, OneOf},
    Enum, File, Message, PackageGraph,
};
use heck::{CamelCase, KebabCase, MixedCase, ShoutyKebabCase, ShoutySnakeCase, SnakeCase};

fn case_to_serde_name(case: NameCase) -> &'static str {
    match case {
        NameCase::CamelCase => "camelCase",
        NameCase::SnakeCase => "snake_case",
        NameCase::KebabCase => "kebab-case",
        NameCase::PascalCase => "PascalCase",
        NameCase::ScreamingSnakeCase => "SCREAMING_SNAKE_CASE",
        NameCase::ScreamingKebabCase => "SCREAMING-KEBAB-CASE",
        NameCase::Unspecified => "",
    }
}

fn layer_impl(req: LayerRequest) -> io::Result<LayerResponse> {
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
    let mut root = Namespace::default();

    for msg_id in file.messages {
        let msg = graph
            .messages
            .remove(&msg_id)
            .expect("missing message declaration");

        root.types.extend(generate_message(msg)?);
    }

    for en_id in file.enums {
        let enum_ = graph
            .enums
            .remove(&en_id)
            .expect("missing enum declaration");

        root.types.push(generate_enum(enum_)?);
    }

    Ok(IrFile {
        file_id: file.id,
        root: Some(root),
    })
}

fn generate_message(mut msg: Message) -> io::Result<Vec<TypeDef>> {
    let default_case = if let Some(val) = msg.options.remove(FIELD_NAME_CASE_MSG_OPTION) {
        translate_name_case_opt(val, FIELD_NAME_CASE_MSG_OPTION)?
    } else {
        NameCase::fields_default()
    };

    let mut properties = HashMap::with_capacity(msg.fields.len());
    for mut field in msg.fields {
        let label = field.label();
        properties.insert(
            field.name,
            Property {
                attributes: handle_field_attrs(&mut field.options, label)?,
                ..Default::default()
            },
        );
    }

    let mut defs = vec![];

    for mut oneof in msg.oneofs {
        defs.push(generate_oneof(&mut oneof, &msg.name, default_case)?);
        properties.insert(
            oneof.name,
            Property {
                attributes: vec![serde_default_attr(), serde_flatten_attr()],
                ..Default::default()
            },
        );
    }

    defs.push(TypeDef {
        header: Some(Type::with_name(&msg.name)),
        definition: Some(Definition::Record(Record { properties })),
        attributes: vec![serde_derive_attr(), serde_rename_all_attr(default_case)],
        ..Default::default()
    });

    Ok(defs)
}

fn handle_field_attrs(
    opts: &mut HashMap<String, baker_pkg_pb::option::Value>,
    label: Label,
) -> io::Result<Vec<Attribute>> {
    let mut attrs = vec![];
    if let Some(val) = opts.remove(FIELD_NAME_FIELD_OPTION) {
        let name = translate_opt_value_to_str(val)?;
        attrs.push(serde_rename_attr(&name, None));
    }

    if let Some(val) = opts.remove(FIELD_ALIAS_FIELD_OPTION) {
        let alias = translate_opt_value_to_str(val)?;
        attrs.push(serde_alias_attr(alias));
    }

    if let Some(val) = opts.remove(FIELD_BEHAVIOR_FIELD_OPTION) {
        let fb = translate_field_behavior_opt(val, FIELD_BEHAVIOR_FIELD_OPTION)?;

        match fb {
            FieldBehavior::InputOnly => attrs.push(serde_generic_arg_attr("skip_serializing")),
            FieldBehavior::OutputOnly => attrs.push(serde_generic_arg_attr("skip_deserializing")),
            _ => {}
        }
    }

    // TODO: integrate with `default` option.
    if label == Label::Optional {
        attrs.push(serde_default_attr());
    }

    Ok(attrs)
}

fn generate_enum(mut enum_: Enum) -> io::Result<TypeDef> {
    fn handle_value_attr(
        opts: &mut HashMap<String, baker_pkg_pb::option::Value>,
    ) -> io::Result<Vec<Attribute>> {
        let mut attrs = vec![];

        if let Some(val) = opts.remove(VALUE_NAME_ENUM_OPTION) {
            let name = translate_opt_value_to_str(val)?;
            attrs.push(serde_rename_attr(&name, None));
        }

        if let Some(val) = opts.remove(VALUE_ALIAS_ENUM_OPTION) {
            let name = translate_opt_value_to_str(val)?;
            attrs.push(serde_alias_attr(name));
        }

        Ok(attrs)
    }

    let default_case = if let Some(val) = enum_.options.remove(VALUE_NAME_CASE_ENUM_OPTION) {
        translate_name_case_opt(val, VALUE_NAME_CASE_ENUM_OPTION)?
    } else {
        NameCase::values_default()
    };

    let mut members = HashMap::with_capacity(enum_.values.len());
    for mut val in enum_.values {
        members.insert(
            val.name.to_camel_case(),
            Member {
                attributes: handle_value_attr(&mut val.options)?,
                ..Default::default()
            },
        );
    }

    Ok(TypeDef {
        header: Some(Type::with_name(&enum_.name)),
        definition: Some(Definition::Sum(Sum { members })),
        attributes: vec![serde_derive_attr(), serde_rename_all_attr(default_case)],
        ..Default::default()
    })
}

fn generate_oneof(
    oneof: &mut OneOf,
    msg_name: &str,
    message_case: NameCase,
) -> io::Result<TypeDef> {
    let oneof_case = if let Some(val) = oneof.options.remove(ONEOF_FIELD_NAME_CASE_ONEOF_OPTION) {
        translate_name_case_opt(val, ONEOF_FIELD_NAME_CASE_ONEOF_OPTION)?
    } else {
        message_case
    };

    let mut members = HashMap::with_capacity(oneof.fields.len());
    for mut field in oneof.fields.drain(..) {
        let label = field.label();
        members.insert(
            field.name.to_camel_case(),
            Member {
                attributes: handle_field_attrs(&mut field.options, label)?,
                ..Default::default()
            },
        );
    }

    Ok(TypeDef {
        header: Some(Type::with_name_and_scope(
            msg_name,
            oneof.name.to_camel_case(),
        )),
        definition: Some(Definition::Sum(Sum { members })),
        attributes: vec![serde_derive_attr(), serde_rename_all_attr(oneof_case)],
        ..Default::default()
    })
}

fn serde_flatten_attr() -> Attribute {
    serde_generic_arg_attr("flatten")
}

fn serde_default_attr() -> Attribute {
    serde_generic_arg_attr("default")
}

fn serde_alias_attr(alias: String) -> Attribute {
    serde_generic_kwargs_attr("alias", alias.to_string())
}

fn serde_rename_attr(base_name: &str, case: Option<NameCase>) -> Attribute {
    let name = if let Some(case) = case {
        match case {
            NameCase::CamelCase => base_name.to_mixed_case(),
            NameCase::SnakeCase => base_name.to_snake_case(),
            NameCase::KebabCase => base_name.to_kebab_case(),
            NameCase::PascalCase => base_name.to_camel_case(),
            NameCase::ScreamingSnakeCase => base_name.to_shouty_snake_case(),
            NameCase::ScreamingKebabCase => base_name.to_shouty_kebab_case(),
            NameCase::Unspecified => base_name.to_string(),
        }
    } else {
        base_name.to_string()
    };

    serde_generic_kwargs_attr("rename", name)
}

fn serde_rename_all_attr(case: NameCase) -> Attribute {
    serde_generic_kwargs_attr("rename_all", case_to_serde_name(case).to_string())
}

fn serde_generic_kwargs_attr(kwarg_name: &str, value: String) -> Attribute {
    Attribute {
        value: Some(baker_ir_pb::attribute::Value::Call(
            baker_ir_pb::FunctionCall {
                function: Some(IdentifierPath::from_dotted_path("serde")),
                kwargs: {
                    let mut map = HashMap::new();
                    map.insert(kwarg_name.to_string(), baker_ir_pb::Value::string(value));

                    map
                },
                ..Default::default()
            },
        )),
    }
}

fn serde_generic_arg_attr(arg: &str) -> Attribute {
    Attribute {
        value: Some(baker_ir_pb::attribute::Value::Call(
            baker_ir_pb::FunctionCall {
                function: Some(IdentifierPath::from_dotted_path("serde")),
                args: vec![baker_ir_pb::Value::identifier(
                    IdentifierPath::from_dotted_path(arg),
                )],
                ..Default::default()
            },
        )),
    }
}

fn serde_derive_attr() -> Attribute {
    derive_call(&["serde.Serialize", "serde.Deserialize"])
}

fn derive_call(traits: &[&str]) -> Attribute {
    use baker_ir_pb::attribute::Value as AttrValue;
    Attribute {
        value: Some(AttrValue::Call(baker_ir_pb::FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("derive")),
            args: traits
                .iter()
                .map(|t| IdentifierPath::from_dotted_path(&t).global())
                .map(baker_ir_pb::Value::identifier)
                .collect(),
            ..Default::default()
        })),
    }
}

fn translate_name_case_opt(val: baker_pkg_pb::option::Value, opt: &str) -> io::Result<NameCase> {
    let case = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "CAMEL_CASE" => NameCase::CamelCase,
            "SNAKE_CASE" => NameCase::SnakeCase,
            "KEBAB_CASE" => NameCase::KebabCase,
            "PASCAL_CASE" => NameCase::PascalCase,
            "SCREAMING_SNAKE_CASE" => NameCase::ScreamingSnakeCase,
            "SCREAMING_KEBAB_CASE" => NameCase::ScreamingKebabCase,
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

    Ok(case)
}

fn translate_field_behavior_opt(
    val: baker_pkg_pb::option::Value,
    opt: &str,
) -> io::Result<FieldBehavior> {
    let case = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "OUTPUT_ONLY" => FieldBehavior::OutputOnly,
            "INPUT_ONLY" => FieldBehavior::InputOnly,
            "IMMUTABLE" => FieldBehavior::Immutable,
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

    Ok(case)
}

fn translate_opt_value_to_str(val: baker_pkg_pb::option::Value) -> io::Result<String> {
    match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::StringValue(val) => Ok(val),
        v => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "Invalud value for option where string was expected: {:?}",
                v
            ),
        )),
    }
}

fn main() -> io::Result<()> {
    baker_layer_pb::execute_flow(layer_impl)
}
