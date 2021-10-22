use std::{collections::HashMap, io};

use baker_ir_pb::{
    statement::{assignment::AssignmentType, Assignment},
    type_def::Constraint,
    Attribute, FunctionCall, IdentifierPath, IrFile, Namespace, Type, TypeDef, Value,
};
use baker_layer_pb::{LayerRequest, LayerResponse};
use baker_pkg_pb::{Enum, File, Message, PackageGraph};

use crate::model::{EnumModel, MsgModel};

mod enums;
mod messages;
mod model;

// TODO: Handle enums
// TODO: handle relationships
// TODO: Handle relationships

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
            .get(&msg_id)
            .cloned()
            .expect("missing message declaration");

        generate_message(msg, &graph, &mut root)?;
    }

    for en_id in file.enums {
        let enum_ = graph
            .enums
            .remove(&en_id)
            .expect("missing enum declaration");

        generate_enum(enum_, &mut root)?;
    }

    Ok(IrFile {
        file_id: file.id,
        root: Some(root),
    })
}

fn generate_message(mut msg: Message, pkg: &PackageGraph, ns: &mut Namespace) -> io::Result<()> {
    let mut msg_def = TypeDef {
        header: Some(Type::with_name(&msg.name)),
        attributes: vec![derive_call(&["diesel.Queryable", "diesel.QueryableByName"])],
        ..Default::default()
    };

    if let Some(model) = MsgModel::from_msg(&mut msg, pkg)? {
        self::messages::handle_schema_opts(&mut msg_def, &mut msg, &model)?;

        ns.types.push(msg_def);
    }

    Ok(())
}

fn generate_enum(mut enum_: Enum, ns: &mut Namespace) -> io::Result<()> {
    if let Some(model) = EnumModel::from_opts(&mut enum_)? {
        let def = self::enums::generate_database_enum(&enum_, &model);
        ns.types.push(def);
    }

    Ok(())
}

pub(crate) fn db_generic() -> (Type, Constraint) {
    let db = Type::with_name("__DB");
    let db_backend_constraint = Constraint {
        constrained: Some(db.clone()),
        interfaces: vec![Type::with_global_name("diesel.backend.Backend")],
        lifetimes: vec![],
    };

    (db, db_backend_constraint)
}

pub(crate) fn translate_opt_value_to_str(val: baker_pkg_pb::option::Value) -> io::Result<String> {
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

pub(crate) fn assignment_attr(identifier: &str, value: Value) -> Attribute {
    use baker_ir_pb::attribute::Value as AttrValue;
    Attribute {
        value: Some(AttrValue::Assignment(Assignment {
            ident: Some(IdentifierPath::from_dotted_path(identifier)),
            assignment_type: AssignmentType::Reassignment as i32,
            value: Some(value),
            ..Default::default()
        })),
    }
}

pub(crate) fn function_call_attr(
    function: String,
    args: Vec<Value>,
    kwargs: HashMap<String, Value>,
) -> Attribute {
    Attribute {
        value: Some(baker_ir_pb::attribute::Value::Call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path(&function)),
            args,
            kwargs,
            ..Default::default()
        })),
    }
}

pub(crate) fn derive_call(traits: &[&str]) -> Attribute {
    use baker_ir_pb::attribute::Value as AttrValue;
    Attribute {
        value: Some(AttrValue::Call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("derive")),
            args: traits
                .iter()
                .map(|t| IdentifierPath::from_dotted_path(&t).global())
                .map(Value::identifier)
                .collect(),
            ..Default::default()
        })),
    }
}

pub(crate) fn doc_hidden() -> Attribute {
    function_call_attr(
        "doc".to_string(),
        vec![Value::identifier(IdentifierPath::from_dotted_path(
            "hidden",
        ))],
        Default::default(),
    )
}

fn main() -> io::Result<()> {
    baker_layer_pb::execute_flow(layer_impl)
}
