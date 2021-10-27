use baker_ir_pb::{
    function::Argument, type_def::ImplBlock, Block, Function, FunctionCall, IdentifierPath, Import,
    Namespace, Type, TypeAlias, TypeDef, Value, Visibility,
};
use baker_pkg_pb::Message;
use heck::SnakeCase;

use crate::model::MsgModel;

pub fn generate_message_helpers(
    msg_def: &mut TypeDef,
    msg: &Message,
    model: &MsgModel,
    ns: &mut Namespace,
) {
    let (columns_func, columns_ty) = generate_columns_helper(msg, model, ns);
    let all_func = generate_all_helper(model, columns_ty);

    ns.imports.push(Import {
        module: Some(IdentifierPath::from_dotted_path("diesel.prelude")),
        glob: true,
        ..Default::default()
    });

    msg_def.blocks.push(ImplBlock {
        methods: vec![columns_func, all_func],
        ..Default::default()
    });
}

fn generate_all_helper(model: &MsgModel, columns_ty: Type) -> Function {
    let table_ident = IdentifierPath::from_dotted_path(&format!("{}.table", model.table_path));
    let table_ty = Type::with_path(table_ident.clone());
    let table_val = Value::identifier(table_ident);
    let ret_ty = Type::with_global_name("diesel.helper_types.Select")
        .set_generics(vec![table_ty, columns_ty.clone()]);

    let select = table_val.with_method_call(FunctionCall {
        function: Some(IdentifierPath::from_dotted_path("select")),
        args: vec![Value::func_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("Self.columns")),
            ..Default::default()
        })],
        ..Default::default()
    });

    Function {
        header: Some(Type::with_name("all")),
        r#return: Some(ret_ty),
        implementation: Some(Block {
            statements: vec![],
            return_value: Some(select),
        }),
        documentation: r"A query that does the correct selection when deserializing to [`Self`]."
            .to_string(),
        ..Default::default()
    }
}

fn generate_columns_helper(
    msg: &Message,
    model: &MsgModel,
    ns: &mut Namespace,
) -> (Function, Type) {
    let columns = msg_columns(msg, model);

    let alias_ty = Type::with_name(&format!("{}Columns", msg.basename()));
    ns.aliases.push(TypeAlias {
        alias: Some(alias_ty.clone()),
        aliased: Some(Type::tuple(columns.clone())),
        visibility: Visibility::Public as i32,
        ..Default::default()
    });

    let columns_func = Function {
        header: Some(Type::with_name("columns")),
        r#return: Some(alias_ty.clone()),
        implementation: Some(Block {
            statements: vec![],
            return_value: Some(Value::func_call(FunctionCall {
                function: Some(IdentifierPath::from_dotted_path(
                    "std.default.Default.default",
                )),
                ..Default::default()
            })),
        }),
        documentation: r"Return the columns in the order specified in the API spec.

This is useful when selecting in the table and loading to the generated struct, as diesel
requires that the order of columns match exactly as in the [`diesel.Queryable`] implementation.

As the schema defined order may be different than the spec order, this function solves the
issue by providing always the correct order.

See [`Self::all`] to a helper that already does the selection for you.
"
        .to_string(),
        ..Default::default()
    };

    (columns_func, alias_ty)
}

pub fn generate_all_of_helper(
    msg: &Message,
    model: &MsgModel,
    parent_name: IdentifierPath,
    fk_name: &str,
    field_ty: Type,
) -> Function {
    let table_ident = IdentifierPath::from_dotted_path(&format!("{}.table", model.table_path));
    let table_ty = Type::with_path(table_ident.clone());

    let col_ident = IdentifierPath::from_dotted_path(&format!("{}.{}", model.table_path, fk_name));
    let col_ty = Type::with_path(col_ident.clone());
    let col_val = Value::identifier(col_ident);

    let all_ret_ty = Type::with_global_name("diesel.dsl.Select").set_generics(vec![
        table_ty,
        Type::with_name(&format!("{}Columns", msg.basename())),
    ]);

    let pred_ty =
        Type::with_global_name("diesel.dsl.Eq").set_generics(vec![col_ty, field_ty.clone()]);

    let ret_ty =
        Type::with_global_name("diesel.dsl.Filter").set_generics(vec![all_ret_ty, pred_ty]);

    let ret_val = Value::func_call(FunctionCall {
        function: Some(IdentifierPath::from_dotted_path("Self.all")),
        ..Default::default()
    })
    .with_method_call(FunctionCall {
        function: Some(IdentifierPath::from_dotted_path("filter")),
        args: vec![col_val.with_method_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("eq")),
            args: vec![Value::identifier(IdentifierPath::from_dotted_path(fk_name))],
            ..Default::default()
        })],
        ..Default::default()
    });

    Function {
        header: Some(Type::with_name(&format!(
            "all_of_{}",
            &parent_name.last().name.to_snake_case()
        ))),
        arguments: vec![Argument {
            name: fk_name.to_string(),
            r#type: Some(field_ty),
            ..Default::default()
        }],
        r#return: Some(ret_ty),
        implementation: Some(Block {
            return_value: Some(ret_val),
            ..Default::default()
        }),
        documentation: format!(
            r" Query all the rows that belongs to a given {} FK value.",
            parent_name.last()
        ),
        ..Default::default()
    }
}

fn msg_columns(msg: &Message, model: &MsgModel) -> Vec<Type> {
    let mut columns_with_idx: Vec<_> = msg
        .fields
        .iter()
        .map(|f| {
            let idx = f.number;
            (
                idx,
                Type::with_name_and_scope(&model.table_path, model.field_columns[&f.name].clone()),
            )
        })
        .collect();

    columns_with_idx.extend(msg.oneofs.iter().map(|o| {
        let idx = o.fields.iter().map(|f| f.number).min().unwrap_or_default();
        let cols = o
            .fields
            .iter()
            .map(|f| {
                let f_ty = Type::with_name_and_scope(
                    &model.table_path,
                    model.field_columns[&f.name].clone(),
                );

                f_ty
            })
            .collect();

        (idx, Type::tuple(cols))
    }));

    columns_with_idx.sort_by_key(|c| c.0);

    columns_with_idx.into_iter().map(|c| c.1).collect()
}
