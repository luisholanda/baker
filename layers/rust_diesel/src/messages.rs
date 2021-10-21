use std::{collections::HashMap, io};

use baker_ir_pb::{
    function::Argument,
    identifier_path::{Scope, Segment},
    r#type::Fundamental,
    statement::{assignment::AssignmentType, Assignment},
    type_def::{record::Property, Constraint, Definition, ImplBlock, Record},
    Block, Function, FunctionCall, IdentifierPath, Import, Namespace, Pattern, Statement, Type,
    TypeAlias, TypeDef, Value, Visibility,
};
use baker_pkg_pb::{
    message::{Field, OneOf},
    Message,
};
use heck::CamelCase;

use crate::model::MsgModel;

pub(crate) fn handle_schema_opts(
    msg_def: &mut TypeDef,
    msg: &mut Message,
    model: &MsgModel,
) -> io::Result<()> {
    let mut assoc_ns = Namespace::default();
    handle_table_attributes(msg_def, model, &mut assoc_ns)?;

    let mut properties = HashMap::with_capacity(msg.fields.len());
    for f in msg.fields.drain(..) {
        if let Some(col_name) = model.field_columns.get(&f.name) {
            properties.insert(
                f.name,
                Property {
                    attributes: vec![crate::assignment_attr(
                        "column_name",
                        Value::string(col_name.clone()),
                    )],
                    ..Default::default()
                },
            );
        }
    }

    for mut oneof in msg.oneofs.drain(..) {
        handle_oneof_schema_traits(&mut oneof, &msg.name, &model, &mut assoc_ns)?;

        properties.insert(
            oneof.name,
            Property {
                attributes: vec![crate::function_call_attr(
                    "diesel".to_string(),
                    vec![Value::identifier(IdentifierPath::from_dotted_path("embed"))],
                    Default::default(),
                )],
                ..Default::default()
            },
        );
    }

    msg_def.definition = Some(Definition::Record(Record { properties }));
    msg_def.associated_namespace = Some(assoc_ns);

    Ok(())
}

fn handle_table_attributes(
    msg_def: &mut TypeDef,
    model: &MsgModel,
    ns: &mut Namespace,
) -> io::Result<()> {
    // Ensure the schema is in scope so that diesel's derives can find it.
    let name = format!("__table_{}", model.table_name);
    ns.imports.push(Import {
        module: Some(IdentifierPath::from_dotted_path(&model.table_path)),
        alias: Some(name.clone()),
        ..Default::default()
    });

    msg_def.attributes.extend([
        crate::derive_call(&["diesel.Insertable", "diesel.Identifiable"]),
        crate::assignment_attr("table_name", Value::string(name)),
    ]);

    let args = model
        .primary_key
        .iter()
        .map(|pk| Value::identifier(IdentifierPath::from_dotted_path(pk)))
        .collect();

    msg_def.attributes.push(crate::function_call_attr(
        "primary_key".to_string(),
        args,
        Default::default(),
    ));

    Ok(())
}

fn handle_oneof_schema_traits(
    oneof: &mut OneOf,
    msg_name: &str,
    model: &MsgModel,
    ns: &mut Namespace,
) -> io::Result<()> {
    let table_ty = Type::with_name_and_scope(&model.table_path, "table".to_string());

    let name = oneof.name.to_camel_case();
    let scope = format!("{}.{}", msg_name, name);
    let oneof_ty = Type::with_name_and_scope(msg_name, oneof.name.to_camel_case());

    let field_names: Vec<_> = oneof
        .fields
        .iter()
        .map(|f| {
            let name = format!("{}.{}", scope, f.name.to_camel_case());
            IdentifierPath::from_dotted_path(&name)
        })
        .collect();

    let type_to_eq = |typ: &Type, col: &str| Type {
        generics: vec![
            Type::with_name_and_scope(&model.table_path, col.to_string()),
            typ.clone(),
        ],
        ..Type::with_path(IdentifierPath::from_dotted_path("diesel.helper_types.Eq").global())
    };

    let field_tys = fields_to_type(&oneof.fields, model, type_to_eq);
    let ref_field_tys = fields_to_type(&oneof.fields, model, |typ, col| {
        type_to_eq(
            &Type {
                generics: vec![typ.clone()],
                lifetimes: vec!["insert".to_string()],
                ..Type::with_fundamental(Fundamental::ShrdRef)
            },
            col,
        )
    });

    let (alias, insertable) = generate_insertable_block(
        &oneof,
        &model,
        &field_names,
        field_tys,
        &table_ty,
        vec![],
        &format!("__{}_Values", oneof.name),
    );
    let (ref_alias, ref_insertable) = generate_insertable_block(
        &oneof,
        &model,
        &field_names,
        ref_field_tys,
        &table_ty,
        vec!["insert".into()],
        &format!("__{}_ref_Values", oneof.name),
    );

    let (row_alias, queryable) = generate_queryable_block(&oneof, &model);

    ns.types.push(TypeDef {
        header: Some(oneof_ty.clone()),
        blocks: vec![insertable, queryable],
        associated_namespace: Some(Namespace {
            aliases: vec![alias, ref_alias, row_alias],
            types: vec![TypeDef {
                header: Some(Type {
                    generics: vec![oneof_ty],
                    lifetimes: vec!["insert".to_string()],
                    ..Type::with_fundamental(Fundamental::ShrdRef)
                }),
                blocks: vec![ref_insertable],
                ..Default::default()
            }],
            imports: vec![Import {
                module: Some(IdentifierPath::from_dotted_path("diesel.prelude").global()),
                glob: true,
                ..Default::default()
            }],
            ..Default::default()
        }),
        ..Default::default()
    });

    Ok(())
}

/// Generate a `diesel::Insertable` impl block for a given oneof enum.
///
/// The generated implementation uses a `Option` n-tuple to represent
/// every possible case in the oneof, in the end only one of them would
/// be `Some`, which may be inefficient, but guarantee that the implementation
/// compiles and is simpler to understand.
fn generate_insertable_block(
    oneof: &OneOf,
    model: &MsgModel,
    field_enum_names: &[IdentifierPath],
    field_tps: Vec<Type>,
    table_ty: &Type,
    lifetime: Vec<String>,
    alias_name: &str,
) -> (TypeAlias, ImplBlock) {
    let some = IdentifierPath::from_dotted_path("std.option.Option.Some").global();
    let n_fields = field_tps.len();
    let values_qualifier = fields_to_options_tuple(field_tps);

    let qualifier_alias = Type {
        lifetimes: lifetime.clone(),
        ..Type::with_name(alias_name)
    };

    let alias_def = TypeAlias {
        alias: Some(qualifier_alias.clone()),
        aliased: Some(values_qualifier),
        visibility: Visibility::Public as i32,
        attributes: vec![crate::doc_hidden()],
        ..Default::default()
    };

    let values_assoc_type_ident = IdentifierPath {
        qualifier: Some(Box::new(qualifier_alias.clone())),
        segments: vec![
            Segment::with_name("diesel".into()),
            Segment {
                generics: vec![table_ty.clone()],
                ..Segment::with_name("Insertable".into())
            },
            Segment::with_name("Values".into()),
        ],
        scope: Scope::Global as i32,
    };

    let values_assoc_type = Type::with_path(values_assoc_type_ident);

    let recv_var = Value::identifier(IdentifierPath::from_dotted_path("x"));
    let patterns = field_enum_names.iter().map(|n| Pattern {
        value: Some(baker_ir_pb::pattern::Value::Sum(FunctionCall {
            function: Some(n.clone()),
            args: vec![recv_var.clone()],
            ..Default::default()
        })),
    });

    let values_var = IdentifierPath::from_dotted_path("values");

    let arms = patterns.zip(&oneof.fields).enumerate().map(|(i, (p, f))| {
        let expr = if let Some(col) = model.field_columns.get(&f.name) {
            let path = format!("{}.{}", model.table_path, col);
            Value::identifier(IdentifierPath::from_dotted_path(&path)).with_method_call(
                FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("eq")),
                    args: vec![recv_var.clone()],
                    ..Default::default()
                },
            )
        } else {
            recv_var.clone()
        };

        let case_tuple = n_tuple_value_with_a_some(
            n_fields,
            i,
            Value::func_call(FunctionCall {
                function: Some(some.clone()),
                args: vec![expr],
                ..Default::default()
            }),
        );

        baker_ir_pb::statement::r#match::MatchArm {
            pattern: vec![p],
            block: Some(Block {
                statements: vec![Statement {
                    statement: Some(baker_ir_pb::statement::Statement::Assignment(Assignment {
                        ident: Some(values_var.clone()),
                        assignment_type: AssignmentType::Reassignment as i32,
                        value: Some(case_tuple),
                        ..Default::default()
                    })),
                }],
                ..Default::default()
            }),
        }
    });

    let match_ = baker_ir_pb::statement::Match {
        value: Some(Value::identifier(IdentifierPath::from_dotted_path("self"))),
        arms: arms.collect(),
    };

    let block = ImplBlock {
        interface: Some(Type {
            generics: vec![table_ty.clone()],
            ..Type::with_path(IdentifierPath::from_dotted_path("diesel.Insertable").global())
        }),
        lifetimes: lifetime,
        assoc_types: vec![TypeAlias {
            alias: Some(Type::with_name("Values")),
            aliased: Some(values_assoc_type),
            ..Default::default()
        }],
        methods: vec![Function {
            header: Some(Type::with_name("values")),
            r#return: Some(Type::with_name("Self.Values")),
            receiver: Some(Type::with_fundamental(Fundamental::Self_)),
            implementation: Some(Block {
                statements: vec![
                    Statement::assignment(Assignment {
                        ident: Some(values_var.clone()),
                        assignment_type: AssignmentType::DefConstant as i32,
                        r#type: Some(qualifier_alias),
                        value: None,
                    }),
                    Statement::switch(match_),
                ],
                return_value: Some(
                    Value::identifier(values_var).with_method_call(FunctionCall {
                        function: Some(IdentifierPath::from_dotted_path("values")),
                        ..Default::default()
                    }),
                ),
            }),
            ..Default::default()
        }],
        ..Default::default()
    };

    (alias_def, block)
}

/// Generate a `diesel::Queryable` impl block for oneof enums.
///
/// The generate impl requires all the necessary columns for all cases to be
/// present in a `.nullable` way, ensuring that we can fetch every possible
/// case. In the end, if the schema is properly defined, only one of the cases
/// will be set, and we can generate the correct variant.
fn generate_queryable_block(oneof: &OneOf, model: &MsgModel) -> (TypeAlias, ImplBlock) {
    let field_tys = fields_to_type(&oneof.fields, model, |ty, _| ty.clone());
    let field_tys_tuple = fields_to_options_tuple(field_tys);

    let fields_row = Type::with_name(&format!("__{}_Queryable_Row", oneof.name));
    let field_tys_tuple_alias_def = TypeAlias {
        alias: Some(fields_row.clone()),
        aliased: Some(field_tys_tuple),
        attributes: vec![crate::doc_hidden()],
        visibility: Visibility::Public as i32,
        ..Default::default()
    };

    let st = Type::with_name("__ST");
    let db = Type::with_name("__DB");

    let queryable = Type {
        generics: vec![st.clone(), db.clone()],
        ..Type::with_path(IdentifierPath::from_dotted_path("diesel.Queryable").global())
    };

    let constraints = vec![
        Constraint {
            constrained: Some(db.clone()),
            interfaces: vec![Type::with_path(
                IdentifierPath::from_dotted_path("diesel.backend.Backend").global(),
            )],
            ..Default::default()
        },
        Constraint {
            constrained: Some(fields_row.clone()),
            interfaces: vec![queryable.clone()],
            ..Default::default()
        },
    ];

    let row_assoc_type = Type::with_path(IdentifierPath {
        qualifier: Some(Box::new(fields_row.clone())),
        segments: vec![
            Segment::with_name("diesel".into()),
            Segment {
                generics: vec![st.clone(), db.clone()],
                ..Segment::with_name("Queryable".into())
            },
            Segment::with_name("Row".into()),
        ],
        scope: Scope::Global as i32,
    });

    let row_var = IdentifierPath::from_dotted_path("row");
    let build_row = Statement::assignment(Assignment {
        ident: Some(row_var.clone()),
        assignment_type: AssignmentType::DefConstant as i32,
        r#type: Some(fields_row),
        value: Some(Value::func_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("diesel.Queryable.build").global()),
            args: vec![Value::identifier(row_var.clone())],
            ..Default::default()
        })),
    });

    let value_var = IdentifierPath::from_dotted_path("value");
    let decl_value = Statement::assignment(Assignment {
        ident: Some(value_var.clone()),
        assignment_type: AssignmentType::DefConstant as i32,
        r#type: Some(Type::with_fundamental(Fundamental::Self_)),
        ..Default::default()
    });

    let some = IdentifierPath::from_dotted_path("std.option.Option.Some");
    let match_row = Statement::switch(baker_ir_pb::statement::Match {
        value: Some(Value::identifier(row_var)),
        arms: oneof
            .fields
            .iter()
            .enumerate()
            .map(|(idx, f)| {
                let match_var = Value::identifier(IdentifierPath::from_dotted_path("x"));
                let match_value = Value::func_call(FunctionCall {
                    function: Some(some.clone()),
                    args: vec![match_var.clone()],
                    ..Default::default()
                });
                let pattern = Pattern {
                    value: Some(baker_ir_pb::pattern::Value::Constant(
                        n_tuple_value_with_a_some(oneof.fields.len(), idx, match_value),
                    )),
                };

                let variant_name = format!("Self.{}", f.name.to_camel_case());
                let set_value = Statement::assignment(Assignment {
                    ident: Some(value_var.clone()),
                    value: Some(Value::func_call(FunctionCall {
                        function: Some(IdentifierPath::from_dotted_path(&variant_name)),
                        args: vec![match_var],
                        ..Default::default()
                    })),
                    ..Default::default()
                });

                baker_ir_pb::statement::r#match::MatchArm {
                    pattern: vec![pattern],
                    block: Some(Block {
                        statements: vec![set_value],
                        ..Default::default()
                    }),
                }
            })
            .collect(),
        ..Default::default()
    });

    let build_impl = Function {
        header: Some(Type::with_name("build")),
        arguments: vec![Argument {
            name: "row".into(),
            r#type: Some(Type::with_name("Self.Row")),
            ..Default::default()
        }],
        r#return: Some(Type::with_fundamental(Fundamental::Self_)),
        visibility: Visibility::Private as i32,
        implementation: Some(Block {
            statements: vec![build_row, decl_value, match_row],
            return_value: Some(Value::identifier(value_var)),
        }),
        ..Default::default()
    };

    let block = ImplBlock {
        constraints,
        interface: Some(queryable),
        assoc_types: vec![TypeAlias {
            alias: Some(Type::with_name("Row")),
            aliased: Some(row_assoc_type),
            ..Default::default()
        }],
        generics: vec![st, db],
        methods: vec![build_impl],
        ..Default::default()
    };

    (field_tys_tuple_alias_def, block)
}

fn fields_to_type(
    fields: &[Field],
    model: &MsgModel,
    field_to_type: impl Fn(&Type, &str) -> Type,
) -> Vec<Type> {
    fields
        .iter()
        .filter_map(|f| model.field_types.get(&f.name).map(|t| (t, f)))
        .map(|(typ, f)| {
            if let Some(col) = model.field_columns.get(&f.name) {
                field_to_type(typ, &col)
            } else {
                model.field_types.get(&f.name).unwrap().clone()
            }
        })
        .collect()
}

fn fields_to_options_tuple(field_tps: Vec<Type>) -> Type {
    Type {
        generics: field_tps
            .into_iter()
            .map(|tp| Type {
                generics: vec![tp],
                ..Type::with_fundamental(Fundamental::Optional)
            })
            .collect(),
        ..Type::with_fundamental(Fundamental::Tuple)
    }
}

fn n_none_values(elems: usize) -> Vec<Value> {
    let none = IdentifierPath::from_dotted_path("std.option.Option.None").global();

    (0..elems)
        .map(|_| Value::identifier(none.clone()))
        .collect()
}

fn n_tuple_value_with_a_some(elems: usize, some_idx: usize, value: Value) -> Value {
    let mut nones = n_none_values(elems);

    nones[some_idx] = value;

    Value::tuple(nones)
}