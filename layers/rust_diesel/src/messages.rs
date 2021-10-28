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
use heck::{CamelCase, SnakeCase};

use crate::model::MsgModel;

pub(crate) fn handle_schema_opts(
    msg_def: &mut TypeDef,
    msg: &mut Message,
    model: &mut MsgModel,
) -> io::Result<()> {
    let mut assoc_ns = Namespace::default();
    handle_table_attributes(msg_def, model, msg, &mut assoc_ns)?;

    crate::helpers::generate_message_helpers(msg_def, msg, model, &mut assoc_ns);

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
    model: &mut MsgModel,
    msg: &Message,
    ns: &mut Namespace,
) -> io::Result<()> {
    let name = if model.table_path == model.table_name {
        model.table_name.clone()
    } else {
        // Ensure the schema is in scope so that diesel's derives can find it.
        let name = format!("{}_schema", msg.basename().to_snake_case());
        ns.imports.push(Import {
            module: Some(IdentifierPath::from_dotted_path(&model.table_path)),
            alias: Some(name.clone()),
            ..Default::default()
        });

        model.table_path = format!("{}.{}", msg.scope(), name);

        name
    };

    msg_def.attributes.extend([
        crate::derive_call(&["diesel.Insertable", "diesel.Identifiable"], true),
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

    if !model.field_parents.is_empty() {
        msg_def
            .attributes
            .push(crate::derive_call(&["diesel.Associations"], true));

        let mut methods = Vec::with_capacity(model.field_parents.len());

        for (field, parent) in &model.field_parents {
            let parent = IdentifierPath::from_dotted_path(parent);
            let mut karwgs = HashMap::default();
            karwgs.insert("foreign_key".to_string(), Value::string(field.to_string()));
            karwgs.insert("parent".to_string(), Value::identifier(parent.clone()));
            msg_def.attributes.push(crate::function_call_attr(
                "belongs_to".to_string(),
                vec![],
                karwgs,
            ));

            let field_ty = model.field_types[field].clone();
            methods.push(crate::helpers::generate_all_of_helper(
                msg, model, parent, field, field_ty,
            ));
        }

        msg_def.blocks.push(ImplBlock {
            methods,
            ..Default::default()
        });
    }

    // TODO: Handle updates when the message has a changeset.
    if msg.oneofs.is_empty() {
        msg_def
            .attributes
            .push(crate::derive_call(&["diesel.AsChangeset"], true));
    }

    Ok(())
}

fn handle_oneof_schema_traits(
    oneof: &mut OneOf,
    msg_name: &str,
    model: &MsgModel,
    ns: &mut Namespace,
) -> io::Result<()> {
    let table_ty = Type::with_name_and_scope(&model.table_path, "table".to_string());

    let oneof_ty = Type::with_name_and_scope(msg_name, oneof.name.to_camel_case());

    let type_to_eq = |typ: &Type, col: &str| {
        Type::with_global_name("diesel.helper_types.Eq").set_generics(vec![
            Type::with_name_and_scope(&model.table_path, col.to_string()),
            typ.clone(),
        ])
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

    let camel_case_name = oneof.name.to_camel_case();
    let (alias, insertable) = generate_insertable_block(
        &oneof,
        &camel_case_name,
        &model,
        field_tys,
        &table_ty,
        vec![],
        &format!("__{}Values", camel_case_name),
    );
    let (ref_alias, ref_insertable) = generate_insertable_block(
        &oneof,
        &camel_case_name,
        &model,
        ref_field_tys,
        &table_ty,
        vec!["insert".into()],
        &format!("__{}ByRefValues", camel_case_name),
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
                module: Some(IdentifierPath::global_path("diesel.prelude")),
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
    oneof_name: &str,
    model: &MsgModel,
    field_tps: Vec<Type>,
    table_ty: &Type,
    lifetime: Vec<String>,
    alias_name: &str,
) -> (TypeAlias, ImplBlock) {
    let some = IdentifierPath::global_path("std.option.Option.Some");
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
    let patterns = oneof
        .fields
        .iter()
        .map(|f| {
            let f_name = f.name.to_camel_case();
            let path = if lifetime.is_empty() {
                format!("Self.{}", f_name)
            } else {
                format!("{}.{}", oneof_name, f_name)
            };

            IdentifierPath::from_dotted_path(&path)
        })
        .map(|id| Pattern {
            value: Some(baker_ir_pb::pattern::Value::Sum(FunctionCall {
                function: Some(id),
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

        let expr = Value::func_call(FunctionCall {
            function: Some(some.clone()),
            args: vec![expr],
            ..Default::default()
        });

        baker_ir_pb::statement::r#match::MatchArm {
            pattern: vec![p],
            block: Some(Block {
                statements: vec![Statement {
                    statement: Some(baker_ir_pb::statement::Statement::Assignment(Assignment {
                        ident: Some(values_var.clone()),
                        assignment_type: AssignmentType::Reassignment as i32,
                        value: Some(expr),
                        field: Some(IdentifierPath::from_dotted_path(&format!("{}", i))),
                        ..Default::default()
                    })),
                }],
                ..Default::default()
            }),
        }
    });

    let mut match_ = baker_ir_pb::statement::Match {
        value: Some(Value::identifier(IdentifierPath::self_())),
        arms: arms.collect(),
    };
    match_.arms.push(baker_ir_pb::statement::r#match::MatchArm {
        pattern: vec![Pattern {
            value: Some(baker_ir_pb::pattern::Value::Constant(Value::identifier(
                IdentifierPath::from_dotted_path("_"),
            ))),
        }],
        block: Some(Default::default()),
    });

    let block = ImplBlock {
        interface: Some(Type::with_global_name("diesel.Insertable").set_generic(table_ty.clone())),
        lifetimes: lifetime,
        assoc_types: vec![TypeAlias {
            alias: Some(Type::with_name("Values")),
            aliased: Some(values_assoc_type),
            ..Default::default()
        }],
        methods: vec![Function {
            header: Some(Type::with_name("values")),
            r#return: Some(Type::with_name("Self.Values")),
            receiver: Some(Type::SELF),
            implementation: Some(Block {
                statements: vec![
                    Statement::assignment(Assignment {
                        ident: Some(values_var.clone()),
                        assignment_type: AssignmentType::DefMutable as i32,
                        r#type: Some(qualifier_alias),
                        value: Some(Value::func_call(FunctionCall {
                            function: Some(IdentifierPath::global_path(
                                "std.default.Default.default",
                            )),
                            ..Default::default()
                        })),
                        ..Default::default()
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

    let fields_row_name =
        IdentifierPath::from_dotted_path(&format!("__{}QueryableRow", oneof.name.to_camel_case()));
    let fields_row = Type::with_path(fields_row_name.clone());
    let field_tys_tuple_alias_def = TypeAlias {
        alias: Some(fields_row.clone()),
        aliased: Some(field_tys_tuple),
        attributes: vec![crate::doc_hidden()],
        visibility: Visibility::Public as i32,
        ..Default::default()
    };

    let st = Type::with_name("__ST");
    let db = Type::with_name("__DB");

    let queryable =
        Type::with_global_name("diesel.Queryable").set_generics(vec![st.clone(), db.clone()]);

    let constraints = vec![
        Constraint {
            constrained: Some(db.clone()),
            interfaces: vec![Type::with_path(IdentifierPath::global_path(
                "diesel.backend.Backend",
            ))],
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

    let some = IdentifierPath::from_dotted_path("std.option.Option.Some");
    let match_row = {
        let build_path = fields_row_name.child(Segment::with_name("build".to_string()));
        let build_row = Value::func_call(FunctionCall {
            function: Some(build_path),
            args: vec![Value::identifier(row_var)],
            ..Default::default()
        });

        let mut match_ = baker_ir_pb::statement::Match {
            value: Some(build_row),
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

                    baker_ir_pb::statement::r#match::MatchArm {
                        pattern: vec![pattern],
                        block: Some(Block {
                            statements: vec![],
                            return_value: Some(Value::func_call(FunctionCall {
                                function: Some(IdentifierPath::from_dotted_path(&variant_name)),
                                args: vec![match_var],
                                ..Default::default()
                            })),
                        }),
                    }
                })
                .collect(),
            ..Default::default()
        };

        match_.arms.push(baker_ir_pb::statement::r#match::MatchArm {
            pattern: vec![Pattern {
                value: Some(baker_ir_pb::pattern::Value::Constant(Value::identifier(
                    IdentifierPath::from_dotted_path("_"),
                ))),
            }],
            block: Some(Block {
                statements: vec![],
                return_value: Some(Value::func_call(FunctionCall {
                    function: Some(IdentifierPath::global_path("std.default.Default.default")),
                    ..Default::default()
                })),
            }),
        });

        Statement::switch(match_)
    };

    let build_impl = Function {
        header: Some(Type::with_name("build")),
        arguments: vec![Argument::new("row", Type::with_name("Self.Row"))],
        r#return: Some(Type::SELF),
        visibility: Visibility::Private as i32,
        implementation: Some(Block {
            statements: vec![match_row],
            return_value: None,
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
    Type::with_fundamental(Fundamental::Tuple).set_generics(
        field_tps
            .into_iter()
            .map(|tp| Type::with_fundamental(Fundamental::Optional).set_generic(tp))
            .collect(),
    )
}

fn n_none_values(elems: usize) -> Vec<Value> {
    let none = IdentifierPath::global_path("std.option.Option.None");

    (0..elems)
        .map(|_| Value::identifier(none.clone()))
        .collect()
}

fn n_tuple_value_with_a_some(elems: usize, some_idx: usize, value: Value) -> Value {
    let mut nones = n_none_values(elems);

    nones[some_idx] = value;

    Value::tuple(nones)
}
