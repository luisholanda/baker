use std::collections::HashMap;

use baker_ir_pb::{
    function::Argument,
    identifier_path::{Scope, Segment},
    r#type::Fundamental,
    statement::{r#match::MatchArm, Match},
    type_def::{Constraint, Definition, ImplBlock},
    Block, Constant, Function, FunctionCall, IdentifierPath, Import, Namespace, Pattern, Statement,
    Type, TypeAlias, TypeDef, Value,
};
use baker_pkg_pb::Enum;
use heck::CamelCase;

use crate::model::EnumModel;

// TODO: how we handle different database engines?
// TODO: support int values

pub(crate) fn generate_database_enum(enum_: &Enum, model: &EnumModel) -> TypeDef {
    let mut ns = Namespace::default();
    let database_ty = if let Some(path) = &model.database_type_path {
        let alias = Type::with_name(&format!("{}SqlAlias", enum_.name));
        ns.aliases.push(TypeAlias {
            alias: Some(alias.clone()),
            aliased: Some(Type::with_name(path)),
            attributes: vec![crate::doc_hidden()],
            ..Default::default()
        });

        alias
    } else {
        let (typ, def) = generate_sql_mapping(enum_, model);
        ns.types.push(def);

        typ
    };

    let nullable_database_ty = Type {
        generics: vec![database_ty.clone()],
        ..Type::with_global_name("diesel.sql_types.Nullable")
    };

    let enum_ty = Type::with_name(&enum_.name);
    let mut blocks = vec![];

    for as_expr in [database_ty.clone(), nullable_database_ty] {
        blocks.extend(generate_as_expression(enum_ty.clone(), as_expr.clone(), vec![]).blocks);
        for lfs in [
            vec!["a".to_string()],
            vec!["a".to_string(), "b".to_string()],
        ] {
            ns.types.push(generate_as_expression(
                enum_ty.clone(),
                as_expr.clone(),
                lfs,
            ));
        }
    }

    blocks.push(generate_queryable(database_ty.clone()));
    blocks.push(generate_from_sql_row(database_ty.clone()));

    let pg_ty = Type::with_global_name("diesel.pg.Pg");

    let case_fn = model.case_fn();
    let field_paths: Vec<_> = enum_
        .values
        .iter()
        .map(|v| {
            let path = format!("Self.{}", v.name.to_camel_case());

            IdentifierPath::from_dotted_path(&path)
        })
        .collect();
    let db_variants: Vec<_> = enum_.values.iter().map(|v| case_fn(&v.name)).collect();

    blocks.push(generate_from_sql(
        database_ty.clone(),
        pg_ty,
        field_paths.clone(),
        db_variants.clone(),
    ));

    blocks.push(generate_to_sql(
        database_ty.clone(),
        field_paths,
        db_variants,
    ));

    blocks.push(generate_to_sql_nullable(database_ty));

    ns.imports.push(Import {
        module: Some(IdentifierPath::from_dotted_path("std.io.Write")),
        alias: Some("__Write".to_string()),
        ..Default::default()
    });

    TypeDef {
        header: Some(enum_ty),
        associated_namespace: Some(ns),
        blocks,
        ..Default::default()
    }
}

fn generate_sql_mapping(enum_: &Enum, model: &EnumModel) -> (Type, TypeDef) {
    let (schema, basename) = if let Some((s, b)) = model.database_type.split_once('.') {
        (Some(s), b)
    } else {
        (None, &*model.database_type)
    };

    let db_ty =
        Type::with_path(IdentifierPath::from_dotted_path(&format!("{}Sql", enum_.name)).package());

    (
        db_ty.clone(),
        TypeDef {
            header: Some(db_ty),
            definition: Some(Definition::Record(Default::default())),
            attributes: vec![
                crate::derive_call(&["diesel.SqlType", "Clone", "Copy"]),
                crate::function_call_attr("postgres".to_string(), vec![], {
                    let mut map = HashMap::with_capacity(2);

                    map.insert("type_name".to_string(), Value::string(basename.to_string()));

                    if let Some(s) = schema {
                        map.insert("type_schema".to_string(), Value::string(s.to_string()));
                    }

                    map
                }),
            ],
            blocks: vec![ImplBlock {
                interface: Some(Type::with_global_name("diesel.query_builder.QueryId")),
                assoc_types: vec![TypeAlias {
                    alias: Some(Type::with_name("QueryId")),
                    aliased: Some(Type::with_fundamental(Fundamental::Self_)),
                    ..Default::default()
                }],
                constants: vec![Constant {
                    name: "HAS_STATIC_QUERY_ID".to_string(),
                    r#type: Some(Type::with_fundamental(Fundamental::Bool)),
                    value: Some(Value::TRUE),
                    ..Default::default()
                }],
                ..Default::default()
            }],
            ..Default::default()
        },
    )
}

fn generate_as_expression(enum_ty: Type, as_expr: Type, lifetimes: Vec<String>) -> TypeDef {
    // impl<#lifetimes> AsExpression<#as_expr> for #enum_ty {
    //     type Expression = Bound<#as_expr, Self>;
    //
    //     fn as_expression(self) -> Self::Expression {
    //         Bound::new(self)
    //     }
    // }
    let bound = IdentifierPath::from_dotted_path("diesel.expression.bound.Bound").global();
    let bound_new = bound.child(Segment::with_name("new".into()));

    let interface = Type {
        generics: vec![as_expr.clone()],
        ..Type::with_global_name("diesel.expression.AsExpression")
    };

    let expression_ty = Type {
        generics: vec![as_expr, Type::with_fundamental(Fundamental::Self_)],
        ..Type::with_path(bound)
    };

    let self_ty = if lifetimes.is_empty() {
        enum_ty
    } else {
        Type {
            generics: vec![enum_ty],
            lifetimes: lifetimes.clone(),
            ..Type::with_fundamental(Fundamental::ShrdRef)
        }
    };

    let as_expression = Function {
        header: Some(Type::with_name("as_expression")),
        receiver: Some(Type::with_fundamental(Fundamental::Self_)),
        r#return: Some(Type::with_name("Self.Expression")),
        implementation: Some(Block {
            statements: vec![],
            return_value: Some(Value::func_call(FunctionCall {
                function: Some(bound_new),
                args: vec![Value::identifier(IdentifierPath::self_())],
                ..Default::default()
            })),
        }),
        ..Default::default()
    };

    TypeDef {
        header: Some(self_ty),
        blocks: vec![ImplBlock {
            interface: Some(interface),
            assoc_types: vec![TypeAlias {
                alias: Some(Type::with_name("Expression")),
                aliased: Some(expression_ty),
                ..Default::default()
            }],
            methods: vec![as_expression],
            lifetimes,
            ..Default::default()
        }],
        ..Default::default()
    }
}

fn generate_queryable(db_ty: Type) -> ImplBlock {
    // impl<__Db> Queryable<#db_ty, __Db> for #enum_ty
    // where
    //     __Db: Backend,
    //     Self: FromSqlRow<#db_ty, __Db>
    // {
    //     type Row = Self;
    //
    //     fn build(row: Self::Row) -> Self {
    //         row
    //     }
    // }

    let (db, db_backend_constraint) = crate::db_generic();

    let from_sql_row_constraint = Constraint {
        constrained: Some(Type::SELF),
        interfaces: vec![Type {
            generics: vec![db_ty.clone(), db.clone()],
            ..Type::with_global_name("diesel.deserialize.FromSqlRow")
        }],
        lifetimes: vec![],
    };

    let queryable = Type {
        generics: vec![db_ty, db.clone()],
        ..Type::with_global_name("diesel.Queryable")
    };

    let row = TypeAlias {
        alias: Some(Type::with_name("Row")),
        aliased: Some(Type::SELF),
        ..Default::default()
    };

    let build = Function {
        header: Some(Type::with_name("build")),
        arguments: vec![Argument {
            name: "row".to_string(),
            r#type: Some(Type::with_name("Self.Row")),
            ..Default::default()
        }],
        r#return: Some(Type::SELF),
        implementation: Some(Block {
            statements: vec![],
            return_value: Some(Value::identifier(IdentifierPath::from_dotted_path("row"))),
        }),
        ..Default::default()
    };

    ImplBlock {
        interface: Some(queryable),
        generics: vec![db],
        constraints: vec![db_backend_constraint, from_sql_row_constraint],
        assoc_types: vec![row],
        methods: vec![build],
        ..Default::default()
    }
}

fn generate_from_sql_row(db_ty: Type) -> ImplBlock {
    // impl<__DB> FromSqlRow<#db_ty, __DB> for #enum_ty
    // where
    //     __DB: Backend,
    //     Self: FromSql<#db_ty, __DB>
    // {
    //     fn build_from_row<R>(row: &mut R) -> deserialize:Result<Self>
    //     where
    //         R: diesel::row::Row<__DB>
    //     {
    //          FromSql::<#db_ty, __DB>::from_sql(row.take())
    //     }
    // }

    let (db, db_backend_constraint) = crate::db_generic();

    let interface = Type {
        generics: vec![db_ty.clone(), db.clone()],
        ..Type::with_global_name("diesel.deserialize.FromSqlRow")
    };

    let from_sql_constraint = Constraint {
        constrained: Some(Type::SELF),
        interfaces: vec![Type {
            generics: vec![db_ty.clone(), db.clone()],
            ..Type::with_global_name("diesel.deserialize.FromSql")
        }],
        lifetimes: vec![],
    };

    let row_gen = Type::with_name("__R");
    let row_constraint = Constraint {
        constrained: Some(row_gen.clone()),
        interfaces: vec![Type {
            generics: vec![db.clone()],
            ..Type::with_global_name("diesel.row")
        }],
        lifetimes: vec![],
    };
    let build_from_row = Function {
        header: Some(Type {
            generics: vec![row_gen.clone()],
            ..Type::with_name("build_from_row")
        }),
        arguments: vec![Argument {
            name: "row".to_string(),
            r#type: Some(Type {
                generics: vec![row_gen],
                ..Type::with_fundamental(Fundamental::UniqRef)
            }),
            ..Default::default()
        }],
        r#return: Some(Type {
            generics: vec![Type::SELF],
            ..Type::with_global_name("diesel.deserialize.Result")
        }),
        constraints: vec![row_constraint],
        implementation: Some(Block {
            statements: vec![],
            return_value: Some({
                let row_take = Value::identifier(IdentifierPath::from_dotted_path("row"))
                    .with_method_call(FunctionCall {
                        function: Some(IdentifierPath::from_dotted_path("take")),
                        ..Default::default()
                    });

                Value::func_call(FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("Self.from_sql")),
                    args: vec![row_take],
                    ..Default::default()
                })
            }),
        }),
        ..Default::default()
    };

    ImplBlock {
        interface: Some(interface),
        generics: vec![db],
        constraints: vec![db_backend_constraint, from_sql_constraint],
        methods: vec![build_from_row],
        ..Default::default()
    }
}

fn generate_from_sql(
    db_ty: Type,
    engine_ty: Type,
    field_paths: Vec<IdentifierPath>,
    db_variants: Vec<String>,
) -> ImplBlock {
    let interface =
        Type::with_global_name("diesel.deserialize.FromSql").set_generics(vec![db_ty, engine_ty]);

    let raw_not_none = Value::func_call(FunctionCall {
        function: Some(IdentifierPath::from_dotted_path("diesel.not_none").global()),
        args: vec![Value::identifier(IdentifierPath::from_dotted_path("raw"))],
        is_macro: true,
        ..Default::default()
    });

    // good arms
    let mut arms: Vec<_> = field_paths
        .into_iter()
        .zip(db_variants)
        .map(|(p, v)| {
            let pattern = Pattern {
                value: Some(baker_ir_pb::pattern::Value::Constant(Value::bytes(v))),
            };

            MatchArm {
                pattern: vec![pattern],
                block: Some(Block {
                    statements: vec![],
                    return_value: Some(Value::func_call(FunctionCall {
                        function: Some(
                            IdentifierPath::from_dotted_path("std.result.Result.Ok").global(),
                        ),
                        args: vec![Value::identifier(p)],
                        ..Default::default()
                    })),
                }),
            }
        })
        .collect();
    let v = Value::identifier(IdentifierPath::from_dotted_path("v"));
    arms.push(MatchArm {
        pattern: vec![Pattern {
            value: Some(baker_ir_pb::pattern::Value::Constant(v.clone())),
        }],
        block: Some(Block {
            statements: vec![],
            return_value: Some({
                let v_utf8_lossy = Value::func_call(FunctionCall {
                    function: Some(
                        IdentifierPath::from_dotted_path("std.string.String.from_utf8_lossy")
                            .global(),
                    ),
                    args: vec![v],
                    ..Default::default()
                });

                let format = Value::func_call(FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("format")),
                    is_macro: true,
                    args: vec![
                        Value::string("Unrecognized enum variant: '{}'".to_string()),
                        v_utf8_lossy,
                    ],
                    ..Default::default()
                });

                let err = format.with_method_call(FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("into")),
                    ..Default::default()
                });

                Value::func_call(FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("std.result.Result.Err")),
                    args: vec![err],
                    ..Default::default()
                })
            }),
        }),
    });

    let from_sql = Function {
        header: Some(Type::with_name("from_sql")),
        arguments: vec![Argument {
            name: "raw".to_string(),
            r#type: Some(
                Type::with_name("u8")
                    .as_generic_of(Type::with_fundamental(Fundamental::Slice))
                    .as_generic_of(Type::with_fundamental(Fundamental::ShrdRef))
                    .as_generic_of(Type::with_fundamental(Fundamental::Optional)),
            ),
            ..Default::default()
        }],
        r#return: Some(Type::with_global_name("diesel.deserialize.Result").set_generic(Type::SELF)),
        implementation: Some(Block {
            statements: vec![Statement::switch(Match {
                value: Some(raw_not_none),
                arms,
            })],
            return_value: None,
        }),
        ..Default::default()
    };

    ImplBlock {
        interface: Some(interface),
        methods: vec![from_sql],
        ..Default::default()
    }
}

fn generate_to_sql(
    db_ty: Type,
    field_paths: Vec<IdentifierPath>,
    db_variants: Vec<String>,
) -> ImplBlock {
    let (db, db_constraint) = crate::db_generic();

    let interface =
        Type::with_global_name("diesel.serialize.ToSql").set_generics(vec![db_ty, db.clone()]);

    let to_sql = Function {
        implementation: Some(Block {
            statements: {
                let deref_self = Value {
                    value: Some(baker_ir_pb::value::Value::UnOp(Box::new(
                        baker_ir_pb::value::UnaryOp {
                            value: Some(Box::new(Value::identifier(IdentifierPath::self_()))),
                            operator: baker_ir_pb::value::unary_op::Op::Deref as i32,
                        },
                    ))),
                    ..Default::default()
                };

                let arms = field_paths
                    .into_iter()
                    .zip(db_variants)
                    .map(|(p, db_v)| {
                        let pattern = Pattern {
                            value: Some(baker_ir_pb::pattern::Value::Identifier(p)),
                        };

                        let out = Value::identifier(IdentifierPath::from_dotted_path("out"));
                        let write_all = out.with_method_call(FunctionCall {
                            function: Some(IdentifierPath::from_dotted_path("write_all")),
                            args: vec![Value::bytes(db_v)],
                            ..Default::default()
                        });
                        let try_write_all = Value {
                            value: Some(baker_ir_pb::value::Value::UnOp(Box::new(
                                baker_ir_pb::value::UnaryOp {
                                    value: Some(Box::new(write_all)),
                                    operator: baker_ir_pb::value::unary_op::Op::Try as i32,
                                },
                            ))),
                            ..Default::default()
                        };
                        MatchArm {
                            pattern: vec![pattern],
                            block: Some(Block {
                                statements: vec![],
                                return_value: Some(try_write_all),
                            }),
                        }
                    })
                    .collect();

                vec![Statement::switch(Match {
                    value: Some(deref_self),
                    arms,
                })]
            },
            return_value: Some(Value::func_call(FunctionCall {
                function: Some(IdentifierPath::from_dotted_path("std.result.Result.Ok")),
                args: vec![Value::identifier(
                    IdentifierPath::from_dotted_path("diesel.serialize.IsNull.No").global(),
                )],
                ..Default::default()
            })),
        }),
        ..to_sql_func_decl(db)
    };

    ImplBlock {
        interface: Some(interface),
        constraints: vec![db_constraint],
        methods: vec![to_sql],
        ..Default::default()
    }
}

fn generate_to_sql_nullable(db_ty: Type) -> ImplBlock {
    let (db, db_constraints) = crate::db_generic();

    let to_sql_trait = Type::with_global_name("diesel.serialize.ToSql");
    let interface = to_sql_trait.clone().set_generics(vec![
        db_ty
            .clone()
            .as_generic_of(Type::with_global_name("diesel.sql_types.Nullable")),
        db.clone(),
    ]);

    let to_sql_db_ty = to_sql_trait.set_generics(vec![db_ty.clone(), db.clone()]);
    let self_constraint = Constraint {
        constrained: Some(Type::SELF),
        interfaces: vec![to_sql_db_ty.clone()],
        ..Default::default()
    };

    let to_sql_impl = Function {
        implementation: Some(Block {
            statements: vec![],
            return_value: Some(Value::func_call(FunctionCall {
                function: Some(IdentifierPath {
                    scope: Scope::Global as i32,
                    segments: vec![
                        Segment::with_name("diesel".into()),
                        Segment::with_name("serialize".into()),
                        Segment {
                            generics: vec![db_ty, db.clone()],
                            ..Segment::with_name("ToSql".into())
                        },
                        Segment::with_name("to_sql".into()),
                    ],
                    qualifier: Some(Box::new(Type::SELF)),
                }),
                args: vec![
                    Value::identifier(IdentifierPath::self_()),
                    Value::identifier(IdentifierPath::from_dotted_path("out")),
                ],
                ..Default::default()
            })),
        }),
        ..to_sql_func_decl(db)
    };

    ImplBlock {
        interface: Some(interface),
        constraints: vec![db_constraints, self_constraint],
        methods: vec![to_sql_impl],
        ..Default::default()
    }
}

fn to_sql_func_decl(db: Type) -> Function {
    let writer = Type::with_name("W");
    Function {
        header: Some(Type::with_name("to_sql").set_generic(writer.clone())),
        constraints: vec![Constraint {
            constrained: Some(writer.clone()),
            interfaces: vec![Type::with_global_name("std.io.Write")],
            ..Default::default()
        }],
        receiver: Some(Type::with_fundamental(Fundamental::ShrdRef)),
        arguments: vec![Argument {
            name: "out".to_string(),
            r#type: Some(
                Type::with_global_name("diesel.serialize.Output")
                    .set_generics(vec![writer, db.clone()])
                    .as_generic_of(Type::with_fundamental(Fundamental::UniqRef)),
            ),
            ..Default::default()
        }],
        r#return: Some(Type::with_global_name("diesel.serialize.Result")),
        ..Default::default()
    }
}
