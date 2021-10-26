use std::collections::HashMap;

use baker_ir_pb::{
    function::Argument,
    r#type::Fundamental,
    statement::{r#match::MatchArm, Assignment, Match},
    type_def::{Constraint, Definition, ImplBlock},
    Attribute, Block, Function, FunctionCall, IdentifierPath, Import, Namespace, Pattern,
    Statement, Type, TypeAlias, TypeDef, Value,
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

    let enum_ty = Type::with_name(&enum_.name);
    let mut blocks = vec![];

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

    ns.imports.push(Import {
        module: Some(IdentifierPath::from_dotted_path("std.io.Write")),
        alias: Some("__Write".to_string()),
        ..Default::default()
    });

    TypeDef {
        header: Some(enum_ty),
        associated_namespace: Some(ns),
        blocks,
        attributes: vec![
            crate::derive_call(&["diesel.AsExpression", "diesel.FromSqlRow"], true),
            Attribute {
                value: Some(baker_ir_pb::attribute::Value::Assignment(Assignment {
                    ident: Some(IdentifierPath::from_dotted_path("sql_type")),
                    value: database_ty
                        .identifier()
                        .map(|i| Value::string(i.last().to_string())),
                    ..Default::default()
                })),
            },
        ],
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
                crate::derive_call(&["diesel.SqlType", "diesel.QueryId"], true),
                crate::derive_call(&["Clone", "Copy"], false),
                crate::function_call_attr("postgres".to_string(), vec![], {
                    let mut map = HashMap::with_capacity(2);

                    map.insert("type_name".to_string(), Value::string(basename.to_string()));

                    if let Some(s) = schema {
                        map.insert("type_schema".to_string(), Value::string(s.to_string()));
                    }

                    map
                }),
            ],
            ..Default::default()
        },
    )
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
                    function: Some(
                        IdentifierPath::from_dotted_path("std.result.Result.Err").global(),
                    ),
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
                function: Some(IdentifierPath::from_dotted_path("std.result.Result.Ok").global()),
                args: vec![Value::identifier(
                    IdentifierPath::from_dotted_path("diesel.serialize.IsNull.No").global(),
                )],
                ..Default::default()
            })),
        }),
        ..to_sql_func_decl(db.clone())
    };

    ImplBlock {
        interface: Some(interface),
        constraints: vec![db_constraint],
        methods: vec![to_sql],
        generics: vec![db],
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
