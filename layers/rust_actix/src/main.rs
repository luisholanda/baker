//! # Actix-web Layer
//!
//! This binary implements a layer that generated an actix-web ready interface
//! for defined services.
//!
//! For each service, we generate a trait, named after the service, that
//! contains methods `fn(&self, req: Req, http_req: HttpRequest) -> Result<Res,
//! Error>`, where `Req` is the RPC method request type and `Res` the
//! response's. The trait also contains a `configure` method that can be used
//! to mount the service and the respective HTTP routes.
//!
//! It also generate a namespace named after the service, that contains
//! actix-web function handlers for each RPC method. These handlers are
//! responsible for converting path/query/body parameters into the single
//! request object passed to the trait method.
use std::io;

use baker_api_pb::HttpMethod;
use baker_ir_pb::{
    function::Argument,
    interface::Bounds,
    r#type::Fundamental,
    statement::{assignment::AssignmentType, Assignment},
    type_def::Constraint,
    Attribute, Block, Function, FunctionCall, IdentifierPath, Import, Interface, IrFile, Namespace,
    Pattern, Statement, Type, Value,
};
use baker_layer_pb::{LayerRequest, LayerResponse};
use baker_pkg_pb::{service::Rpc, File, PackageGraph, Service};
use heck::SnakeCase;

use crate::model::{MethodModel, ServiceModel};

mod model;

fn layer_impl(req: LayerRequest) -> io::Result<LayerResponse> {
    let mut pkg = req.packages.expect("missing packages graph");

    let mut resp = LayerResponse::default();

    for file_id in std::mem::take(&mut pkg.main_files) {
        let file = pkg
            .files
            .remove(&file_id)
            .expect("main file not found in graph");

        resp.ir_files.push(generate_file(file, &pkg)?);
    }

    Ok(resp)
}

fn generate_file(file: File, pkg: &PackageGraph) -> io::Result<IrFile> {
    let mut ns = Namespace::default();

    for mut srv in file.services {
        let model = ServiceModel::from_service(&mut srv)?;

        let (handlers_ns, models) = generate_service_method_handlers(&mut srv, &model, pkg)?;
        generate_service_trait(&mut srv, pkg, models, &mut ns);

        ns.nested_namespaces
            .insert(srv.basename().to_snake_case(), handlers_ns);
    }

    Ok(IrFile {
        file_id: file.id,
        root: Some(ns),
    })
}

fn generate_service_trait(
    service: &mut Service,
    pkg: &PackageGraph,
    models: Vec<MethodModel>,
    ns: &mut Namespace,
) {
    let http_req_ty = Type::with_global_name("actix_web.HttpRequest");
    let configure = generate_service_trait_configure_method(&service, models);

    let methods = service.methods.iter_mut().map(|rpc| {
        let req = pkg.messages.get(&rpc.request).unwrap();
        let res = pkg.messages.get(&rpc.response).unwrap();

        let req_ty = Type::with_name(&req.name);
        let res_ty = Type::with_global_name("std.result.Result").set_generics(vec![
            Type::with_name(&res.name),
            Type::with_global_name("actix_web.error.Error"),
        ]);

        Function {
            header: Some(Type::with_name(&rpc.name.to_snake_case())),
            receiver: Some(Type::with_fundamental(Fundamental::ShrdRef)),
            r#return: Some(res_ty),
            arguments: vec![
                Argument::new("req", req_ty),
                Argument::new("http_req", http_req_ty.clone()),
            ],
            asyncness: true,
            documentation: rpc.documentation().to_string(),
            ..Default::default()
        }
    });

    let service_trait = Interface {
        header: Some(Type::with_name(&service.name)),
        methods: methods.chain(std::iter::once(configure)).collect(),
        documentation: std::mem::take(&mut service.documentation).unwrap_or_default(),
        attributes: vec![Attribute {
            value: Some(baker_ir_pb::attribute::Value::Call(FunctionCall {
                function: Some(IdentifierPath::global_path("async_trait.async_trait")),
                args: vec![Value {
                    value: Some(baker_ir_pb::value::Value::Raw("?Send".to_string())),
                    ..Default::default()
                }],
                ..Default::default()
            })),
        }],
        bounds: Some(Bounds {
            interfaces: vec![
                Type::with_global_name("std.marker.Send"),
                Type::with_global_name("std.marker.Sync"),
            ],
            lifetimes: vec!["static".to_string()],
        }),
        ..Default::default()
    };

    ns.interfaces.push(service_trait);
}

fn generate_service_trait_configure_method(
    service: &Service,
    models: Vec<MethodModel>,
) -> Function {
    fn web_route_from_http_method(method: HttpMethod) -> IdentifierPath {
        let path = match method {
            HttpMethod::UnknownMethod => "actix_web.web.route",
            HttpMethod::Get => "actix_web.web.get",
            HttpMethod::Post => "actix_web.web.post",
            HttpMethod::Patch => "actix_web.web.patch",
            HttpMethod::Put => "actix_web.web.put",
        };

        IdentifierPath::from_dotted_path(path)
    }

    let service_ident = IdentifierPath::from_dotted_path("service");
    let mut cfg_val = Value::identifier(IdentifierPath::from_dotted_path("cfg"));

    cfg_val = cfg_val.with_method_call(FunctionCall {
        function: Some(IdentifierPath::from_dotted_path("app_data")),
        args: vec![Value::identifier(service_ident.clone())],
        ..Default::default()
    });

    for model in models {
        let handler_name = format!("{}.{}", service.name, model.handler_name);
        let handler = Value::identifier(IdentifierPath::from_dotted_path(&handler_name));
        let route = Value::func_call(FunctionCall {
            function: Some(web_route_from_http_method(model.http_method)),
            ..Default::default()
        });

        let route = route.with_method_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("to")),
            args: vec![handler],
            ..Default::default()
        });

        cfg_val = cfg_val.with_method_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path("route")),
            args: vec![model.actix_route_path(), route],
            ..Default::default()
        });
    }

    let decl_arc_service = Statement::assignment(Assignment {
        ident: Some(service_ident.clone()),
        value: Some(Value::identifier(IdentifierPath::self_())),
        r#type: Some(
            Type::with_name(&service.name)
                .as_generic_of(Type::with_fundamental(Fundamental::Dynamic))
                .as_generic_of(Type::with_global_name("std.sync.Arc")),
        ),
        assignment_type: AssignmentType::DefConstant as i32,
        ..Default::default()
    });
    let decl_service = Statement::assignment(Assignment {
        ident: Some(service_ident.clone()),
        value: Some(
            Value::identifier(service_ident).with_method_call(FunctionCall {
                function: Some(IdentifierPath::from_dotted_path("into")),
                ..Default::default()
            }),
        ),
        r#type: Some(
            Type::with_name(&service.name)
                .as_generic_of(Type::with_fundamental(Fundamental::Dynamic))
                .as_generic_of(Type::with_global_name("actix_web.web.Data")),
        ),
        assignment_type: AssignmentType::DefConstant as i32,
        ..Default::default()
    });

    Function {
        header: Some(Type::with_name("configure")),
        receiver: Some(Type::with_global_name("std.sync.Arc").set_generic(Type::SELF)),
        arguments: vec![Argument::new(
            "cfg",
            Type::with_global_name("actix_web.web.ServiceConfig").as_uniq_ref(None),
        )],
        implementation: Some(Block {
            statements: vec![
                decl_arc_service,
                decl_service,
                Statement {
                    statement: Some(baker_ir_pb::statement::Statement::Expression(cfg_val)),
                },
            ],
            ..Default::default()
        }),
        constraints: vec![Constraint {
            constrained: Some(Type::SELF),
            interfaces: vec![Type::with_global_name("std.marker.Sized")],
            ..Default::default()
        }],
        ..Default::default()
    }
}

fn generate_service_method_handlers(
    service: &mut Service,
    srv_model: &ServiceModel,
    pkg: &PackageGraph,
) -> io::Result<(Namespace, Vec<MethodModel>)> {
    let mut ns = Namespace::default();

    ns.imports.push(Import {
        module: Some(IdentifierPath::global_path("actix_web.FromRequest")),
        alias: Some("__FromRequest".into()),
        ..Default::default()
    });

    let mut methods = std::mem::take(&mut service.methods);
    let mut models = Vec::with_capacity(methods.len());

    for rpc in &mut methods {
        if let Some(model) = MethodModel::from_method(rpc, srv_model)? {
            ns.functions
                .push(generate_handler_method(service, rpc, &model, pkg));
            models.push(model);
        }
    }

    service.methods = methods;

    Ok((ns, models))
}

fn generate_handler_method(
    service: &Service,
    rpc: &Rpc,
    model: &MethodModel,
    pkg: &PackageGraph,
) -> Function {
    let header = Type::with_name(&rpc.name.to_snake_case());

    let req = pkg.messages.get(&rpc.request).unwrap();
    let res = pkg.messages.get(&rpc.response).unwrap();

    let payload_ty_wrapper_name = if model.http_method == HttpMethod::Get {
        "actix_web.web.Query"
    } else {
        "actix_web.web.Json"
    };

    let payload_ty =
        Type::with_global_name(payload_ty_wrapper_name).set_generic(Type::with_name(&req.name));

    let dyn_service =
        Type::with_fundamental(Fundamental::Dynamic).set_generic(Type::with_name(&service.name));

    let http_req = Value::identifier(IdentifierPath::from_dotted_path("http_req"));

    let mut statements = vec![];

    // Declare request
    let req_ident = IdentifierPath::from_dotted_path("request");
    statements.push(Statement::assignment(Assignment {
        ident: Some(req_ident.clone()),
        assignment_type: if model.path_used_fields.is_empty() {
            AssignmentType::DefConstant
        } else {
            AssignmentType::DefMutable
        } as i32,
        value: Some(
            Value::identifier(IdentifierPath::from_dotted_path("payload")).with_method_call(
                FunctionCall {
                    function: Some(IdentifierPath::from_dotted_path("into_inner")),
                    ..Default::default()
                },
            ),
        ),
        ..Default::default()
    }));

    // Apply path fields
    if !model.path_used_fields.is_empty() {
        let path_fields_paths: Vec<_> = model
            .path_used_fields
            .iter()
            .map(|f| IdentifierPath::from_dotted_path(f))
            .collect();

        let path_fields_idents: Vec<_> = path_fields_paths
            .iter()
            .map(|i| i.last().name.clone())
            .map(|i| IdentifierPath::from_dotted_path(&i))
            .map(Value::identifier)
            .collect();

        let path_field_tuple = Value::tuple(path_fields_idents.clone());

        let path_extract = Value::func_call(FunctionCall {
            function: Some(IdentifierPath::global_path("actix_web.web.Path.extract")),
            args: vec![http_req.clone().const_ref()],
            ..Default::default()
        });

        let path_match = Value::func_call(FunctionCall {
            function: Some(IdentifierPath::global_path("actix_web.web.Path")),
            args: vec![path_field_tuple],
            ..Default::default()
        });

        // Extract fields from path.
        statements.push(Statement::assignment(Assignment {
            ident: None,
            assignment_type: AssignmentType::DefConstant as i32,
            value: Some(
                path_extract
                    .await_()
                    .unary_operate(baker_ir_pb::value::unary_op::Op::Try),
            ),
            pattern_decl: Some(Pattern {
                value: Some(baker_ir_pb::pattern::Value::Constant(path_match)),
            }),
            ..Default::default()
        }));

        for (field_path, field_ident) in path_fields_paths.into_iter().zip(path_fields_idents) {
            statements.push(Statement::assignment(Assignment {
                ident: Some(req_ident.clone()),
                field: Some(field_path.clone()),
                value: Some(field_ident),
                assignment_type: AssignmentType::Reassignment as i32,
                ..Default::default()
            }));
        }
    }

    // Call method and declare response
    let resp_ident = IdentifierPath::from_dotted_path("response");

    let service = Value::identifier(IdentifierPath::from_dotted_path("service"));
    let response_value = service
        .with_method_call(FunctionCall {
            function: Some(IdentifierPath::from_dotted_path(&rpc.name.to_snake_case())),
            args: vec![Value::identifier(req_ident), http_req],
            ..Default::default()
        })
        .await_()
        .unary_operate(baker_ir_pb::value::unary_op::Op::Try);

    statements.push(Statement::assignment(Assignment {
        ident: Some(resp_ident.clone()),
        assignment_type: AssignmentType::DefConstant as i32,
        value: Some(response_value),
        ..Default::default()
    }));

    let resp_val = Value::func_call(FunctionCall {
        function: Some(IdentifierPath::global_path("actix_web.web.Json")),
        args: vec![Value::identifier(resp_ident)],
        ..Default::default()
    });
    let ret_val = Value::func_call(FunctionCall {
        function: Some(IdentifierPath::global_path("std.result.Result.Ok")),
        args: vec![resp_val],
        ..Default::default()
    });

    Function {
        header: Some(header),
        r#return: Some(
            Type::with_global_name("std.result.Result").set_generics(vec![
                Type::with_global_name("actix_web.web.Json")
                    .set_generic(Type::with_name(&res.name)),
                Type::with_global_name("actix_web.Error"),
            ]),
        ),
        arguments: vec![
            Argument::new(
                "service",
                Type::with_global_name("actix_web.web.Data").set_generic(dyn_service),
            ),
            Argument::new("payload", payload_ty),
            Argument::new("http_req", Type::with_global_name("actix_web.HttpRequest")),
        ],
        implementation: Some(Block {
            statements,
            return_value: Some(ret_val),
        }),
        asyncness: true,
        documentation: rpc.documentation().to_string(),
        ..Default::default()
    }
}

fn main() -> io::Result<()> {
    baker_layer_pb::execute_flow(layer_impl)
}
