use std::io;

use baker_api_pb::{
    HttpBodyEncoding, HttpMethod, HttpPath, DEFAULT_HTTP_BODY_ENCODING_SERVICE_OPTION,
    HTTP_BODY_ENCODING_METHOD_OPTION, HTTP_METHOD_METHOD_OPTION, HTTP_PATH_METHOD_OPTION,
};
use baker_pkg_pb::{service::Rpc, Service};
use heck::SnakeCase;

pub struct ServiceModel {
    pub(crate) default_encoding: Option<HttpBodyEncoding>,
}

impl ServiceModel {
    pub fn from_service(src: &mut Service) -> io::Result<Self> {
        let default_encoding = if let Some(val) = src
            .options
            .remove(DEFAULT_HTTP_BODY_ENCODING_SERVICE_OPTION)
        {
            Some(translate_http_body_encoding(
                val,
                DEFAULT_HTTP_BODY_ENCODING_SERVICE_OPTION,
            )?)
        } else {
            None
        };

        Ok(Self { default_encoding })
    }
}

pub struct MethodModel {
    pub(crate) handler_name: String,
    pub(crate) http_path: HttpPath,
    pub(crate) http_method: HttpMethod,
    pub(crate) http_body_encoding: Option<HttpBodyEncoding>,
    pub(crate) path_used_fields: Vec<String>,
}

impl MethodModel {
    pub fn from_method(rpc: &mut Rpc, srv_model: &ServiceModel) -> io::Result<Option<Self>> {
        let http_path = if let Some(val) = rpc.options.remove(HTTP_PATH_METHOD_OPTION) {
            let p = translate_opt_value_to_str(val)?;

            HttpPath::from_path_str(&p)
        } else {
            return Ok(None);
        };

        let enc = if let Some(val) = rpc.options.remove(HTTP_BODY_ENCODING_METHOD_OPTION) {
            Some(translate_http_body_encoding(
                val,
                DEFAULT_HTTP_BODY_ENCODING_SERVICE_OPTION,
            )?)
        } else {
            srv_model.default_encoding
        };

        let method = if let Some(val) = rpc.options.remove(HTTP_METHOD_METHOD_OPTION) {
            translate_http_method(val, HTTP_METHOD_METHOD_OPTION)?
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("no HTTP method defined for {}", rpc.name),
            ));
        };

        let path_used_fields = http_path
            .segments
            .iter()
            .filter_map(|s| {
                if let baker_api_pb::HttpPathSegment::Var { name, .. } = s {
                    Some(name.to_string())
                } else {
                    None
                }
            })
            .collect();

        Ok(Some(Self {
            handler_name: rpc.name.to_snake_case(),
            http_path,
            http_method: method,
            http_body_encoding: enc,
            path_used_fields,
        }))
    }

    pub fn actix_route_path(&self) -> baker_ir_pb::Value {
        let mut path = String::new();

        for seg in &self.http_path.segments {
            path.push('/');
            match seg {
                baker_api_pb::HttpPathSegment::Static(s) => path.push_str(&s),
                baker_api_pb::HttpPathSegment::Var { name, pattern } => {
                    path.push('{');
                    path.push_str(&name);

                    if let Some(p) = pattern {
                        path.push(':');
                        path.push_str(&p);
                    }
                    path.push('}');
                }
            }
        }

        baker_ir_pb::Value::string(path)
    }
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

fn translate_http_body_encoding(
    val: baker_pkg_pb::option::Value,
    opt: &str,
) -> io::Result<HttpBodyEncoding> {
    let enc = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "UNKNOWN_ENCODING" => HttpBodyEncoding::UnknownEncoding,
            "JSON" => HttpBodyEncoding::Json,
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

    Ok(enc)
}

fn translate_http_method(val: baker_pkg_pb::option::Value, opt: &str) -> io::Result<HttpMethod> {
    let method = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "UNKNOWN_METHOD" => HttpMethod::UnknownMethod,
            "GET" => HttpMethod::Get,
            "POST" => HttpMethod::Post,
            "PATCH" => HttpMethod::Patch,
            "PUT" => HttpMethod::Put,
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

    Ok(method)
}
