include!(concat!(env!("OUT_DIR"), "/baker.api.rs"));

pub const FIELD_BEHAVIOR_FIELD_OPTION: &str = "baker.api.field_behavior";
pub const FIELD_NAME_FIELD_OPTION: &str = "baker.api.field_name";
pub const FIELD_ALIAS_FIELD_OPTION: &str = "baker.api.field_alias";

pub const FIELD_NAME_CASE_MSG_OPTION: &str = "baker.api.field_name_case";
pub const ONEOF_FIELD_NAME_CASE_ONEOF_OPTION: &str = "baker.api.oneof_field_name_case";

pub const VALUE_NAME_ENUM_OPTION: &str = "baker.api.value_name";
pub const VALUE_ALIAS_ENUM_OPTION: &str = "baker.api.value_alias";
pub const VALUE_NAME_CASE_ENUM_OPTION: &str = "baker.api.value_name_case";

pub const DEFAULT_HTTP_BODY_ENCODING_SERVICE_OPTION: &str = "baker.api.default_http_body_encoding";

pub const HTTP_PATH_METHOD_OPTION: &str = "baker.api.http_path";
pub const HTTP_METHOD_METHOD_OPTION: &str = "baker.api.http_method";
pub const HTTP_BODY_ENCODING_METHOD_OPTION: &str = "baker.api.http_body_enconding";

impl NameCase {
    // Default value for fields.
    pub const fn fields_default() -> Self {
        Self::CamelCase
    }

    // Default value for oneof fields.
    pub const fn oneof_fields_default() -> Self {
        Self::CamelCase
    }

    // Default value for enum values.
    pub const fn values_default() -> Self {
        Self::ScreamingSnakeCase
    }
}

#[derive(Debug, Clone)]
pub struct HttpPath {
    pub segments: Vec<HttpPathSegment>,
    pub verb: Option<String>,
}

impl HttpPath {
    pub fn from_path_str(mut path: &str) -> Self {
        let verb = if let Some((p, v)) = path.rsplit_once(':') {
            if v.chars().all(|c| c.is_alphanumeric()) {
                path = p;
                Some(v.to_string())
            } else {
                None
            }
        } else {
            None
        };

        let segments = path.trim_matches('/').split('/').map(|s| {
            if s.starts_with('{') && s.ends_with('}') {
                let s = s.trim_end_matches('}').trim_start_matches('{');

                if let Some((var, pattern)) = s.split_once('=') {
                    HttpPathSegment::Var {
                        name: var.to_string(),
                        pattern: Some(pattern.to_string()),
                    }
                } else {
                    HttpPathSegment::Var {
                        name: s.to_string(),
                        pattern: None,
                    }
                }
            } else {
                HttpPathSegment::Static(s.to_string())
            }
        });

        Self {
            segments: segments.collect(),
            verb,
        }
    }
}

#[derive(Debug, Clone)]
pub enum HttpPathSegment {
    Static(String),
    Var {
        name: String,
        pattern: Option<String>,
    },
}
