include!(concat!(env!("OUT_DIR"), "/baker.api.rs"));

pub const FIELD_BEHAVIOR_FIELD_OPTION: &str = "baker.api.field_behavior";
pub const FIELD_NAME_FIELD_OPTION: &str = "baker.api.field_name";
pub const FIELD_ALIAS_FIELD_OPTION: &str = "baker.api.field_alias";

pub const FIELD_NAME_CASE_MSG_OPTION: &str = "baker.api.field_name_case";
pub const ONEOF_FIELD_NAME_CASE_ONEOF_OPTION: &str = "baker.api.oneof_field_name_case";

pub const VALUE_NAME_ENUM_OPTION: &str = "baker.api.value_name";
pub const VALUE_ALIAS_ENUM_OPTION: &str = "baker.api.value_alias";
pub const VALUE_NAME_CASE_ENUM_OPTION: &str = "baker.api.value_name_case";

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
