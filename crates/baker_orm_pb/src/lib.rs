include!(concat!(env!("OUT_DIR"), "/baker.orm.rs"));

pub const DATABASE_TYPE_ENUM_OPTION: &str = "baker.orm.database_type";
pub const DATABASE_TYPE_PATH_ENUM_OPTION: &str = "baker.orm.database_type_path";
pub const DATABASE_ENUM_VALUE_CASE_ENUM_OPTION: &str = "baker.orm.database_enum_value_case";
pub const DATABASE_USE_INT_VALUES_ENUM_OPTION: &str = "baker.orm.database.use_int_values";

pub const COLUMN_NAME_FIELD_OPTION: &str = "baker.orm.column_name";
pub const TABLE_NAME_MSG_OPTION: &str = "baker.orm.table_name";
pub const TABLE_PATH_MSG_OPTION: &str = "baker.orm.table_path";
pub const PRIMARY_KEY_MSG_OPTION: &str = "baker.orm.primary_key";

pub const RELATIONSHIP_FIELD_OPTION: &str = "baker.orm.relationship";
pub const RELATED_TYPE_FIELD_OPTION: &str = "baker.orm.related_type";
