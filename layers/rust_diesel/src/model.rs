use std::{collections::HashMap, io};

use baker_api_pb::NameCase;
use baker_ir_pb::{r#type::Fundamental, Type};
use baker_orm_pb::*;
use baker_pkg_pb::{
    r#type::{BuiltIn, Value},
    Enum, Message, PackageGraph,
};

pub struct MsgModel {
    pub(crate) table_name: String,
    pub(crate) table_path: String,
    pub(crate) primary_key: Vec<String>,
    pub(crate) field_columns: HashMap<String, String>,
    pub(crate) field_types: HashMap<String, Type>,
    pub(crate) field_parents: HashMap<String, String>,
}

impl MsgModel {
    pub fn from_msg(msg: &mut Message, pkg: &PackageGraph) -> io::Result<Option<Self>> {
        let table_name = if let Some(table_name) = msg.options.remove(TABLE_NAME_MSG_OPTION) {
            crate::translate_opt_value_to_str(table_name)?
        } else {
            return Ok(None);
        };

        let table_path = if let Some(table_path) = msg.options.remove(TABLE_PATH_MSG_OPTION) {
            crate::translate_opt_value_to_str(table_path)?
        } else {
            table_name.clone()
        };

        let primary_key = msg
            .options
            .remove(PRIMARY_KEY_MSG_OPTION)
            .map(|pk| crate::translate_opt_value_to_str(pk))
            .transpose()?
            .map(|pk| pk.split(',').map(|s| s.trim().to_string()).collect())
            .unwrap_or_else(|| vec!["id".to_string()]);

        let mut field_columns = HashMap::with_capacity(msg.fields.len());
        let mut field_types = HashMap::with_capacity(msg.fields.len());
        let mut field_parents = HashMap::new();

        let fields = msg
            .fields
            .iter_mut()
            .chain(msg.oneofs.iter_mut().flat_map(|o| o.fields.iter_mut()));

        for f in fields {
            let column_name = if let Some(val) = f.options.remove(COLUMN_NAME_FIELD_OPTION) {
                crate::translate_opt_value_to_str(val)?
            } else {
                match &f.r#type.as_ref().unwrap().value {
                    Some(Value::Bultin(b)) => {
                        let typ = match BuiltIn::from_i32(*b).unwrap() {
                            BuiltIn::Unknown => unreachable!(),
                            BuiltIn::Double => Type::with_fundamental(Fundamental::Double),
                            BuiltIn::Float => Type::with_fundamental(Fundamental::Float),
                            BuiltIn::SInt => Type::with_fundamental(Fundamental::SInt),
                            BuiltIn::SLong => Type::with_fundamental(Fundamental::SLong),
                            BuiltIn::UInt => Type::with_fundamental(Fundamental::UInt),
                            BuiltIn::ULong => Type::with_fundamental(Fundamental::ULong),
                            BuiltIn::Bool => Type::with_fundamental(Fundamental::Bool),
                            BuiltIn::String => Type::with_fundamental(Fundamental::String),
                            BuiltIn::Bytes => Type::with_fundamental(Fundamental::Bytes),
                            BuiltIn::Duration => {
                                panic!("diesel doesn't support Duration values directly")
                            }
                            BuiltIn::Timestamp => Type::with_global_name("chrono.DateTime")
                                .set_generic(Type::with_global_name("chrono.Utc")),
                            BuiltIn::Unit => panic!("diesel doesn't support empty values"),
                        };

                        field_types.insert(f.name.clone(), typ);
                    }
                    Some(Value::Custom(c)) => {
                        let typ = pkg
                            .enums
                            .get(c)
                            .map(|t| &*t.name)
                            .or_else(|| pkg.messages.get(c).map(|c| &*c.name))
                            .map(Type::with_name)
                            .unwrap();

                        field_types.insert(f.name.clone(), typ);
                        continue;
                    }
                    None => continue,
                }

                f.name.clone()
            };

            field_columns.insert(f.name.clone(), column_name);

            if let Some(rel) = f.options.remove(RELATIONSHIP_FIELD_OPTION) {
                let rel = translate_relationship_type_opt(rel, RELATIONSHIP_FIELD_OPTION)?;

                if rel == RelationshipType::ParentId {
                    if let Some(par) = f.options.remove(RELATED_TYPE_FIELD_OPTION) {
                        field_parents
                            .insert(f.name.clone(), crate::translate_opt_value_to_str(par)?);
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("Field {} missing parent type", f.name),
                        ));
                    }
                }
            }
        }

        Ok(Some(Self {
            table_name,
            table_path,
            primary_key,
            field_columns,
            field_types,
            field_parents,
        }))
    }
}

pub struct EnumModel {
    pub(crate) database_type: String,
    pub(crate) database_type_path: Option<String>,
    pub(crate) name_case: NameCase,
}

impl EnumModel {
    pub fn from_opts(enum_: &mut Enum) -> io::Result<Option<Self>> {
        let database_type = if let Some(val) = enum_.options.remove(DATABASE_TYPE_ENUM_OPTION) {
            crate::translate_opt_value_to_str(val)?
        } else {
            return Ok(None);
        };

        let database_type_path =
            if let Some(val) = enum_.options.remove(DATABASE_TYPE_PATH_ENUM_OPTION) {
                Some(crate::translate_opt_value_to_str(val)?)
            } else {
                None
            };

        let case = if let Some(val) = enum_.options.remove(DATABASE_ENUM_VALUE_CASE_ENUM_OPTION) {
            translate_name_case_opt(val, DATABASE_ENUM_VALUE_CASE_ENUM_OPTION)?
        } else {
            NameCase::ScreamingSnakeCase
        };

        Ok(Some(Self {
            database_type,
            database_type_path,
            name_case: case,
        }))
    }

    pub(crate) fn case_fn(&self) -> fn(&str) -> String {
        match self.name_case {
            NameCase::Unspecified => |s| s.to_string(),
            NameCase::CamelCase => heck::MixedCase::to_mixed_case,
            NameCase::SnakeCase => heck::SnakeCase::to_snake_case,
            NameCase::KebabCase => heck::KebabCase::to_kebab_case,
            NameCase::PascalCase => heck::CamelCase::to_camel_case,
            NameCase::ScreamingSnakeCase => heck::ShoutySnakeCase::to_shouty_snake_case,
            NameCase::ScreamingKebabCase => heck::ShoutyKebabCase::to_shouty_kebab_case,
        }
    }
}

fn translate_name_case_opt(val: baker_pkg_pb::option::Value, opt: &str) -> io::Result<NameCase> {
    let case = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "CAMEL_CASE" => NameCase::CamelCase,
            "SNAKE_CASE" => NameCase::SnakeCase,
            "KEBAB_CASE" => NameCase::KebabCase,
            "PASCAL_CASE" => NameCase::PascalCase,
            "SCREAMING_SNAKE_CASE" => NameCase::ScreamingSnakeCase,
            "SCREAMING_KEBAB_CASE" => NameCase::ScreamingKebabCase,
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

    Ok(case)
}

fn translate_relationship_type_opt(
    val: baker_pkg_pb::option::Value,
    opt: &str,
) -> io::Result<RelationshipType> {
    let case = match val.value.unwrap() {
        baker_pkg_pb::option::value::Value::IdentifierValue(ident) => match ident.as_str() {
            "RELATIONSHIP_UNSPECIFIED" => RelationshipType::RelationshipUnspecified,
            "PARENT_ID" => RelationshipType::ParentId,
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

    Ok(case)
}
