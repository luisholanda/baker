use std::{collections::HashMap, io};

use baker_ir_pb::{r#type::Fundamental, Type};
use baker_orm_pb::*;
use baker_pkg_pb::{
    r#type::{BuiltIn, Value},
    Message, PackageGraph,
};

pub struct MsgModel {
    pub(crate) table_name: String,
    pub(crate) table_path: String,
    pub(crate) primary_key: Vec<String>,
    pub(crate) field_columns: HashMap<String, String>,
    pub(crate) field_types: HashMap<String, Type>,
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
                        };

                        field_types.insert(f.name.clone(), typ);
                    }
                    Some(Value::Custom(c)) => {
                        dbg!(c);
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
        }

        Ok(Some(Self {
            table_name,
            table_path,
            primary_key,
            field_columns,
            field_types,
        }))
    }
}
