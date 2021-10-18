use std::{
    collections::{HashMap, HashSet},
    fs, io,
    path::Path,
};

use baker_pkg_graph::PkgGraph;
use baker_pkg_pb::{
    message::{field, Field, OneOf},
    option::{value::Value as OptValueType, Value as OptValue},
    r#type as typ, Type,
};
use idgen::{Id, SequentialGenerator};
use proto_parser::ast::{
    Constant, Enum, Field as AstField, FieldLabel, FieldType, File, FullIdent, Message, Options,
};

#[derive(Debug)]
pub struct UndefinedType {
    pub location: String,
    pub typ: String,
}

pub struct PkgLoader<'i> {
    include_paths: HashSet<&'i Path>,
    entry_points: HashSet<&'i Path>,
    state: PkgLoaderState,
}

impl PkgLoader<'_> {
    pub fn new() -> Self {
        Self {
            include_paths: Default::default(),
            entry_points: Default::default(),
            state: PkgLoaderState {
                graph: PkgGraph::new(SequentialGenerator::default()),
                types_ids: Default::default(),
                undefined_names: vec![],
                file_ids: Default::default(),
                pkg_ids: Default::default(),
            },
        }
    }
}

impl<'i> PkgLoader<'i> {
    pub fn add_include_path(&mut self, include: &'i Path) {
        self.include_paths.insert(include);
    }

    pub fn add_entry_point(&mut self, entry_point: &'i Path) {
        self.entry_points.insert(entry_point);
    }
}

impl PkgLoader<'_> {
    pub fn load(&mut self) -> io::Result<()> {
        let mut content = String::new();

        // SAFETY: This block ensures `loaded_files` is dropped before `content`.
        {
            let mut loaded_files: Vec<File<'static>> = vec![];

            for entry_point in &self.entry_points {
                use io::Read;
                fs::File::open(entry_point)?.read_to_string(&mut content)?;

                let mut parsed_file = parse_content(&content)?;

                if !parsed_file.imports.is_empty() {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "we don't support imports yet!",
                    ));
                }

                self.state
                    .load_file(entry_point.display().to_string(), &mut parsed_file);

                // remove lifetime dependency with `content`, so that we can share the same
                // buffer with all files.
                //
                // SAFETY: `loaded_files` will be dropped before `content`.
                loaded_files.push(unsafe { std::mem::transmute::<_, File<'static>>(parsed_file) })
            }

            for file in loaded_files {
                self.state.process_file(file);
            }
        }

        Ok(())
    }
}

struct PkgLoaderState {
    graph: PkgGraph<SequentialGenerator>,
    types_ids: HashMap<String, Id>,
    undefined_names: Vec<UndefinedType>,
    file_ids: HashMap<String, Id>,
    pkg_ids: HashMap<String, Id>,
}

impl PkgLoaderState {
    fn load_file(&mut self, filename: String, parsed_file: &mut File) -> Id {
        let mut file = self.graph.define_file();
        let file_id = file.id;
        file.filename = filename.clone();
        self.file_ids.insert(filename, file.id);

        translate_comments(&parsed_file.comments, &mut file.documentation);
        translate_options(std::mem::take(&mut parsed_file.options), &mut file.options);

        let mut scope = Scope::new(full_ident_to_string(&parsed_file.package));
        self.load_parsed_messages(&mut scope, &mut parsed_file.messages, file_id, None);
        self.load_parsed_enums(
            &mut scope,
            std::mem::take(&mut parsed_file.enums),
            file_id,
            None,
        );

        let pkg_name = full_ident_to_string(&parsed_file.package);
        if let Some(pkg_id) = self.pkg_ids.get(&pkg_name) {
            self.graph.pkg_mut(*pkg_id).files.push(file_id);
        } else {
            let pkg = self.graph.define_pkg(pkg_name.clone());
            self.pkg_ids.insert(pkg_name, pkg.id);
        }

        file_id
    }

    fn process_file(&mut self, parsed_file: File) {
        let mut scope = Scope::new(full_ident_to_string(&parsed_file.package));
        self.process_file_messages_fields(&mut scope, parsed_file.messages);
        dbg!(&self.graph);
    }

    fn load_parsed_messages(
        &mut self,
        scope: &mut Scope,
        msgs: &mut [Message],
        file_id: Id,
        par_id: Option<Id>,
    ) {
        for parsed_msg in msgs {
            let abs_name = scope.relative_to_absolute(parsed_msg.name);
            let msg = self.graph.define_message(abs_name.clone());
            let msg_id = msg.id;
            self.types_ids.insert(abs_name, msg.id);

            translate_comments(&parsed_msg.comments, &mut msg.documentation);
            translate_options(std::mem::take(&mut parsed_msg.options), &mut msg.options);

            self.graph.file_mut(file_id).messages.push(msg_id);
            if let Some(id) = par_id {
                self.graph.message_mut(id).unwrap().messages.push(msg_id);
            } else {
                self.graph.file_mut(file_id).root_messages.push(msg_id);
            }

            scope.push_scope(parsed_msg.name);
            self.load_parsed_messages(scope, &mut parsed_msg.messages, file_id, Some(msg_id));
            self.load_parsed_enums(
                scope,
                std::mem::take(&mut parsed_msg.enums),
                file_id,
                Some(msg_id),
            );
            scope.pop_scope();
        }
    }

    fn load_parsed_enums(
        &mut self,
        scope: &Scope,
        enums: Vec<Enum>,
        file_id: Id,
        msg_id: Option<Id>,
    ) {
        for parsed_enum in enums {
            let abs_name = scope.relative_to_absolute(parsed_enum.name);
            let enum_ = self.graph.define_enum(abs_name.clone());
            let enum_id = enum_.id;
            self.types_ids.insert(abs_name, enum_id);

            translate_comments(&parsed_enum.comments, &mut enum_.documentation);
            translate_options(parsed_enum.options, &mut enum_.options);

            for val in parsed_enum.values {
                let mut enum_val = baker_pkg_pb::r#enum::Value {
                    name: val.name.to_string(),
                    value: val.value as i32,
                    ..Default::default()
                };

                translate_comments(&val.comments, &mut enum_val.documentation);
                translate_options(val.options, &mut enum_val.options);

                enum_.values.push(enum_val);
            }

            self.graph.file_mut(file_id).enums.push(enum_id);
            if let Some(id) = msg_id {
                self.graph.message_mut(id).unwrap().enums.push(enum_id);
            } else {
                self.graph.file_mut(file_id).root_enums.push(enum_id);
            }
        }
    }

    fn process_file_messages_fields(&mut self, scope: &mut Scope, messages: Vec<Message>) {
        for parsed_msg in messages {
            let abs_name = scope.relative_to_absolute(parsed_msg.name);
            scope.push_scope(&parsed_msg.name);

            self.process_file_messages_fields(scope, parsed_msg.messages);

            let &msg_id = self
                .types_ids
                .get(&abs_name)
                .expect("message being processed wasn't loaded");
            let msg = self
                .graph
                .message_mut(msg_id)
                .expect("message being processed wasn't defined");

            let mut msg = std::mem::take(msg);
            msg.fields = parsed_msg
                .fields
                .into_iter()
                .filter_map(|f| self.translate_field(&scope, f, msg_id))
                .collect();

            for parsed_oneof in parsed_msg.oneofs {
                let mut oneof = OneOf {
                    name: parsed_oneof.name.to_string(),
                    ..Default::default()
                };

                translate_comments(&parsed_oneof.comments, &mut oneof.documentation);
                translate_options(parsed_oneof.options, &mut oneof.options);

                oneof.fields = parsed_oneof
                    .fields
                    .into_iter()
                    .filter_map(|f| self.translate_field(&scope, f, msg_id))
                    .collect();
                msg.oneofs.push(oneof);
            }
            scope.pop_scope();

            *self.graph.message_mut(msg_id).unwrap() = msg;
        }
    }

    fn translate_field(
        &mut self,
        scope: &Scope,
        parsed_field: AstField,
        msg_id: Id,
    ) -> Option<Field> {
        let mut field = Field {
            name: parsed_field.name.to_string(),
            number: parsed_field.num,
            ..Default::default()
        };

        translate_comments(&parsed_field.comments, &mut field.documentation);
        translate_options(parsed_field.options, &mut field.options);

        match parsed_field.label {
            Some(FieldLabel::Optional) => field.set_label(field::Label::Optional),
            Some(FieldLabel::Repeated) => field.set_label(field::Label::Repeated),
            None => {}
        }

        if let Some(kt) = parsed_field.key_type {
            field.set_key_type(translate_builtin_type(kt));
        }

        field.r#type = Some(Type {
            value: Some(if let FieldType::Custom(ident) = parsed_field.type_ {
                let name = full_ident_to_string(&ident);
                let abs_name = scope.relative_to_absolute(&name);

                // First search in the current scope, then in the global scope, if we didn't
                // found, search in parents.
                let typ_id = if let Some(id) = self.types_ids.get(&abs_name) {
                    *id
                } else if let Some(id) = self.types_ids.get(&abs_name) {
                    *id
                } else {
                    let mut curr_scope = scope.clone();
                    let mut id = None;

                    while !curr_scope.is_root() && id.is_none() {
                        curr_scope.pop_scope();
                        let name = curr_scope.relative_to_absolute(&name);

                        id = self.types_ids.get(&name).copied();
                    }

                    if let Some(id) = id {
                        id
                    } else {
                        self.undefined_names.push(UndefinedType {
                            location: scope.relative_to_absolute(&field.name),
                            typ: name,
                        });
                        return None;
                    }
                };

                if self.graph.message(typ_id).is_some() {
                    self.graph
                        .add_msg_usage_ref(msg_id, typ_id, field.name.clone());
                }

                typ::Value::Custom(typ_id)
            } else {
                typ::Value::Bultin(translate_builtin_type(parsed_field.type_).into())
            }),
        });

        Some(field)
    }
}

fn parse_content<'a>(content: &'a str) -> io::Result<File<'a>> {
    match proto_parser::parse_file(content) {
        Ok(file) => Ok(file),
        Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err.to_string())),
    }
}

fn full_ident_to_string(ident: &FullIdent<'_>) -> String {
    ident.join(".")
}

fn translate_options(options: Options<'_>, dest: &mut HashMap<String, OptValue>) {
    for (opt_name, opt_value) in options {
        let value = OptValue {
            value: Some(match opt_value {
                Constant::BoolLit(v) => OptValueType::BooleanValue(v),
                Constant::FloatLit(v) => OptValueType::FloatValue(v),
                Constant::FullIdent(i) => OptValueType::IdentifierValue(full_ident_to_string(&i)),
                Constant::StrLit(v) => OptValueType::StringValue(v.to_string()),
                Constant::UninterpretedValue(v) => OptValueType::UninterpretedValue(v.to_string()),
            }),
        };

        dest.insert(full_ident_to_string(&opt_name), value);
    }
}

fn translate_comments(comments: &[&str], dest: &mut Option<String>) {
    if comments.is_empty() {
        *dest = None;
    } else {
        *dest = Some(comments.join("\n"));
    }
}

fn translate_builtin_type(typ: FieldType) -> typ::BuiltIn {
    match typ {
        FieldType::Bool => typ::BuiltIn::Bool,
        FieldType::Double => typ::BuiltIn::Double,
        FieldType::Float => typ::BuiltIn::Float,
        FieldType::Int32 | FieldType::SInt32 | FieldType::SFixed32 => typ::BuiltIn::SInt,
        FieldType::Int64 | FieldType::SInt64 | FieldType::SFixed64 => typ::BuiltIn::SLong,
        FieldType::UInt32 | FieldType::Fixed32 => typ::BuiltIn::UInt,
        FieldType::UInt64 | FieldType::Fixed64 => typ::BuiltIn::ULong,
        FieldType::String => typ::BuiltIn::String,
        FieldType::Bytes => typ::BuiltIn::Bytes,
        kt => unreachable!("invalid builtin type: {:?}", kt),
    }
}

#[derive(Debug, Clone)]
struct Scope {
    scope_name: String,
}

impl Scope {
    fn new(pkg: String) -> Self {
        Self { scope_name: pkg }
    }

    fn push_scope(&mut self, name: &str) {
        self.scope_name.push('.');
        self.scope_name.push_str(name);
    }

    fn pop_scope(&mut self) {
        if let Some((start, _)) = self.scope_name.rsplit_once('.') {
            let len = start.len();
            self.scope_name.truncate(len);
        }
    }

    fn relative_to_absolute(&self, relative: &str) -> String {
        format!("{}.{}", self.scope_name, relative)
    }

    fn is_root(&self) -> bool {
        !self.scope_name.contains('.')
    }
}
