use std::fmt;

include!(concat!(env!("OUT_DIR"), "/baker.ir.v1.rs"));

impl fmt::Display for self::identifier_path::Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if self.generics.is_empty() && self.lifetimes.is_empty() {
            return Ok(());
        }

        write!(f, "<")?;

        for lf in &self.lifetimes {
            write!(f, "'{},", lf)?;
        }

        for ty in &self.generics {
            write!(f, "{},", ty)?;
        }

        write!(f, ">")?;
        Ok(())
    }
}

impl fmt::Display for IdentifierPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(qualifier) = &self.qualifier {
            write!(f, "<{} as ", qualifier)?;

            if let Some((last, init)) = self.segments.split_last() {
                for seg in init {
                    write!(f, "::{}", seg)?;
                }

                write!(f, ">::{}", last)?;
            }
        } else {
            for seg in &self.segments {
                write!(f, "::{}", seg)?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::r#type::{Fundamental, Name};

        match &self.name {
            Some(Name::Identifier(ident)) => write!(f, "{}", ident)?,
            // TODO: improve this
            Some(Name::Fundamental(fund)) => {
                write!(f, "{:?}", Fundamental::from_i32(*fund).unwrap())?
            }
            None => return Ok(()),
        }

        write!(f, "<")?;

        for lf in &self.lifetimes {
            write!(f, "'{},", lf)?;
        }

        for ty in &self.generics {
            write!(f, "{},", ty)?;
        }

        write!(f, ">")?;

        Ok(())
    }
}

impl IdentifierPath {
    pub fn from_dotted_path(name: &str) -> Self {
        Self {
            segments: name
                .split('.')
                .map(|seg| self::identifier_path::Segment {
                    name: seg.to_string(),
                    ..Default::default()
                })
                .collect(),
            ..Default::default()
        }
    }

    pub fn scope_as_dotted_path(&self) -> String {
        use fmt::Write;

        let mut path = String::new();
        if let Some((_, init)) = self.segments.split_last() {
            for seg in init {
                write!(&mut path, "{}.", seg).unwrap();
            }
        }

        path.pop();

        path
    }

    pub fn global(mut self) -> Self {
        self.set_scope(self::identifier_path::Scope::Global);
        self
    }

    pub fn package(mut self) -> Self {
        self.set_scope(self::identifier_path::Scope::Package);
        self
    }

    pub fn last(&self) -> &self::identifier_path::Segment {
        self.segments.last().unwrap()
    }
}

impl Type {
    pub fn with_path(path: IdentifierPath) -> Self {
        Self {
            name: Some(self::r#type::Name::Identifier(Box::new(path))),
            ..Default::default()
        }
    }

    pub fn with_name(name: &str) -> Self {
        Self::with_path(IdentifierPath::from_dotted_path(name))
    }

    pub fn with_name_and_scope(scope: &str, name: String) -> Self {
        let mut path = IdentifierPath::from_dotted_path(scope);
        path.segments.push(self::identifier_path::Segment {
            name,
            ..Default::default()
        });

        Self {
            name: Some(self::r#type::Name::Identifier(Box::new(path))),
            ..Default::default()
        }
    }

    pub fn with_fundamental(fund: self::r#type::Fundamental) -> Self {
        Self {
            name: Some(self::r#type::Name::Fundamental(fund as i32)),
            ..Default::default()
        }
    }

    pub fn fundamental(&self) -> self::r#type::Fundamental {
        use self::r#type::Name;

        if let Some(Name::Fundamental(f)) = &self.name {
            self::r#type::Fundamental::from_i32(*f).unwrap()
        } else {
            self::r#type::Fundamental::Unknown
        }
    }
}

impl Value {
    pub fn string(value: String) -> Self {
        Self {
            value: Some(self::value::Value::StringValue(value)),
        }
    }

    pub fn identifier(identifier: IdentifierPath) -> Self {
        Self {
            value: Some(self::value::Value::Identifier(identifier)),
        }
    }

    pub fn func_call(call: FunctionCall) -> Self {
        Self {
            value: Some(self::value::Value::Call(call)),
        }
    }

    pub fn tuple(values: Vec<Self>) -> Self {
        Self {
            value: Some(self::value::Value::Tuple(self::value::Tuple { values })),
        }
    }

    pub fn cast_as(self, ty: Type) -> Self {
        Self {
            value: Some(self::value::Value::Cast(Box::new(self::value::Cast {
                value: Some(Box::new(self)),
                cast_as: Some(ty),
            }))),
        }
    }

    pub fn with_method_call(self, call: FunctionCall) -> Self {
        Self {
            value: Some(self::value::Value::Method(Box::new(
                self::value::MethodCall {
                    receiver: Some(Box::new(self)),
                    method: Some(call),
                },
            ))),
        }
    }
}

impl self::identifier_path::Segment {
    pub fn with_name(name: String) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }
}

impl Statement {
    pub fn assignment(assignment: self::statement::Assignment) -> Self {
        Self {
            statement: Some(self::statement::Statement::Assignment(assignment)),
        }
    }

    pub fn switch(match_: self::statement::Match) -> Self {
        Self {
            statement: Some(self::statement::Statement::Switch(match_)),
        }
    }
}
