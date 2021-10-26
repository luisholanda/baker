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
    pub fn self_() -> Self {
        Self::from_dotted_path("self")
    }

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

    pub fn child(&self, seg: self::identifier_path::Segment) -> Self {
        let mut child = self.clone();
        child.segments.push(seg);
        child
    }
}

impl Type {
    pub const SELF: Self = Self {
        name: Some(self::r#type::Name::Fundamental(
            self::r#type::Fundamental::Self_ as i32,
        )),
        lifetimes: vec![],
        generics: vec![],
    };

    pub fn with_path(path: IdentifierPath) -> Self {
        Self {
            name: Some(self::r#type::Name::Identifier(Box::new(path))),
            ..Default::default()
        }
    }

    pub fn with_name(name: &str) -> Self {
        Self::with_path(IdentifierPath::from_dotted_path(name))
    }

    pub fn with_global_name(name: &str) -> Self {
        Self::with_path(IdentifierPath::from_dotted_path(name).global())
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

    pub fn identifier(&self) -> Option<&IdentifierPath> {
        use self::r#type::Name;

        if let Some(Name::Identifier(p)) = &self.name {
            Some(p)
        } else {
            None
        }
    }

    pub fn set_generics(mut self, generics: Vec<Self>) -> Self {
        self.generics = generics;
        self
    }

    pub fn set_generic(mut self, generic: Self) -> Self {
        self.generics.push(generic);
        self
    }

    pub fn as_generic_of(self, wrapper: Self) -> Self {
        wrapper.set_generic(self)
    }
}

impl Value {
    pub const TRUE: Self = Self {
        value: Some(self::value::Value::BoolValue(true)),
        by_ref: self::value::ByRef::ByValue as i32,
    };

    pub const FALSE: Self = Self {
        value: Some(self::value::Value::BoolValue(true)),
        by_ref: self::value::ByRef::ByValue as i32,
    };

    pub fn boolean(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    pub fn string(value: String) -> Self {
        Self {
            value: Some(self::value::Value::StringValue(value)),
            ..Default::default()
        }
    }

    pub fn bytes(value: String) -> Self {
        Self {
            value: Some(self::value::Value::BytesValue(value.into_bytes())),
            ..Default::default()
        }
    }

    pub fn identifier(identifier: IdentifierPath) -> Self {
        Self {
            value: Some(self::value::Value::Identifier(identifier)),
            ..Default::default()
        }
    }

    pub fn func_call(call: FunctionCall) -> Self {
        Self {
            value: Some(self::value::Value::Call(call)),
            ..Default::default()
        }
    }

    pub fn tuple(values: Vec<Self>) -> Self {
        Self {
            value: Some(self::value::Value::Tuple(self::value::Tuple { values })),
            ..Default::default()
        }
    }

    pub fn cast_as(self, ty: Type) -> Self {
        Self {
            value: Some(self::value::Value::Cast(Box::new(self::value::Cast {
                value: Some(Box::new(self)),
                cast_as: Some(ty),
            }))),
            ..Default::default()
        }
    }

    pub fn operate(self, op: self::value::bin_op::Op, right: Self) -> Self {
        Self {
            value: Some(self::value::Value::BinOp(Box::new(self::value::BinOp {
                left: Some(Box::new(self)),
                operator: op as i32,
                right: Some(Box::new(right)),
            }))),
            ..Default::default()
        }
    }

    pub fn unary_operate(self, op: self::value::unary_op::Op) -> Self {
        Self {
            value: Some(self::value::Value::UnOp(Box::new(self::value::UnaryOp {
                value: Some(Box::new(self)),
                operator: op as i32,
            }))),
            ..Default::default()
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
            ..Default::default()
        }
    }

    pub fn const_ref(mut self) -> Self {
        self.set_by_ref(self::value::ByRef::ConstRef);
        self
    }

    pub fn mut_ref(mut self) -> Self {
        self.set_by_ref(self::value::ByRef::MutRef);
        self
    }

    pub fn await_(self) -> Self {
        Self {
            value: Some(self::value::Value::Await(Box::new(self))),
            ..Default::default()
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
