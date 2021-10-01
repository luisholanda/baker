//! # Protobuf Parser
//!
//! This library provides a custom [`nom`] based protobuf parser.
//!
//! No validations other than syntatic ones are made in the result,
//! is responsibility of the caller to semantically analyse the content.
use nom::{branch, character::complete as character, combinator, multi, sequence, IResult};
use tokens::Token;

#[cfg(test)]
macro_rules! assert_output {
    ($parser: ident, $input: expr, $output: expr) => {
        assert_eq!($parser($input).unwrap().1, $output)
    };
}

#[cfg(test)]
macro_rules! test_parser {
    ($parser: ident, $($input: expr => $output: expr,)+) => {
        $(assert_output!($parser, $input, $output);)+
    }
}

pub mod ast;
pub mod lexer;
pub mod tokens;

/// Parse a file's content into a protobuf [`File`].
pub fn parse_file(input: &str) -> std::result::Result<File, nom::Err<nom::error::Error<&str>>> {
    let (s, comments) = comments(input)?;
    let (mut s, (_, package, imports, options)) = sequence::tuple((
        syntax,
        package,
        multi::many0(import),
        multi::many0(option),
    ))(s)?;

    let mut file = File {
        comments,
        package,
        imports,
        options,
        enums: vec![],
        messages: vec![],
        services: vec![],
    };

    let mut error = None;
    while !s.is_empty() {
        while let Ok((ns, m)) = message(s) {
            s = ns;
            file.messages.push(m);
        }

        while let Ok((ns, e)) = enum_(s) {
            s = ns;
            file.enums.push(e);
        }

        match service(s) {
            Ok((ns, srv)) => {
                s = ns;
                file.services.push(srv);
            }
            Err(e) => {
                if let Some(err) = error {
                    return Err(err)
                } else {
                    error = Some(e)
                }
            }
        }

        s = s.trim_end();
    }

    Ok(file)
}

use self::ast::*;
use self::lexer::*;

type Result<'a, T> = IResult<&'a str, T>;

fn comments(input: &str) -> Result<Vec<&str>> {
    if let (s, Token::Comment(comments)) = COMMENT(input)? {
        Ok((s, comments))
    } else {
        unreachable!()
    }
}

fn constant(input: &str) -> Result<Constant> {
    let (s, tok) = branch::alt((BOOL_LIT, FULL_IDENT, FLOAT_LIT, STR_LIT))(input)?;

    let constan = match tok {
        Token::FullIdent(f) => Constant::FullIdent(f),
        Token::FloatLit(f) => Constant::FloatLit(f),
        Token::StrLit(s) => Constant::StrLit(s),
        Token::BoolLit(b) => Constant::BoolLit(b),
        _ => unreachable!(),
    };

    Ok((s, constan))
}

fn syntax(input: &str) -> Result<()> {
    let line = sequence::tuple((SYNTAX, EQUALS, QUOTE, PROTO3, QUOTE, SEMI_COLON));

    combinator::value((), line)(input)
}

fn import(input: &str) -> Result<String> {
    let (s, tok) = sequence::delimited(IMPORT, STR_LIT, SEMI_COLON)(input)?;

    if let Token::StrLit(lit) = tok {
        Ok((s, lit))
    } else {
        unreachable!()
    }
}

fn package(input: &str) -> Result<Vec<&str>> {
    let (s, tok) = sequence::delimited(PACKAGE, FULL_IDENT, SEMI_COLON)(input)?;

    if let Token::FullIdent(lit) = tok {
        Ok((s, lit))
    } else {
        unreachable!()
    }
}

fn uninterpreted_block(input: &str) -> Result<&str> {
    let (start, _) = OPEN_BRACE(input)?;
    let start_len = start.len();
    let mut s = start;
    let mut depth = 1;

    loop {
        let (ns, ch) = character::anychar(s)?;
        s = ns;
        match ch {
            '{' => {
                depth += 1;
            }
            '}' => {
                depth -= 1;
                if depth == 0 {
                    let curr_len = s.len();
                    let value = &start[..start_len - curr_len - 1];

                    return Ok((s, value.trim()));
                }
            }
            _ => {}
        }
    }
}

fn option(input: &str) -> Result<(Vec<&str>, Constant)> {
    let (s, _) = comments(input)?;
    let (s, val) = sequence::tuple((OPTION, option_name, EQUALS, option_value, SEMI_COLON))(s)?;

    Ok((s, (val.1, val.3)))
}

fn option_name(input: &str) -> Result<Vec<&str>> {
    match FULL_IDENT(input) {
        Ok((input, Token::FullIdent(idents))) => Ok((input, idents)),
        _ => {
            let (s, ident) = sequence::delimited(OPEN_PAREN, FULL_IDENT, CLOSE_PAREN)(input)?;
            if let Token::FullIdent(mut start) = ident {
                let (s, idents) = combinator::opt(MESSAGE_TYPE)(s)?;

                match idents {
                    Some(Token::FullIdent(rem)) => {
                        start.extend(rem);
                        Ok((s, start))
                    }
                    None => Ok((s, start)),
                    _ => unreachable!(),
                }
            } else {
                unreachable!()
            }
        }
    }
}

fn option_value(input: &str) -> Result<Constant> {
    branch::alt((
        constant,
        combinator::map(uninterpreted_block, Constant::UninterpretedValue),
    ))(input)
}

fn field(input: &str) -> Result<Field> {
    let (s, comments) = comments(input)?;

    let (s, label) = combinator::opt(branch::alt((
        combinator::value(FieldLabel::Repeated, REPEATED),
        combinator::value(FieldLabel::Optional, OPTIONAL),
    )))(s)?;

    let (s, typ) = field_type(s)?;
    let (s, field_name) = field_name(s)?;
    let (s, field_number) = sequence::preceded(EQUALS, number)(s)?;
    let (s, options) = field_options(s)?;
    let (s, _) = SEMI_COLON(s)?;

    Ok((
        s,
        Field {
            comments,
            key_type: None,
            type_: typ,
            name: field_name,
            num: field_number,
            options,
            label,
        },
    ))
}

fn field_name(input: &str) -> Result<&str> {
    if let (s, Token::Ident(name)) = IDENT(input)? {
        Ok((s, name))
    } else {
        unreachable!()
    }
}

fn field_type(input: &str) -> Result<FieldType> {
    let (s, tok) = branch::alt((BUILTIN_TYPE, MESSAGE_TYPE))(input)?;

    let typ = match tok {
        Token::Keyword(builtin) => match builtin {
            "double" => FieldType::Double,
            "float" => FieldType::Float,
            "int32" => FieldType::Int32,
            "int64" => FieldType::Int64,
            "uint32" => FieldType::UInt32,
            "uint64" => FieldType::UInt64,
            "sint32" => FieldType::SInt32,
            "sint64" => FieldType::SInt64,
            "fixed32" => FieldType::Fixed32,
            "fixed64" => FieldType::Fixed64,
            "sfixed32" => FieldType::SFixed32,
            "sfixed64" => FieldType::SFixed64,
            "bool" => FieldType::Bool,
            "string" => FieldType::String,
            "bytes" => FieldType::Bytes,
            _ => unreachable!(),
        },
        Token::FullIdent(idents) => FieldType::Custom(idents),
        _ => unreachable!(),
    };

    Ok((s, typ))
}

fn field_options(input: &str) -> Result<Options> {
    let options = multi::separated_list1(COMMA, field_option);

    let (s, options) =
        combinator::opt(sequence::delimited(OPEN_BRACKET, options, CLOSE_BRACKET))(input)?;

    Ok((s, options.unwrap_or_default()))
}

fn field_option(input: &str) -> Result<(Vec<&str>, Constant)> {
    let (s, _) = comments(input)?;
    sequence::separated_pair(option_name, EQUALS, option_value)(s)
}

fn oneof_field(input: &str) -> Result<OneOf> {
    let (s, comments) = comments(input)?;
    let (s, name) = sequence::preceded(ONEOF, field_name)(s)?;

    let (s, options) = sequence::preceded(OPEN_BRACE, multi::many0(option))(s)?;
    let (s, fields) = multi::many1(field)(s)?;

    if fields.iter().any(|f| f.label.is_some()) {
        Err(nom::Err::Failure(nom::error::Error::new(
            "oneof fields cannot have labels",
            nom::error::ErrorKind::Fail,
        )))
    } else {
        let (s, _) = sequence::pair(CLOSE_BRACE, SEMI_COLON)(s)?;

        Ok((
            s,
            OneOf {
                comments,
                name,
                options,
                fields,
            },
        ))
    }
}

fn map_field(input: &str) -> Result<Field> {
    let (s, comments) = comments(input)?;
    let (s, (key_type, type_)) = sequence::delimited(
        sequence::pair(MAP, LESS_THAN),
        sequence::separated_pair(field_type, COMMA, field_type),
        GREATER_THAN,
    )(s)?;

    if matches!(key_type, FieldType::Bytes | FieldType::Custom(_)) {
        return Err(nom::Err::Failure(nom::error::Error::new(
            "invalid key type",
            nom::error::ErrorKind::IsNot,
        )));
    }

    let (s, (name, num)) = sequence::separated_pair(field_name, EQUALS, number)(s)?;

    let (s, options) = field_options(s)?;

    let (s, _) = SEMI_COLON(s)?;

    Ok((
        s,
        Field {
            comments,
            key_type: Some(key_type),
            type_,
            name,
            num,
            options,
            label: None,
        },
    ))
}

fn enum_(input: &str) -> Result<Enum> {
    let (s, comments) = comments(input)?;
    let (s, name) = sequence::delimited(ENUM, field_name, OPEN_BRACE)(s)?;
    let (s, options) = multi::many0(option)(s)?;
    let (s, values) = sequence::terminated(multi::many0(enum_field), CLOSE_BRACE)(s)?;

    Ok((
        s,
        Enum {
            comments,
            name,
            options,
            values,
        },
    ))
}

fn enum_field(input: &str) -> Result<EnumValue> {
    let (s, comments) = comments(input)?;
    let (s, (name, value)) = sequence::separated_pair(field_name, EQUALS, character::i16)(s)?;
    let (s, options) = sequence::terminated(field_options, SEMI_COLON)(s)?;

    Ok((
        s,
        EnumValue {
            comments,
            name,
            value,
            options,
        },
    ))
}

fn message(input: &str) -> Result<Message> {
    #[derive(Debug)]
    enum MessageEvent<'i> {
        Field(Field<'i>),
        Enum(Enum<'i>),
        Message(Message<'i>),
        OneOf(OneOf<'i>),
    }

    let (s, comments) = comments(input)?;
    let (s, name) = sequence::delimited(MESSAGE, field_name, OPEN_BRACE)(s)?;
    let (s, options) = multi::many0(option)(s)?;

    let (s, parts) = multi::many1(branch::alt((
        combinator::map(enum_, MessageEvent::Enum),
        combinator::map(oneof_field, MessageEvent::OneOf),
        combinator::map(message, MessageEvent::Message),
        combinator::map(field, MessageEvent::Field),
        combinator::map(map_field, MessageEvent::Field),
    )))(s)?;

    let (s, _) = CLOSE_BRACE(s)?;

    let mut message = Message {
        name,
        comments,
        options,
        fields: vec![],
        oneofs: vec![],
        enums: vec![],
        messages: vec![],
    };

    for part in parts {
        match part {
            MessageEvent::Enum(en) => message.enums.push(en),
            MessageEvent::Field(field) => message.fields.push(field),
            MessageEvent::Message(msg) => message.messages.push(msg),
            MessageEvent::OneOf(on) => message.oneofs.push(on),
        }
    }

    Ok((s, message))
}

fn rpc(input: &str) -> Result<Rpc> {
    let (s, comments) = comments(input)?;
    let (s, name) = sequence::preceded(RPC, field_name)(s)?;
    let (s, request) = sequence::delimited(OPEN_PAREN, field_type, CLOSE_PAREN)(s)?;
    let (s, response) =
        sequence::delimited(sequence::pair(RETURNS, OPEN_PAREN), field_type, CLOSE_PAREN)(s)?;

    let (mut s, options) = combinator::opt(sequence::delimited(
        OPEN_BRACE,
        multi::many0(option),
        CLOSE_BRACE,
    ))(s)?;

    let options = if let Some(options) = options {
        options
    } else {
        s = SEMI_COLON(s)?.0;
        vec![]
    };

    Ok((
        s,
        Rpc {
            comments,
            name,
            request,
            response,
            options,
        },
    ))
}

fn service(input: &str) -> Result<Service> {
    let (s, comments) = comments(input)?;
    let (s, name) = sequence::preceded(SERVICE, field_name)(s)?;
    let (s, (_, options, rpcs, _)) = sequence::tuple((
        OPEN_BRACE,
        multi::many0(option),
        multi::many0(rpc),
        CLOSE_BRACE,
    ))(s)?;

    Ok((
        s,
        Service {
            comments,
            name,
            rpcs,
            options,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant() {
        test_parser! {
            constant,
            "abc123.abca" => Constant::FullIdent(vec!["abc123", "abca"]),
            "1" => Constant::FloatLit(1.0),
            "-1.234E5" => Constant::FloatLit(-1.234E5),
            "true" => Constant::BoolLit(true),
            r#""bla\r\t""# => Constant::StrLit("bla\r\t".to_string()),
        }
    }

    #[test]
    fn test_syntax() {
        test_parser! {
            syntax,
            "syntax = 'proto3';" => (),
            "syntax = \"proto3\";" => (),
        }
    }

    #[test]
    fn test_import() {
        test_parser! {
            import,
            "import \"bla\";" => "bla".to_string(),
            "import \'bla\';" => "bla".to_string(),
        }
    }

    #[test]
    fn test_package() {
        test_parser! {
            package,
            "package foo.bar.v1;" => vec!["foo", "bar", "v1"],
        }
    }

    #[test]
    fn test_uninterpreted_block() {
        test_parser! {
            uninterpreted_block,
            "{}" => "",
            "{ foo }" => "foo",
            "{ foo { bla } }" => "foo { bla }",
        }
    }

    #[test]
    fn test_option_name() {
        test_parser! {
            option_name,
            "for" => vec!["for"],
            "for.bla.bar" => vec!["for", "bla", "bar"],
            "(for).bla.bar" => vec!["for", "bla", "bar"],
            "(for.bla).bar" => vec!["for", "bla", "bar"],
        }
    }

    #[test]
    fn test_option() {
        test_parser! {
            option,
            "option for = 1;" => (vec!["for"], Constant::FloatLit(1.0)),
            "option for.bla.bar = 1.2;" => (vec!["for", "bla", "bar"], Constant::FloatLit(1.2)),
            "option (for).bla.bar = true;" => (vec!["for", "bla", "bar"], Constant::BoolLit(true)),
            "option (for.bla).bar = 'bla';" => (vec!["for", "bla", "bar"], Constant::StrLit("bla".to_string())),
            "option (foo.bla) = { bar: '/bar' };" => (vec!["foo", "bla"], Constant::UninterpretedValue("bar: '/bar'")),
        }
    }

    #[test]
    fn test_field() {
        test_parser! {
            field,
            "int32 bla = 1;" => Field {
                comments: vec![],
                key_type: None,
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "foo.Bar bla = 1;" => Field {
                comments: vec![],
                key_type: None,
                type_: FieldType::Custom(vec!["foo", "Bar"]),
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "// bla\nint32 bla = 1;" => Field {
                comments: vec!["bla"],
                key_type: None,
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "repeated int32 bla = 1;" => Field {
                comments: vec![],
                key_type: None,
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: Some(FieldLabel::Repeated)
            },
            "optional int32 bla = 1;" => Field {
                comments: vec![],
                key_type: None,
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: Some(FieldLabel::Optional)
            },
            "int32 bla = 1 [packed=true];" => Field {
                comments: vec![],
                key_type: None,
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![(vec!["packed"], Constant::BoolLit(true))],
                label: None
            },
        }
    }

    #[test]
    fn test_field_option() {
        test_parser! {
            field_option,
            "packed = true" => (vec!["packed"], Constant::BoolLit(true)),
            "(for).bla = 1.2" => (vec!["for", "bla"], Constant::FloatLit(1.2)),
            "(for).bla = { t: 1.2 }" => (vec!["for", "bla"], Constant::UninterpretedValue("t: 1.2")),
        }
    }

    #[test]
    fn test_field_options() {
        test_parser! {
            field_options,
            "" => vec![],
            "[packed = true]" => vec![(vec!["packed"], Constant::BoolLit(true))],
            "[(for).bla = 1.2]" => vec![(vec!["for", "bla"], Constant::FloatLit(1.2))],
            "[packed = true, (for).bla = 1.2]" => vec![
                (vec!["packed"], Constant::BoolLit(true)),
                (vec!["for", "bla"], Constant::FloatLit(1.2))
            ],
        }
    }

    #[test]
    fn test_oneof() {
        test_parser! {
            oneof_field,
            "// bla\noneof bla {
                option bla = true;
                bool field = 1;
            };" => OneOf {
                comments: vec!["bla"],
                name: "bla",
                options: vec![(vec!["bla"], Constant::BoolLit(true))],
                fields: vec![Field {
                    comments: vec![],
                    key_type: None,
                    type_: FieldType::Bool,
                    name: "field",
                    num: 1,
                    options: vec![],
                    label: None
                }]
            },
        }
    }

    #[test]
    fn test_map_field() {
        test_parser! {
            map_field,
            "map<bool, int32> bla = 1;" => Field {
                comments: vec![],
                key_type: Some(FieldType::Bool),
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "map<string, foo.Bar> bla = 1;" => Field {
                comments: vec![],
                key_type: Some(FieldType::String),
                type_: FieldType::Custom(vec!["foo", "Bar"]),
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "// bla\nmap<int32, int32> bla = 1;" => Field {
                comments: vec!["bla"],
                key_type: Some(FieldType::Int32),
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![],
                label: None
            },
            "map<string,int32> bla = 1 [packed=true];" => Field {
                comments: vec![],
                key_type: Some(FieldType::String),
                type_: FieldType::Int32,
                name: "bla",
                num: 1,
                options: vec![(vec!["packed"], Constant::BoolLit(true))],
                label: None
            },
        }
    }

    #[test]
    fn test_enum() {
        test_parser! {
            enum_,
            "enum bla { FOO = 1; }" => Enum {
                comments: vec![],
                name: "bla",
                options: vec![],
                values: vec![
                    EnumValue {
                        comments: vec![],
                        name: "FOO",
                        value: 1,
                        options: vec![]
                    }
                ]
            },
            "// bla\nenum bla { FOO = 1; }" => Enum {
                comments: vec!["bla"],
                name: "bla",
                options: vec![],
                values: vec![
                    EnumValue {
                        comments: vec![],
                        name: "FOO",
                        value: 1,
                        options: vec![]
                    }
                ]
            },
            "enum bla { FOO = 1; BAR = -2; }" => Enum {
                comments: vec![],
                name: "bla",
                options: vec![],
                values: vec![
                    EnumValue {
                        comments: vec![],
                        name: "FOO",
                        value: 1,
                        options: vec![]
                    },
                    EnumValue {
                        comments: vec![],
                        name: "BAR",
                        value: -2,
                        options: vec![]
                    }
                ]
            },
            "enum EnumAllowingAlias {
                option allow_alias = true;
                UNKNOWN = 0;
                STARTED = 1;
                RUNNING = 2 [(custom_option) = 'hello world'];
            }" => Enum {
                comments: vec![],
                name: "EnumAllowingAlias",
                options: vec![(vec!["allow_alias"], Constant::BoolLit(true))],
                values: vec![
                    EnumValue {
                        comments: vec![],
                        name: "UNKNOWN",
                        value: 0,
                        options: vec![],
                    },
                    EnumValue {
                        comments: vec![],
                        name: "STARTED",
                        value: 1,
                        options: vec![],
                    },
                    EnumValue {
                        comments: vec![],
                        name: "RUNNING",
                        value: 2,
                        options: vec![(vec!["custom_option"], Constant::StrLit("hello world".into()))],
                    },
                ]
            },
        }
    }

    #[test]
    fn test_enum_field() {
        test_parser! {
            enum_field,
            "FOO = 1;" => EnumValue {
                comments: vec![],
                name: "FOO",
                value: 1,
                options: vec![],
            },
            "// bla\nFOO = 1;" => EnumValue {
                comments: vec!["bla"],
                name: "FOO",
                value: 1,
                options: vec![],
            },
            "FOO = -1;" => EnumValue {
                comments: vec![],
                name: "FOO",
                value: -1,
                options: vec![],
            },
            "FOO = 1 [packed=true];" => EnumValue {
                comments: vec![],
                name: "FOO",
                value: 1,
                options: vec![(vec!["packed"], Constant::BoolLit(true))],
            },
            "FOO = 1 [packed=true, bar='boo'];" => EnumValue {
                comments: vec![],
                name: "FOO",
                value: 1,
                options: vec![
                    (vec!["packed"], Constant::BoolLit(true)),
                    (vec!["bar"], Constant::StrLit("boo".to_string())),
                ],
            },
        }
    }

    #[test]
    fn test_message() {
        test_parser! {
            message,
            "message Outer {
                option (my_option).a = true;

                message Inner {
                    int64 ival = 1;
                }

                map<int32, string> my_map = 2;
            }" => Message {
                comments: vec![],
                name: "Outer",
                fields: vec![
                    Field {
                        comments: vec![],
                        key_type: Some(FieldType::Int32),
                        type_: FieldType::String,
                        name: "my_map",
                        num: 2,
                        options: vec![],
                        label: None
                    }
                ],
                oneofs: vec![],
                enums: vec![],
                messages: vec![
                    Message {
                        comments: vec![],
                        name: "Inner",
                        fields: vec![
                            Field {
                                comments: vec![],
                                key_type: None,
                                type_: FieldType::Int64,
                                name: "ival",
                                num: 1,
                                options: vec![],
                                label: None
                            }
                        ],
                        enums: vec![],
                        messages: vec![],
                        oneofs: vec![],
                        options: vec![]
                    }
                ],
                options: vec![(vec!["my_option", "a"], Constant::BoolLit(true))]
            },
        }
    }

    #[test]
    fn test_rpc() {
        test_parser! {
            rpc,
            "rpc Search (SearchRequest) returns (SearchResponse);" => Rpc {
                comments: vec![],
                name: "Search",
                request: FieldType::Custom(vec!["SearchRequest"]),
                response: FieldType::Custom(vec!["SearchResponse"]),
                options: vec![]
            },
        }
    }

    #[test]
    fn test_service() {
        test_parser! {
                    service,
                    "service SearchService {
                rpc Search(SearchRequest) returns (SearchResponse);
            }" => Service {
                        comments: vec![],
                        name: "SearchService",
                        rpcs: vec![
                            Rpc {
                                comments: vec![],
                                name: "Search",
                                request: FieldType::Custom(vec!["SearchRequest"]),
                                response: FieldType::Custom(vec!["SearchResponse"]),
                                options: vec![]
                            }
                        ],
                        options: vec![]
                    },
                }
    }

    #[test]
    fn test_parse_file() {
        let input = r#"
syntax = "proto3";
package foo.bar.v1;

import "other.proto";
option bla = 1;

enum Foo {
    BAR = 1;
}

message Bla {
    Foo foo = 1;
}

service BlaService {
    rpc Bla (Bla) returns (Bla);
}
            "#;
        let output = parse_file(input).unwrap();

        assert_eq!(output, File {
            comments: vec![],
            package: vec!["foo", "bar", "v1"],
            imports: vec!["other.proto".to_string()],
            options: vec![(vec!["bla"], Constant::FloatLit(1.0))],
            enums: vec![
                Enum {
                    comments: vec![],
                    name: "Foo",
                    options: vec![],
                    values: vec![
                        EnumValue {
                            comments: vec![],
                            name: "BAR",
                            value: 1,
                            options: vec![],
                        }
                    ],
                }
            ],
            messages: vec![
                Message {
                    comments: vec![],
                    name: "Bla",
                    fields: vec![
                        Field {
                            comments: vec![],
                            key_type: None,
                            type_: FieldType::Custom(vec!["Foo"]),
                            name: "foo",
                            num: 1,
                            options: vec![],
                            label: None
                        }
                    ],
                    oneofs: vec![],
                    enums: vec![],
                    messages: vec![],
                    options: vec![],
                }
            ],
            services: vec![
                Service {
                    comments: vec![],
                    name: "BlaService",
                    rpcs: vec![
                        Rpc {
                            comments: vec![],
                            name: "Bla",
                            request: FieldType::Custom(vec!["Bla"]),
                            response: FieldType::Custom(vec!["Bla"]),
                            options: vec![],
                        }
                    ],
                    options: vec![]
                }
            ]
        });
    }
}
