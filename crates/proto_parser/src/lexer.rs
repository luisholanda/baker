use crate::tokens::Token;
use nom::{
    branch, bytes::complete as bytes, character::complete as character, combinator, multi,
    number::complete as number, sequence, IResult,
};

macro_rules! def_pub_parsers {
    ($($name: ident = $parser: expr;)+) => {
        $(#[allow(non_snake_case)] #[inline(always)] pub fn $name(input: &str) -> Result {
            use nom::Parser;
            Parser($parser).parse(input)
        })+
    }
}

def_pub_parsers! {
    IDENT = ident;
    FULL_IDENT = full_ident;
    MESSAGE_TYPE = message_type;
    FLOAT_LIT = float_lit;
    BOOL_LIT = bool_lit;
    STR_LIT = str_lit;
    COMMENT = comment;
    SYNTAX = syntax;
    PROTO3 = proto3;
    IMPORT = import;
    PACKAGE = package;
    REPEATED = repeated;
    OPTIONAL = optional;
    OPTION = option;
    ONEOF = oneof;
    ENUM = _enum;
    MAP = map;
    MESSAGE = message;
    SERVICE = service;
    RPC = rpc;
    RETURNS = returns;
    SEMI_COLON = semi_colon;
    EQUALS = equals;
    OPEN_BRACE = open_brace;
    OPEN_BRACKET = open_bracket;
    OPEN_PAREN = open_paren;
    CLOSE_BRACE = close_brace;
    CLOSE_BRACKET = close_bracket;
    CLOSE_PAREN = close_paren;
    LESS_THAN = less_than;
    GREATER_THAN = greater_than;
    QUOTE = quote;
    COMMA = comma;
    BUILTIN_TYPE = builtin_type;
}

pub fn number(input: &str) -> Result<u64> {
    sequence::delimited(
        character::multispace0,
        character::u64,
        character::multispace0,
    )(input)
}

type Result<'a, T = Token<'a>> = IResult<&'a str, T>;

#[derive(Clone, Copy)]
pub struct Parser(for<'a> fn(&'a str) -> Result);

impl<'a> nom::Parser<&'a str, Token<'a>, nom::error::Error<&'a str>> for Parser {
    #[inline]
    fn parse(&mut self, input: &'a str) -> IResult<&'a str, Token<'a>, nom::error::Error<&'a str>> {
        sequence::delimited(character::multispace0, self.0, character::multispace0)(input)
    }
}

fn oct_digit(input: &str) -> IResult<&str, char> {
    character::one_of("01234567")(input)
}

fn hex_digit(input: &str) -> IResult<&str, char> {
    character::one_of("0123456789ABCDEFabcdef")(input)
}

fn comment(input: &str) -> Result {
    let prefix = sequence::pair(bytes::tag("//"), character::space1);
    let line = sequence::preceded(prefix, character::not_line_ending);
    let mut lines = multi::separated_list0(character::line_ending, line);
    let (s, comments) = lines(input)?;

    Ok((s, Token::Comment(comments)))
}

macro_rules! def_keyword_parsers {
    ($($keyword: ident),+) => {
        $(
            #[inline]
            fn $keyword(input: &str) -> Result {
                combinator::map(bytes::tag(stringify!($keyword)), Token::Keyword)(input)
            }
        )+
    }
}

def_keyword_parsers!(
    syntax, proto3, import, package, repeated, optional, option, oneof, map, message,
    service, rpc, double, float, int32, int64, uint32, uint64, sint32, sint64, fixed32,
    sfixed32, sfixed64, bool, string, bytes, returns
);

fn builtin_type(input: &str) -> Result {
    branch::alt((
        double, float, int32, int64, uint32, uint64, sint32, sint64, fixed32, sfixed32, sfixed64,
        bool, string, bytes,
    ))(input)
}

#[inline]
fn _enum(input: &str) -> Result {
    combinator::map(bytes::tag("enum"), Token::Keyword)(input)
}

macro_rules! def_punctuation_parsers {
    ($($punc: ident = $char: literal = $value: ident;)+) => {
        $(
            #[inline]
            fn $punc(input: &str) -> Result {
                combinator::value(Token::$value, character::char($char))(input)
            }
        )+
    }
}

def_punctuation_parsers! {
    semi_colon = ';' = SemiColon;
    equals = '=' = Equals;
    open_brace = '{' = OpenBrace;
    open_bracket = '[' = OpenBracket;
    open_paren = '(' = OpenParen;
    close_brace = '}' = CloseBrace;
    close_bracket = ']' = CloseBracket;
    close_paren = ')' = CloseParen;
    less_than = '<' = LessThan;
    greater_than = '>' = GreaterThan;
    double_quote = '"' = DoubleQuote;
    single_quote = '\'' = SingleQuote;
    comma = ',' = Comma;
}

#[inline]
fn quote(input: &str) -> Result {
    branch::alt((double_quote, single_quote))(input)
}

fn str_ident(input: &str) -> IResult<&str, &str> {
    combinator::recognize(sequence::pair(
        character::alpha1,
        multi::many0(branch::alt((character::alphanumeric1, bytes::tag("_")))),
    ))(input)
}

fn ident(input: &str) -> Result {
    let (s, idnt) = str_ident(input)?;

    // A period after the ident means a full_ident.
    if character::char::<_, nom::error::Error<_>>('.')(s).is_ok() {
        Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::Char,
        )))
    } else {
        Ok((s, Token::Ident(idnt)))
    }
}

fn full_ident(input: &str) -> Result {
    let (s, idents) = multi::separated_list1(bytes::tag("."), str_ident)(input)?;

    Ok((s, Token::FullIdent(idents)))
}

fn message_type(input: &str) -> Result {
    let s = combinator::opt(bytes::tag("."))(input)?.0;

    full_ident(s)
}

fn signal(input: &str) -> IResult<&str, Option<&str>> {
    combinator::opt(branch::alt((bytes::tag("-"), bytes::tag("+"))))(input)
}

fn float_lit(input: &str) -> Result {
    let (s, signal) = signal(input)?;
    let (s, val) = branch::alt((
        combinator::value(f64::INFINITY, bytes::tag("inf")),
        combinator::value(f64::NAN, bytes::tag("nan")),
        number::double,
    ))(s)?;

    let val = if signal == Some("-") { -val } else { val };

    Ok((s, Token::FloatLit(val)))
}

fn bool_lit(input: &str) -> Result {
    branch::alt((
        combinator::value(Token::BoolLit(true), bytes::tag("true")),
        combinator::value(Token::BoolLit(false), bytes::tag("false")),
    ))(input)
}

fn str_lit(input: &str) -> Result {
    let parse_chars = |i| multi::many0(char_value)(i);

    let (s, quote) = branch::alt((double_quote, single_quote))(input)?;
    let final_quote = match quote {
        Token::SingleQuote => single_quote,
        Token::DoubleQuote => double_quote,
        _ => unreachable!(),
    };

    let (s, chars) = sequence::terminated(parse_chars, final_quote)(s)?;

    Ok((s, Token::StrLit(chars.into_iter().collect())))
}

fn char_value(input: &str) -> IResult<&str, char> {
    fn hex_escape(input: &str) -> IResult<&str, char> {
        let parse_hex_str = sequence::preceded(
            character::one_of("xX"),
            combinator::recognize(sequence::pair(hex_digit, hex_digit)),
        );
        let parse_hex_u32 = combinator::map_res(parse_hex_str, |opt| u32::from_str_radix(opt, 16));
        combinator::map_opt(parse_hex_u32, char::from_u32)(input)
    }

    fn oct_escape(input: &str) -> IResult<&str, char> {
        let parse_oct_str =
            combinator::recognize(sequence::tuple((oct_digit, oct_digit, oct_digit)));
        let parse_oct_u32 = combinator::map_res(parse_oct_str, |opt| u32::from_str_radix(opt, 8));
        combinator::map_opt(parse_oct_u32, char::from_u32)(input)
    }

    // TODO: Add \a.
    fn char_escape(input: &str) -> IResult<&str, char> {
        let input = character::char('\\')(input)?.0;
        branch::alt((
            combinator::value('\u{08}', character::char('b')),
            combinator::value('\u{0C}', character::char('f')),
            combinator::value('\n', character::char('n')),
            combinator::value('\r', character::char('r')),
            combinator::value('\t', character::char('t')),
            combinator::value('\x0B', character::char('v')),
            character::char('\\'),
            character::char('\''),
            character::char('"'),
        ))(input)
    }

    branch::alt((
        hex_escape,
        oct_escape,
        char_escape,
        character::none_of("\0\n\\'\""),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment() {
        assert_output!(comment, "// bla\n// foobar", Token::Comment(vec!["bla", "foobar"]));
    }

    #[test]
    fn test_keyword() {
        // Keywords are created in macros, test one is enough.
        assert_output!(syntax, "syntax", Token::Keyword("syntax"));
    }

    #[test]
    fn test_str_ident() {
        assert_output!(str_ident, "abc123_abc", "abc123_abc");
    }

    #[test]
    fn test_ident() {
        assert_output!(ident, "abc123_abc123", Token::Ident("abc123_abc123"));
        assert!(ident("abcasd.").is_err());
    }

    #[test]
    fn test_full_ident() {
        assert_output!(
            full_ident,
            "abc123_abc123.abca",
            Token::FullIdent(vec!["abc123_abc123", "abca"])
        );
    }

    #[test]
    fn test_message_type() {
        assert_output!(
            message_type,
            "abc123_abc123.abca",
            Token::FullIdent(vec!["abc123_abc123", "abca"])
        );
        assert_output!(
            message_type,
            ".abc123_abc123.abca",
            Token::FullIdent(vec!["abc123_abc123", "abca"])
        );
    }

    #[test]
    fn test_float_lit() {
        test_parser! {
            float_lit,
            "1.2345" => Token::FloatLit(1.2345),
            "1.234e5" => Token::FloatLit(1.234e5),
            "1.234E5" => Token::FloatLit(1.234E5),
            "-1.2345" => Token::FloatLit(-1.2345),
            "-1.234e5" => Token::FloatLit(-1.234e5),
            "-1.234E5" => Token::FloatLit(-1.234E5),
            "inf" => Token::FloatLit(f64::INFINITY),
            "-inf" => Token::FloatLit(-f64::INFINITY),
        }
    }

    #[test]
    fn test_bool_lit() {
        assert_output!(bool_lit, "true", Token::BoolLit(true));
        assert_output!(bool_lit, "false", Token::BoolLit(false));
    }

    #[test]
    fn test_str_lit() {
        test_parser! {
            str_lit,
            r#""bla\r\t""# => Token::StrLit("bla\r\t".to_string()),
            r#""""# => Token::StrLit("".to_string()),
            r"'bla\r\t'" => Token::StrLit("bla\r\t".to_string()),
            "''" => Token::StrLit("".to_string()),
        }
    }
}
