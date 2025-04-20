use crate::token::{Span, Token, TokenKind};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, hex_digit1, satisfy};
use nom::combinator::{map, recognize, value};
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated};
use nom::{IResult, Parser};
use nom_locate::LocatedSpan;
use thiserror::Error;

pub type LexerResult<'l> = Result<Token<'l>, LexerError>;

#[derive(Debug, Error)]
pub enum LexerError {}

#[derive(Debug)]
pub struct Lexer<'l> {
    source: LocatedSpan<&'l str>,
}

impl<'l> Iterator for Lexer<'l> {
    type Item = LexerResult<'l>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl<'l> From<&'l str> for Lexer<'l> {
    fn from(value: &'l str) -> Self {
        Self {
            source: LocatedSpan::new(value),
        }
    }
}

impl<'l> Lexer<'l> {
    pub fn new<I: Into<LocatedSpan<&'l str>>>(source: I) -> Self {
        Self {
            source: source.into(),
        }
    }

    fn parse_next_token(&mut self) -> IResult<Span, Token> {
        todo!();
    }
}

fn parse_token(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, output) = alt((
        parse_identifier,
        parse_boolean,
        parse_number,
        parse_character,
        parse_string,
    ))
    .parse(source)?;

    Ok((remaining, output))
}

fn parse_identifier(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, output) = alt((
        identifier_init_subsequent,
        identifier_symbol_element,
        identifier_peculiar,
    ))
    .parse(source)?;

    Ok((
        remaining,
        Token {
            kind: TokenKind::Identifier((*output.fragment()).into()),
            span: output,
        },
    ))
}

fn identifier_init_subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(pair(initial, many0(subsequent))).parse(source)
}

fn initial(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(satisfy(|c| {
        matches!(
            c,
            'a'..='z'|
            'A'..='Z'|
            '!'|
            '$'|
            '%'|
            '&'|
            '*'|
            '/'|
            ':'|
            '<'|
            '='|
            '>'|
            '?'|
            '@'|
            '^'|
            '_'|
            '~'
        )
    }))
    .parse(source)
}

fn subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(alt((initial, digit, special_subsequent))).parse(source)
}

fn digit(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(satisfy(|c| matches!(c, '0'..='9'))).parse(source)
}

fn special_subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(alt((
        explicit_sign,
        recognize(char('.')),
        recognize(char('@')),
    )))
    .parse(source)
}

fn explicit_sign(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(satisfy(|c| c == '+' || c == '-')).parse(source)
}

fn identifier_symbol_element(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize((vertical_line, many0(symbol_element), vertical_line)).parse(source)
}

fn symbol_element(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((
        recognize(satisfy(|c| c != '|' && c != '\\')),
        recognize(inline_hex_escape),
        mnemonic_escape,
    ))
    .parse(source)
}

fn vertical_line(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(satisfy(|c| c == '|')).parse(source)
}

fn inline_hex_escape(source: LocatedSpan<&str>) -> IResult<Span, char> {
    preceded(
        tag(r"\x"),
        map(terminated(hex_digit1, char(';')), |hex: Span| {
            let code = u32::from_str_radix(hex.fragment(), 16).unwrap();
            std::char::from_u32(code).unwrap()
        }),
    )
    .parse(source)
}

fn mnemonic_escape(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(alt((
        value(r"\a", tag("a")),
        value(r"\b", tag("b")),
        value(r"\t", tag("t")),
        value(r"\n", tag("n")),
        value(r"\r", tag("r")),
    )))
    .parse(source)
}

fn identifier_peculiar(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((
        recognize((explicit_sign, sign_subsequent, many0(subsequent))),
        recognize((explicit_sign, char('.'), dot_subsequent, many0(subsequent))),
        recognize((char('.'), dot_subsequent, many0(subsequent))),
        recognize(explicit_sign),
    ))
    .parse(source)
}

fn sign_subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((
        recognize(initial),
        recognize(explicit_sign),
        recognize(char('@')),
    ))
    .parse(source)
}

fn dot_subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((recognize(sign_subsequent), recognize(char('.')))).parse(source)
}

fn parse_number(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    todo!();
}

fn parse_character(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    todo!();
}

fn parse_string(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    todo!();
}

fn parse_boolean(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, output) = alt((parse_true, parse_false)).parse(source)?;

    Ok((remaining, output))
}

fn parse_true(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, output) = alt((tag("#true"), tag("#t"))).parse(source)?;

    Ok((
        remaining,
        Token {
            kind: TokenKind::BooleanTrue,
            span: output,
        },
    ))
}

fn parse_false(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, output) = alt((tag("#false"), tag("#f"))).parse(source)?;

    Ok((
        remaining,
        Token {
            kind: TokenKind::BooleanFalse,
            span: output,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boolean() {
        let input = "#true";
        let (remaining, result) = parse_boolean(input.into()).unwrap();

        assert_eq!(result.kind, TokenKind::BooleanTrue);
        assert_eq!(result.span.location_line(), 1);
        assert_eq!(result.span.location_offset(), 0);
        assert_eq!(remaining.fragment(), &"");
    }

    #[test]
    fn identifier() {
        let identifiers = [
            "...",
            "+soup+",
            "->string",
            "lambda",
            "q",
            "|two words|",
            "the-word-recursion-has-many-meanings",
            "+",
            "<=?",
            "a34kTMNs",
            "list->vector",
            "V17a",
            r#"|two\x20;words|"#,
        ];

        for id in identifiers {
            let result = parse_identifier(id.into());
            assert!(
                result.is_ok(),
                "parsing '{}' resulted in an error: {:?}",
                id,
                result
            );

            let (remaining, output) = result.unwrap();

            assert_eq!(
                output.kind,
                TokenKind::Identifier(id.into()),
                "should have captured entire identifier '{}'",
                id
            );
            assert_eq!(remaining.fragment(), &"");
        }
    }
}
