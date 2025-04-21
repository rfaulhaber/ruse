use std::borrow::Cow;

use crate::token::{Span, Token, TokenKind};
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_till, take_until, take_while};
use nom::character::complete::{bin_digit1, char, digit1, hex_digit1, oct_digit1, satisfy};
use nom::character::one_of;
use nom::combinator::{consumed, map, opt, recognize, value};
use nom::multi::{many0, many1};
use nom::sequence::{pair, preceded, separated_pair, terminated};
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

fn recognize_token(source: LocatedSpan<&str>) -> IResult<Span, Token> {
    let (remaining, (output, token_kind)) = alt((
        identifier,
        number,
        string,
        basic_token,
        character,
        datum_label_ref,
        datum_label_define,
    ))
    .parse(source)?;

    Ok((
        remaining,
        Token {
            kind: token_kind,
            span: output,
        },
    ))
}

fn basic_token(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    consumed(alt((
        value(TokenKind::LParen, char('(')),
        value(TokenKind::RParen, char(')')),
        value(TokenKind::VectorStart, tag("#(")),
        value(TokenKind::BytevectorStart, tag("#u8(")),
        value(TokenKind::Dot, char('.')),
        value(TokenKind::QuoteTick, char('\'')),
        value(TokenKind::QuasiquoteTick, char('`')),
        value(TokenKind::UnquoteSplicing, tag(",@")),
        value(TokenKind::UnquoteTick, char(',')),
        value(TokenKind::BooleanTrue, alt((tag("#true"), tag("#t")))),
        value(TokenKind::BooleanFalse, alt((tag("#false"), tag("#f")))),
        value(TokenKind::DirectiveFoldCase, tag("#!fold-case")),
        value(TokenKind::DirectiveNoFoldCase, tag("#!no-fold-case")),
    )))
    .parse(source)
}

fn identifier(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    let (remaining, output) = recognize(alt((
        preceded(
            one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!$%&*/:<=>?@^_~.+-"),
            take_while(|c| !is_delimeter(c)),
        ),
        recognize(separated_pair(char('|'), is_not("|"), char('|'))),
    )))
    .parse(source)?;

    Ok((
        remaining,
        (
            output,
            TokenKind::Identifier(Cow::Borrowed(output.fragment())),
        ),
    ))
}

fn number(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!();
}

fn character(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!()
}

fn string(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!()
}

fn datum_label_define(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!()
}

fn datum_label_ref(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!()
}

fn is_delimeter(c: char) -> bool {
    match c {
        '|' | '(' | ')' | '"' | ';' => true,
        c => c.is_whitespace(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boolean() {
        let input = "#true";
        let (remaining, result) = recognize_token(input.into()).unwrap();

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
            let result = recognize_token(id.into());
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

    #[test]
    fn number() {
        let numbers = [
            // Decimal Integers
            "0",
            "42",
            "+123",
            "-987654321",
            // Decimal Floating Point
            "1.0",
            "0.5",
            ".25",
            "123.",
            "3.14e2",
            "6.022e+23",
            "2.99792458e8",
            "-0.0",
            "-1e-10",
            "+1E10",
            // Rational Numbers
            "1/2",
            "3/4",
            "-4/5",
            "+12/7",
            // Complex Numbers (Rectangular)
            "1+2i",
            "3-4i",
            "0+0i",
            "1.5+2.5i",
            "3/2-5/6i",
            "+1.0-0.0i",
            // Complex Numbers (Pure Imaginary)
            "i",
            "+i",
            "-i",
            "1i",
            "-1.5i",
            "3/4i",
            // Infinity & NaN
            "+inf.0",
            "-inf.0",
            "+nan.0",
            "-nan.0",
            // Exactness Prefix
            "#e42",
            "#e1/2",
            "#e3.14",
            "#i123",
            "#i3/4",
            "#i2.71828",
            "#e+i",
            // Radix Prefix
            "#b1010",
            "#o755",
            "#d1234",
            "#xFF",
            "#xdeadbeef",
            "#b+1101",
            "#o-77",
            // Combined Prefixes (Exactness + Radix)
            "#e#xF",
            "#i#b1010",
            "#x#eF",
            "#d#i42",
            // Mixed edge cases
            "#e3+4i",
            "#i1.5-2.0i",
            "#x+1.0",
            "#b1/10",
            "#i+i",
        ];

        for num in numbers {
            let result = recognize_token(num.into());
            assert!(
                result.is_ok(),
                "parsing '{}' resulted in an error: {:?}",
                num,
                result
            );

            let (remaining, output) = result.unwrap();

            assert_eq!(
                output.kind,
                TokenKind::Number(num.into()),
                "should have captured entire number '{}'",
                num
            );
            assert_eq!(remaining.fragment(), &"");
        }
    }
}
