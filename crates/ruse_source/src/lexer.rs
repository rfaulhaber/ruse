use std::borrow::Cow;

use crate::token::{Span, Token, TokenKind};
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_till, take_until, take_while, take_while1};
use nom::character::complete::{bin_digit1, char, digit0, digit1, hex_digit1, oct_digit1, satisfy};
use nom::character::one_of;
use nom::combinator::{consumed, map, opt, recognize, value};
use nom::multi::{many0, many1};
use nom::sequence::{pair, preceded, separated_pair, terminated};
use nom::{AsChar, IResult, Parser};
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
        number,
        identifier,
        string,
        datum_label_ref,
        datum_label_define,
        basic_token,
        character,
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
    let (remaining, output) = alt((
        identifier_initial_subsequent,
        identifier_vertical_line,
        identifier_peculiar,
    ))
    .parse(source)?;

    Ok((
        remaining,
        (output, TokenKind::Identifier(Cow::from(*output.fragment()))),
    ))
}

fn identifier_initial_subsequent(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!()
}

fn identifier_vertical_line(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(separated_pair(char('|'), is_not("|"), char('|'))).parse(source)
}

fn identifier_peculiar(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!()
}

fn number(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    let (remaining, output) = todo!();

    Ok((remaining, (output, TokenKind::Number(output.fragment()))))
}

fn number_complex(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!()
}

fn number_real(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!()
}

fn number_ureal(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(alt((
        recognize(separated_pair(number_uinteger, char('/'), number_uinteger)),
        number_uinteger,
        number_decimal,
    )))
    .parse(source)
}

fn number_uinteger(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    take_while1(|c| "0123456789abcdefABCDEF".contains(c)).parse(source)
}

fn number_prefix(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((
        recognize(pair(number_radix, number_exactness)),
        recognize(pair(number_exactness, number_radix)),
    ))
    .parse(source)
}

fn number_radix(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(opt(alt((tag("#b"), tag("#o"), tag("#d"), tag("#x"))))).parse(source)
}

fn number_suffix(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!();
}

fn number_exactness(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize(opt(alt((tag("#i"), tag("#e"))))).parse(source)
}

fn number_decimal(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    todo!();
}

fn number_exponent_part(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    recognize((one_of("eE"), opt(one_of("+-")), digit1)).parse(source)
}

fn number_infnan(source: LocatedSpan<&str>) -> IResult<Span, Span> {
    alt((tag("+inf.0"), tag("-inf.0"), tag("+nan.0"), tag("-nan.0"))).parse(source)
}

fn character(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    todo!()
}

fn string(source: LocatedSpan<&str>) -> IResult<Span, (Span, TokenKind)> {
    let (remaining, output) =
        recognize(separated_pair(char('"'), is_not("\""), char('"'))).parse(source)?;

    Ok((
        remaining,
        (output, TokenKind::String((*output.fragment()).into())),
    ))
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

fn is_special_initial(c: char) -> bool {
    matches!(
        c,
        '!' | '$' | '%' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
    )
}

fn is_dot_subsequent(c: char) -> bool {
    is_sign_subsequent(c) || c == '.'
}

fn is_sign_subsequent(c: char) -> bool {
    is_identifier_initial(c) || is_explicit_sign(c) || c == '@'
}

fn is_explicit_sign(c: char) -> bool {
    c == '+' || c == '-'
}

fn is_identifier_subsequent(c: char) -> bool {
    is_identifier_initial(c) || c.is_digit(10) || is_special_subsequent(c)
}

fn is_special_subsequent(c: char) -> bool {
    matches!(c, '+' | '-' | '.' | '@')
}

fn is_identifier_initial(c: char) -> bool {
    c.is_alphabetic() || is_special_initial(c)
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
    fn identifier_parser() {
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
            let result = super::identifier(id.into());
            assert!(
                result.is_ok(),
                "parsing '{}' resulted in an error: {:?}",
                id,
                result
            );

            let (remaining, (output_span, token_kind)) = result.unwrap();

            assert_eq!(
                token_kind,
                TokenKind::Identifier(id.into()),
                "should have captured entire identifier '{}'",
                id
            );

            assert_eq!(
                *output_span.fragment(),
                id,
                "Output span should capture entire identifier '{}', but got '{:?}'",
                id,
                output_span.fragment()
            );

            assert_eq!(remaining.fragment(), &"");
        }
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
            let result = super::number(num.into());
            assert!(
                result.is_ok(),
                "parsing '{}' resulted in an error: {:?}",
                num,
                result
            );

            let (remaining, (output, token_kind)) = result.unwrap();

            assert_eq!(
                token_kind,
                TokenKind::Number(num.into()),
                "should have captured entire number '{}'",
                num
            );
            assert_eq!(remaining.fragment(), &"");
        }
    }

    #[test]
    fn uinteger_parser() {
        let input = "+123";
        let result = super::number_complex(input.into());
        assert!(result.is_ok(), "Parsing failed, received {:?}", result);
        let (_, output) = result.unwrap();
        assert_eq!(*output.fragment(), input);
    }
}
