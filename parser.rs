use crate::ast::Expr;
use crate::lexer::{Token, TokenKind, Lexer, LexError};
use crate::span::SourceFile;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    source: Option<SourceFile>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            source: None,
        }
    }
    
    pub fn with_source(tokens: Vec<Token>, source: SourceFile) -> Self {
        Self {
            tokens,
            current: 0,
            source: Some(source),
        }
    }
    
    pub fn parse_from_str(input: &str) -> Result<Vec<Expr>, ParseError> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().map_err(ParseError::LexError)?;
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut expressions = Vec::new();
        
        while !self.is_at_end() {
            if self.check(&TokenKind::Eof) {
                break;
            }
            expressions.push(self.expression()?);
        }
        
        Ok(expressions)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        match &self.peek().kind {
            TokenKind::LeftParen => self.list(),
            TokenKind::LeftBracket => self.list(),
            TokenKind::Quote => {
                let quote_span = self.advance().span;
                let expr = self.expression()?;
                let span = quote_span.merge(expr.span());
                Ok(Expr::Quote(Box::new(expr), span))
            },
            TokenKind::Quasiquote => {
                let quasi_span = self.advance().span;
                let expr = self.expression()?;
                let span = quasi_span.merge(expr.span());
                Ok(Expr::Quasiquote(Box::new(expr), span))
            },
            TokenKind::Unquote => {
                let unquote_span = self.advance().span;
                let expr = self.expression()?;
                let span = unquote_span.merge(expr.span());
                Ok(Expr::Unquote(Box::new(expr), span))
            },
            TokenKind::UnquoteSplicing => {
                let splice_span = self.advance().span;
                let expr = self.expression()?;
                let span = splice_span.merge(expr.span());
                Ok(Expr::UnquoteSplicing(Box::new(expr), span))
            },
            _ => self.atom(),
        }
    }

    fn list(&mut self) -> Result<Expr, ParseError> {
        let open_token = self.advance().clone();
        let is_bracket = matches!(open_token.kind, TokenKind::LeftBracket);
        let closing_kind = if is_bracket { TokenKind::RightBracket } else { TokenKind::RightParen };
        
        let mut elements = Vec::new();
        
        while !self.check(&closing_kind) && !self.is_at_end() {
            if self.check(&TokenKind::Dot) {
                self.advance(); // consume dot
                let tail = self.expression()?;
                let end_token = self.consume(closing_kind, "Expected closing delimiter after dotted list")?;
                let span = open_token.span.merge(end_token.span);
                return Ok(Expr::DottedList(elements, Box::new(tail), span));
            }
            elements.push(self.expression()?);
        }
        
        let close_token = self.consume(closing_kind, "Expected closing delimiter")?;
        let span = open_token.span.merge(close_token.span);
        Ok(Expr::List(elements, span))
    }

    fn atom(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance().clone();
        
        match token.kind {
            TokenKind::Number(n) => Ok(Expr::Number(n, token.span)),
            TokenKind::Integer(i) => Ok(Expr::Integer(i, token.span)),
            TokenKind::String(s) => Ok(Expr::String(s, token.span)),
            TokenKind::Character(c) => Ok(Expr::Character(c, token.span)),
            TokenKind::Boolean(b) => Ok(Expr::Boolean(b, token.span)),
            TokenKind::Identifier(s) => Ok(Expr::Symbol(s, token.span)),
            _ => Err(ParseError::UnexpectedToken {
                lexeme: token.lexeme.clone(),
                span: SourceSpan::new(token.span.start.into(), token.span.len()),
            }),
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<&Token, ParseError> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            Err(ParseError::Expected {
                message: message.to_string(),
                lexeme: token.lexeme.clone(),
                span: SourceSpan::new(token.span.start.into(), token.span.len()),
            })
        }
    }
}

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    LexError(#[from] LexError),
    
    #[error("Unexpected token '{lexeme}'")]
    #[diagnostic(help("Check the syntax at this location"))]
    UnexpectedToken {
        lexeme: String,
        #[label("unexpected token here")]
        span: SourceSpan,
    },
    
    #[error("{message}")]
    #[diagnostic(help("Check the syntax - this token was expected"))]
    Expected {
        message: String,
        lexeme: String,
        #[label("found this instead")]
        span: SourceSpan,
    },
    
    #[error("Unexpected end of input")]
    #[diagnostic(help("The input ended while parsing was still in progress"))]
    UnexpectedEof {
        #[label("input ends here")]
        span: SourceSpan,
    },
}

impl ParseError {
    pub fn span(&self) -> SourceSpan {
        match self {
            ParseError::LexError(err) => err.span(),
            ParseError::UnexpectedToken { span, .. } => *span,
            ParseError::Expected { span, .. } => *span,
            ParseError::UnexpectedEof { span, .. } => *span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_atoms() {
        let exprs = Parser::parse_from_str("42 foo #t").unwrap();
        
        assert_eq!(exprs.len(), 3);
        assert!(matches!(exprs[0], Expr::Integer(42, _)));
        assert!(matches!(exprs[1], Expr::Symbol(ref s, _) if s == "foo"));
        assert!(matches!(exprs[2], Expr::Boolean(true, _)));
    }

    #[test]
    fn test_parser_simple_list() {
        let exprs = Parser::parse_from_str("(+ 1 2)").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::List(ref elements, _) = exprs[0] {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0], Expr::Symbol(ref s, _) if s == "+"));
            assert!(matches!(elements[1], Expr::Integer(1, _)));
            assert!(matches!(elements[2], Expr::Integer(2, _)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_parser_nested_lists() {
        let exprs = Parser::parse_from_str("((f 1) (g 2))").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::List(ref outer, _) = exprs[0] {
            assert_eq!(outer.len(), 2);
            
            if let Expr::List(ref inner1, _) = outer[0] {
                assert_eq!(inner1.len(), 2);
                assert!(matches!(inner1[0], Expr::Symbol(ref s, _) if s == "f"));
                assert!(matches!(inner1[1], Expr::Integer(1, _)));
            } else {
                panic!("Expected inner list");
            }
        } else {
            panic!("Expected outer list");
        }
    }

    #[test]
    fn test_parser_dotted_list() {
        let exprs = Parser::parse_from_str("(a b . c)").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::DottedList(ref elements, ref tail, _) = exprs[0] {
            assert_eq!(elements.len(), 2);
            assert!(matches!(elements[0], Expr::Symbol(ref s, _) if s == "a"));
            assert!(matches!(elements[1], Expr::Symbol(ref s, _) if s == "b"));
            assert!(matches!(**tail, Expr::Symbol(ref s, _) if s == "c"));
        } else {
            panic!("Expected dotted list");
        }
    }

    #[test]
    fn test_parser_quotes() {
        let exprs = Parser::parse_from_str("'x `(a ,b ,@c)").unwrap();
        
        assert_eq!(exprs.len(), 2);
        
        if let Expr::Quote(ref inner, _) = exprs[0] {
            assert!(matches!(**inner, Expr::Symbol(ref s, _) if s == "x"));
        } else {
            panic!("Expected quote");
        }
        
        if let Expr::Quasiquote(ref inner, _) = exprs[1] {
            if let Expr::List(ref elements, _) = &**inner {
                assert_eq!(elements.len(), 3);
                assert!(matches!(elements[0], Expr::Symbol(ref s, _) if s == "a"));
                assert!(matches!(elements[1], Expr::Unquote(_, _)));
                assert!(matches!(elements[2], Expr::UnquoteSplicing(_, _)));
            } else {
                panic!("Expected list inside quasiquote");
            }
        } else {
            panic!("Expected quasiquote");
        }
    }

    #[test]
    fn test_parser_comments_ignored() {
        let exprs = Parser::parse_from_str("; this is a comment\n42 ; another comment\n").unwrap();
        
        assert_eq!(exprs.len(), 1);
        assert!(matches!(exprs[0], Expr::Integer(42, _)));
    }

    #[test]
    fn test_parser_span_tracking() {
        let input = "(+ 1 2)";
        let exprs = Parser::parse_from_str(input).unwrap();
        
        // Check that the list spans the entire input
        let list_span = exprs[0].span();
        assert_eq!(list_span.start, 0);
        assert_eq!(list_span.end, input.len());
    }

    #[test]
    fn test_parser_brackets() {
        let exprs = Parser::parse_from_str("[+ 1 2]").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::List(ref elements, _) = exprs[0] {
            assert_eq!(elements.len(), 3);
            assert!(matches!(elements[0], Expr::Symbol(ref s, _) if s == "+"));
        } else {
            panic!("Expected list from brackets");
        }
    }

    #[test]
    fn test_parser_mixed_quotes() {
        let exprs = Parser::parse_from_str("'`(,a ,@b)").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::Quote(ref inner, _) = exprs[0] {
            if let Expr::Quasiquote(ref inner2, _) = &**inner {
                if let Expr::List(ref elements, _) = &**inner2 {
                    assert_eq!(elements.len(), 2);
                    assert!(matches!(elements[0], Expr::Unquote(_, _)));
                    assert!(matches!(elements[1], Expr::UnquoteSplicing(_, _)));
                } else {
                    panic!("Expected list inside quasiquote");
                }
            } else {
                panic!("Expected quasiquote inside quote");
            }
        } else {
            panic!("Expected quote");
        }
    }

    #[test]
    fn test_parser_empty_list() {
        let exprs = Parser::parse_from_str("()").unwrap();
        
        assert_eq!(exprs.len(), 1);
        if let Expr::List(ref elements, _) = exprs[0] {
            assert_eq!(elements.len(), 0);
        } else {
            panic!("Expected empty list");
        }
    }

    #[test]
    fn test_parser_string_atoms() {
        let exprs = Parser::parse_from_str(r#""hello world" #\a #t #f"#).unwrap();
        
        assert_eq!(exprs.len(), 4);
        assert!(matches!(exprs[0], Expr::String(ref s, _) if s == "hello world"));
        assert!(matches!(exprs[1], Expr::Character('a', _)));
        assert!(matches!(exprs[2], Expr::Boolean(true, _)));
        assert!(matches!(exprs[3], Expr::Boolean(false, _)));
    }
}