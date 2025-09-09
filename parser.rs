use crate::ast::Expr;
use crate::lexer::{LexError, Lexer, Token, TokenKind};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct ParserConfig {
    pub allow_extra_closing_parens: bool,
    pub recover_from_errors: bool,
    pub max_nesting_depth: usize,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            allow_extra_closing_parens: false,
            recover_from_errors: false,
            max_nesting_depth: 1000,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    config: ParserConfig,
    nesting_depth: usize,
}

pub struct StreamingParser<I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    tokens: I,
    peeked: Vec<Token>, // Lookahead buffer
    eof_reached: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            config: ParserConfig::default(),
            nesting_depth: 0,
        }
    }
    
    pub fn with_config(tokens: Vec<Token>, config: ParserConfig) -> Self {
        Self {
            tokens,
            current: 0,
            config,
            nesting_depth: 0,
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

    pub fn parse_streaming_from_str(input: &str) -> Result<Vec<Expr>, ParseError> {
        let lexer = Lexer::new(input);
        let mut parser = StreamingParser::new(lexer.into_iter());
        parser.parse()
    }
    
    /// Parse with error recovery - returns partial results even if some expressions fail
    pub fn parse_with_recovery(input: &str) -> (Vec<Expr>, Vec<ParseError>) {
        let mut lexer = Lexer::new(input);
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(lex_err) => return (Vec::new(), vec![ParseError::LexError(lex_err)]),
        };
        
        let mut parser = Parser::new(tokens);
        let mut expressions = Vec::new();
        let mut errors = Vec::new();
        
        while !parser.is_at_end() {
            if parser.check(&TokenKind::Eof) {
                break;
            }
            
            match parser.expression() {
                Ok(expr) => expressions.push(expr),
                Err(err) => {
                    errors.push(err);
                    // Try to recover by skipping to the next top-level expression
                    parser.recover_to_next_expression();
                }
            }
        }
        
        (expressions, errors)
    }
    
    /// Attempt to recover from parse errors by finding the next likely expression start
    fn recover_to_next_expression(&mut self) {
        while !self.is_at_end() {
            match self.peek().kind {
                TokenKind::LeftParen | TokenKind::LeftBracket | 
                TokenKind::Quote | TokenKind::Quasiquote |
                TokenKind::Integer(_) | TokenKind::Number(_) |
                TokenKind::String(_) | TokenKind::Character(_) |
                TokenKind::Boolean(_) | TokenKind::Identifier(_) => {
                    // Found a potential start of an expression
                    break;
                }
                TokenKind::RightParen | TokenKind::RightBracket => {
                    // Skip unmatched closing delimiters
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
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
            }
            TokenKind::Quasiquote => {
                let quasi_span = self.advance().span;
                let expr = self.expression()?;
                let span = quasi_span.merge(expr.span());
                Ok(Expr::Quasiquote(Box::new(expr), span))
            }
            TokenKind::Unquote => {
                let unquote_span = self.advance().span;
                let expr = self.expression()?;
                let span = unquote_span.merge(expr.span());
                Ok(Expr::Unquote(Box::new(expr), span))
            }
            TokenKind::UnquoteSplicing => {
                let splice_span = self.advance().span;
                let expr = self.expression()?;
                let span = splice_span.merge(expr.span());
                Ok(Expr::UnquoteSplicing(Box::new(expr), span))
            }
            _ => self.atom(),
        }
    }

    fn list(&mut self) -> Result<Expr, ParseError> {
        let open_token = self.advance().clone();
        let is_bracket = matches!(open_token.kind, TokenKind::LeftBracket);
        let closing_kind = if is_bracket {
            TokenKind::RightBracket
        } else {
            TokenKind::RightParen
        };

        let mut elements = Vec::new();

        while !self.check(&closing_kind) && !self.is_at_end() {
            if self.check(&TokenKind::Dot) {
                self.advance(); // consume dot
                let tail = self.expression()?;
                let end_token =
                    self.consume(closing_kind, "Expected closing delimiter after dotted list")?;
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
            _ => {
                let context = "atom expression";
                let suggestion = match token.kind {
                    TokenKind::LeftParen => "This looks like the start of a list - atoms cannot start with '('".to_string(),
                    TokenKind::RightParen => "Unexpected closing parenthesis - check for unmatched delimiters".to_string(),
                    TokenKind::LeftBracket => "This looks like the start of a list - atoms cannot start with '['".to_string(),
                    TokenKind::RightBracket => "Unexpected closing bracket - check for unmatched delimiters".to_string(),
                    TokenKind::Dot => "Dots can only appear in dotted lists like (a . b)".to_string(),
                    TokenKind::Eof => "Unexpected end of file".to_string(),
                    _ => format!("'{}' cannot be used as an atom here", token.lexeme),
                };
                
                Err(ParseError::UnexpectedToken {
                    lexeme: token.lexeme.clone(),
                    context: context.to_string(),
                    suggestion,
                    span: SourceSpan::new(token.span.start.into(), token.span.len()),
                })
            }
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
            let suggestion = match kind {
                TokenKind::RightParen => "Add a ')' to close the list or check for extra opening parentheses".to_string(),
                TokenKind::RightBracket => "Add a ']' to close the list or check for extra opening brackets".to_string(),
                TokenKind::LeftParen => "Expected '(' to start a list".to_string(),
                _ => format!("Expected '{}' here", Self::token_kind_name(&kind)),
            };
            
            Err(ParseError::Expected {
                message: message.to_string(),
                lexeme: token.lexeme.clone(),
                suggestion,
                span: SourceSpan::new(token.span.start.into(), token.span.len()),
            })
        }
    }
    
    fn token_kind_name(kind: &TokenKind) -> &'static str {
        match kind {
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::Quote => "'",
            TokenKind::Quasiquote => "`",
            TokenKind::Unquote => ",",
            TokenKind::UnquoteSplicing => ",@",
            TokenKind::Dot => ".",
            TokenKind::Eof => "end of file",
            _ => "token",
        }
    }
}

impl<I> StreamingParser<I>
where
    I: Iterator<Item = Result<Token, LexError>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens,
            peeked: Vec::new(),
            eof_reached: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut expressions = Vec::new();

        while !self.is_at_end()? {
            expressions.push(self.expression()?);
        }

        Ok(expressions)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        let next = self.peek()?;
        match &next.kind {
            TokenKind::LeftParen | TokenKind::LeftBracket => self.list(),
            TokenKind::Quote => {
                let quote_token = self.advance()?;
                let expr = self.expression()?;
                let span = quote_token.span.merge(expr.span());
                Ok(Expr::Quote(Box::new(expr), span))
            }
            TokenKind::Quasiquote => {
                let quasi_token = self.advance()?;
                let expr = self.expression()?;
                let span = quasi_token.span.merge(expr.span());
                Ok(Expr::Quasiquote(Box::new(expr), span))
            }
            TokenKind::Unquote => {
                let unquote_token = self.advance()?;
                let expr = self.expression()?;
                let span = unquote_token.span.merge(expr.span());
                Ok(Expr::Unquote(Box::new(expr), span))
            }
            TokenKind::UnquoteSplicing => {
                let splice_token = self.advance()?;
                let expr = self.expression()?;
                let span = splice_token.span.merge(expr.span());
                Ok(Expr::UnquoteSplicing(Box::new(expr), span))
            }
            _ => self.atom(),
        }
    }

    fn list(&mut self) -> Result<Expr, ParseError> {
        let open_token = self.advance()?;
        let is_bracket = matches!(open_token.kind, TokenKind::LeftBracket);
        let closing_kind = if is_bracket {
            TokenKind::RightBracket
        } else {
            TokenKind::RightParen
        };

        let mut elements = Vec::new();

        while !self.check(&closing_kind)? && !self.is_at_end()? {
            if self.check(&TokenKind::Dot)? {
                self.advance()?; // consume dot
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
        let token = self.advance()?;

        match token.kind {
            TokenKind::Number(n) => Ok(Expr::Number(n, token.span)),
            TokenKind::Integer(i) => Ok(Expr::Integer(i, token.span)),
            TokenKind::String(s) => Ok(Expr::String(s, token.span)),
            TokenKind::Character(c) => Ok(Expr::Character(c, token.span)),
            TokenKind::Boolean(b) => Ok(Expr::Boolean(b, token.span)),
            TokenKind::Identifier(s) => Ok(Expr::Symbol(s, token.span)),
            _ => {
                let context = "atom expression";
                let suggestion = match token.kind {
                    TokenKind::LeftParen => "This looks like the start of a list - atoms cannot start with '('".to_string(),
                    TokenKind::RightParen => "Unexpected closing parenthesis - check for unmatched delimiters".to_string(),
                    TokenKind::LeftBracket => "This looks like the start of a list - atoms cannot start with '['".to_string(),
                    TokenKind::RightBracket => "Unexpected closing bracket - check for unmatched delimiters".to_string(),
                    TokenKind::Dot => "Dots can only appear in dotted lists like (a . b)".to_string(),
                    TokenKind::Eof => "Unexpected end of file".to_string(),
                    _ => format!("'{}' cannot be used as an atom here", token.lexeme),
                };
                
                Err(ParseError::UnexpectedToken {
                    lexeme: token.lexeme.clone(),
                    context: context.to_string(),
                    suggestion,
                    span: SourceSpan::new(token.span.start.into(), token.span.len()),
                })
            }
        }
    }

    fn advance(&mut self) -> Result<Token, ParseError> {
        if self.peeked.is_empty() {
            self.next_token()
        } else {
            Ok(self.peeked.remove(0))
        }
    }

    fn peek(&mut self) -> Result<&Token, ParseError> {
        if self.peeked.is_empty() && !self.eof_reached {
            let token = self.next_token()?;
            self.peeked.push(token);
        }
        
        if self.peeked.is_empty() {
            // Return EOF token
            self.peeked.push(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
                span: crate::span::Span::new(0, 0),
                line: 0,
                column: 0,
            });
        }
        
        Ok(&self.peeked[0])
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        if self.eof_reached {
            return Ok(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
                span: crate::span::Span::new(0, 0),
                line: 0,
                column: 0,
            });
        }

        match self.tokens.next() {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(ParseError::LexError(err)),
            None => {
                self.eof_reached = true;
                Ok(Token {
                    kind: TokenKind::Eof,
                    lexeme: String::new(),
                    span: crate::span::Span::new(0, 0),
                    line: 0,
                    column: 0,
                })
            }
        }
    }

    fn is_at_end(&mut self) -> Result<bool, ParseError> {
        Ok(self.peek()?.kind == TokenKind::Eof)
    }

    fn check(&mut self, kind: &TokenKind) -> Result<bool, ParseError> {
        if self.is_at_end()? {
            return Ok(false);
        }
        Ok(std::mem::discriminant(&self.peek()?.kind) == std::mem::discriminant(kind))
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, ParseError> {
        if self.check(&kind)? {
            self.advance()
        } else {
            let token = self.peek()?.clone();
            let suggestion = match kind {
                TokenKind::RightParen => "Add a ')' to close the list or check for extra opening parentheses".to_string(),
                TokenKind::RightBracket => "Add a ']' to close the list or check for extra opening brackets".to_string(),
                TokenKind::LeftParen => "Expected '(' to start a list".to_string(),
                _ => format!("Expected '{}' here", Parser::token_kind_name(&kind)),
            };
            
            Err(ParseError::Expected {
                message: message.to_string(),
                lexeme: token.lexeme.clone(),
                suggestion,
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

    #[error("Unexpected token '{lexeme}' in {context}")]
    #[diagnostic(help("{suggestion}"))]
    UnexpectedToken {
        lexeme: String,
        context: String,
        suggestion: String,
        #[label("unexpected token here")]
        span: SourceSpan,
    },

    #[error("{message}")]
    #[diagnostic(help("{suggestion}"))]
    Expected {
        message: String,
        lexeme: String,
        suggestion: String,
        #[label("found '{lexeme}' instead")]
        span: SourceSpan,
    },

    #[error("Unexpected end of input while parsing {context}")]
    #[diagnostic(help("Add the missing {missing} to complete the {context}"))]
    UnexpectedEof {
        context: String,
        missing: String,
        #[label("input ends here, expected {missing}")]
        span: SourceSpan,
    },

    #[error("Mismatched delimiters: opened with '{open}' but closed with '{close}'")]
    #[diagnostic(help("Use '{expected}' to match the opening '{open}'"))]
    MismatchedDelimiters {
        open: String,
        close: String,
        expected: String,
        #[label("opened here")]
        open_span: SourceSpan,
        #[label("closed here with wrong delimiter")]
        close_span: SourceSpan,
    },
}

impl ParseError {
    pub fn span(&self) -> SourceSpan {
        match self {
            ParseError::LexError(err) => err.span(),
            ParseError::UnexpectedToken { span, .. } => *span,
            ParseError::Expected { span, .. } => *span,
            ParseError::UnexpectedEof { span, .. } => *span,
            ParseError::MismatchedDelimiters { close_span, .. } => *close_span,
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
        assert!(matches!(&exprs[1], Expr::Symbol(s, _) if s == "foo"));
        assert!(matches!(exprs[2], Expr::Boolean(true, _)));
    }

    #[test]
    fn test_parser_simple_list() {
        let exprs = Parser::parse_from_str("(+ 1 2)").unwrap();

        assert_eq!(exprs.len(), 1);
        if let Expr::List(elements, _) = &exprs[0] {
            assert_eq!(elements.len(), 3);
            assert!(matches!(&elements[0], Expr::Symbol(s, _) if s == "+"));
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
        if let Expr::List(outer, _) = &exprs[0] {
            assert_eq!(outer.len(), 2);

            if let Expr::List(inner1, _) = &outer[0] {
                assert_eq!(inner1.len(), 2);
                assert!(matches!(&inner1[0], Expr::Symbol(s, _) if s == "f"));
                assert!(matches!(inner1[1], Expr::Integer(1, _)));
            } else {
                panic!("Expected inner list, got {:?}", &outer[0]);
            }
        } else {
            panic!("Expected outer list, got {:?}", &exprs[0]);
        }
    }

    #[test]
    fn test_parser_dotted_list() {
        let exprs = Parser::parse_from_str("(a b . c)").unwrap();

        assert_eq!(exprs.len(), 1);
        if let Expr::DottedList(elements, tail, _) = &exprs[0] {
            assert_eq!(elements.len(), 2);
            assert!(matches!(&elements[0], Expr::Symbol(s, _) if s == "a"));
            assert!(matches!(&elements[1], Expr::Symbol(s, _) if s == "b"));
            assert!(matches!(&**tail, Expr::Symbol(s, _) if s == "c"));
        } else {
            panic!("Expected dotted list");
        }
    }

    #[test]
    fn test_parser_quotes() {
        let exprs = Parser::parse_from_str("'x `(a ,b ,@c)").unwrap();

        assert_eq!(exprs.len(), 2);

        if let Expr::Quote(inner, _) = &exprs[0] {
            assert!(matches!(&**inner, Expr::Symbol(s, _) if s == "x"));
        } else {
            panic!("Expected quote");
        }

        if let Expr::Quasiquote(inner, _) = &exprs[1] {
            if let Expr::List(elements, _) = &**inner {
                assert_eq!(elements.len(), 3);
                assert!(matches!(&elements[0], Expr::Symbol(s, _) if s == "a"));
                assert!(matches!(&elements[1], Expr::Unquote(_, _)));
                assert!(matches!(&elements[2], Expr::UnquoteSplicing(_, _)));
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
        if let Expr::List(elements, _) = &exprs[0] {
            assert_eq!(elements.len(), 3);
            assert!(matches!(&elements[0], Expr::Symbol(s, _) if s == "+"));
        } else {
            panic!("Expected list from brackets");
        }
    }

    #[test]
    fn test_parser_mixed_quotes() {
        let exprs = Parser::parse_from_str("'`(,a ,@b)").unwrap();

        assert_eq!(exprs.len(), 1);
        if let Expr::Quote(inner, _) = &exprs[0] {
            if let Expr::Quasiquote(inner2, _) = &**inner {
                if let Expr::List(elements, _) = &**inner2 {
                    assert_eq!(elements.len(), 2);
                    assert!(matches!(&elements[0], Expr::Unquote(_, _)));
                    assert!(matches!(&elements[1], Expr::UnquoteSplicing(_, _)));
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
        if let Expr::List(elements, _) = &exprs[0] {
            assert_eq!(elements.len(), 0);
        } else {
            panic!("Expected empty list");
        }
    }

    #[test]
    fn test_parser_string_atoms() {
        let exprs = Parser::parse_from_str(r#""hello world" #\a #t #f"#).unwrap();

        assert_eq!(exprs.len(), 4);
        assert!(matches!(&exprs[0], Expr::String(s, _) if s == "hello world"));
        assert!(matches!(exprs[1], Expr::Character('a', _)));
        assert!(matches!(exprs[2], Expr::Boolean(true, _)));
        assert!(matches!(exprs[3], Expr::Boolean(false, _)));
    }

    #[test]
    fn test_streaming_parser() {
        let exprs = Parser::parse_streaming_from_str("(+ 1 2) '(a b c)").unwrap();

        assert_eq!(exprs.len(), 2);
        if let Expr::List(elements, _) = &exprs[0] {
            assert_eq!(elements.len(), 3);
            assert!(matches!(&elements[0], Expr::Symbol(s, _) if s == "+"));
            assert!(matches!(elements[1], Expr::Integer(1, _)));
            assert!(matches!(elements[2], Expr::Integer(2, _)));
        } else {
            panic!("Expected list");
        }

        assert!(matches!(&exprs[1], Expr::Quote(_, _)));
    }

    #[test]
    fn test_streaming_vs_regular_parser() {
        let input = "'(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))";
        
        let regular = Parser::parse_from_str(input).unwrap();
        let streaming = Parser::parse_streaming_from_str(input).unwrap();
        
        assert_eq!(regular.len(), streaming.len());
        assert_eq!(regular[0], streaming[0]);
    }

    #[test]
    fn test_improved_error_messages() {
        // Test unexpected token in atom context
        let err = Parser::parse_from_str("(").unwrap_err();
        match err {
            ParseError::Expected { suggestion, .. } => {
                assert!(suggestion.contains("Add a ')' to close the list"));
            }
            _ => panic!("Expected Expected error, got {:?}", err),
        }

        // Test misplaced dot
        let err = Parser::parse_from_str(". 42").unwrap_err();
        match err {
            ParseError::UnexpectedToken { suggestion, context, .. } => {
                assert!(suggestion.contains("Dots can only appear in dotted lists"));
                assert_eq!(context, "atom expression");
            }
            _ => panic!("Expected UnexpectedToken error, got {:?}", err),
        }

        // Test unmatched closing paren
        let err = Parser::parse_from_str("42 )").unwrap_err();
        match err {
            ParseError::UnexpectedToken { suggestion, .. } => {
                assert!(suggestion.contains("Unexpected closing parenthesis"));
            }
            _ => panic!("Expected UnexpectedToken error, got {:?}", err),
        }
    }

    #[test]
    fn test_parse_with_recovery() {
        // Test recovery from simple parsing errors
        let input = "42 ) 'valid (+ 1 2)"; // Extra closing paren
        let (exprs, errors) = Parser::parse_with_recovery(input);
        
        assert!(exprs.len() >= 2); // Should recover and parse multiple expressions
        assert!(errors.len() >= 1); // Should have at least one error
        
        assert!(matches!(exprs[0], Expr::Integer(42, _)));
    }

    #[test]
    fn test_find_at_position() {
        let input = "(+ 1 (* 2 3))";
        let exprs = Parser::parse_from_str(input).unwrap();
        let expr = &exprs[0];
        
        // Position 1 should find the "+" symbol
        if let Some(found) = expr.find_at_position(1) {
            assert!(matches!(found, Expr::Symbol(s, _) if s == "+"));
        } else {
            panic!("Should find symbol at position 1");
        }
        
        // Position 5 should find the inner list (* 2 3)
        if let Some(found) = expr.find_at_position(5) {
            assert!(matches!(found, Expr::List(_, _)));
        } else {
            panic!("Should find list at position 5");
        }
    }

    #[test]
    fn test_collect_symbols() {
        let input = "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))";
        let exprs = Parser::parse_from_str(input).unwrap();
        let symbols = exprs[0].collect_symbols();
        
        assert!(symbols.contains(&"define"));
        assert!(symbols.contains(&"factorial"));
        assert!(symbols.contains(&"n"));
        assert!(symbols.contains(&"if"));
        assert!(symbols.contains(&"="));
        assert!(symbols.contains(&"*"));
        assert!(symbols.contains(&"-"));
    }
}
