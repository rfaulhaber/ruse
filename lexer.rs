use crate::span::{Span, SourceFile};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Delimiters
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    
    // Literals
    Number(f64),
    Integer(i64),
    String(String),
    Character(char),
    Boolean(bool),
    
    // Identifiers and symbols
    Identifier(String),
    
    // Special syntax
    Quote,          // '
    Quasiquote,     // `
    Unquote,        // ,
    UnquoteSplicing, // ,@
    Dot,            // .
    
    // Comments and whitespace (usually ignored)
    Comment(String),
    Whitespace,
    
    // End of file
    Eof,
}

pub struct Lexer {
    source: SourceFile,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            source: SourceFile::anonymous(input.to_string()),
            position: 0,
            line: 1,
            column: 1,
        }
    }
    
    pub fn with_source(source: SourceFile) -> Self {
        Self {
            source,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        
        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        
        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments();
        
        let start_line = self.line;
        let start_column = self.column;
        
        if self.is_at_end() {
            return Ok(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
                span: Span::new(self.position, self.position),
                line: start_line,
                column: start_column,
            });
        }
        
        let c = self.advance();
        
        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '\'' => TokenKind::Quote,
            '`' => TokenKind::Quasiquote,
            '.' => {
                if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                    return self.number_starting_with_dot();
                }
                TokenKind::Dot
            },
            ',' => {
                if self.peek() == Some('@') {
                    self.advance();
                    TokenKind::UnquoteSplicing
                } else {
                    TokenKind::Unquote
                }
            },
            '"' => return self.string(),
            '#' => return self.hash_syntax(),
            _ if c.is_ascii_digit() || (c == '-' && self.peek().map_or(false, |p| p.is_ascii_digit())) => {
                return self.number();
            },
            _ if self.is_identifier_start(c) => return self.identifier(),
            _ => return Err(LexError::UnexpectedCharacter {
                character: c,
                span: SourceSpan::new((self.position - c.len_utf8()).into(), c.len_utf8()),
            }),
        };
        
        let start_pos = self.position - c.len_utf8();
        Ok(Token {
            kind,
            lexeme: c.to_string(),
            span: Span::new(start_pos, self.position),
            line: start_line,
            column: start_column,
        })
    }

    fn advance(&mut self) -> char {
        let c = self.current_char().unwrap();
        self.position += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        c
    }

    fn peek(&self) -> Option<char> {
        self.source.content[self.position..].chars().next()
    }

    fn current_char(&self) -> Option<char> {
        self.source.content[self.position..].chars().next()
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.source.content.len()
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    self.advance();
                },
                Some(';') => {
                    // Skip line comment
                    while self.peek().map_or(false, |c| c != '\n') {
                        self.advance();
                    }
                },
                _ => break,
            }
        }
    }

    fn string(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let start_pos = self.position - 1; // account for opening quote
        let mut value = String::new();
        
        while let Some(c) = self.peek() {
            if c == '"' {
                self.advance(); // consume closing quote
                return Ok(Token {
                    kind: TokenKind::String(value.clone()),
                    lexeme: format!("\"{}\"", value),
                    span: Span::new(start_pos, self.position),
                    line: start_line,
                    column: start_column,
                });
            }
            
            if c == '\\' {
                self.advance(); // consume backslash
                match self.peek() {
                    Some('n') => { self.advance(); value.push('\n'); },
                    Some('t') => { self.advance(); value.push('\t'); },
                    Some('r') => { self.advance(); value.push('\r'); },
                    Some('\\') => { self.advance(); value.push('\\'); },
                    Some('"') => { self.advance(); value.push('"'); },
                    Some(c) => { 
                        self.advance(); 
                        value.push(c);
                    },
                    None => return Err(LexError::UnterminatedString {
                        span: SourceSpan::new(start_pos.into(), self.position - start_pos),
                    }),
                }
            } else {
                value.push(self.advance());
            }
        }
        
        Err(LexError::UnterminatedString {
            span: SourceSpan::new(start_pos.into(), self.position - start_pos),
        })
    }

    fn number(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let start_pos = self.position - 1;
        
        // Handle optional minus sign
        if self.source.content[start_pos..].starts_with('-') {
            // Already advanced past the minus
        }
        
        // Consume digits
        while self.peek().map_or(false, |c| c.is_ascii_digit()) {
            self.advance();
        }
        
        let mut is_float = false;
        
        // Handle decimal point
        if self.peek() == Some('.') {
            is_float = true;
            self.advance();
            while self.peek().map_or(false, |c| c.is_ascii_digit()) {
                self.advance();
            }
        }
        
        // Handle scientific notation
        if matches!(self.peek(), Some('e') | Some('E')) {
            is_float = true;
            self.advance();
            if matches!(self.peek(), Some('+') | Some('-')) {
                self.advance();
            }
            while self.peek().map_or(false, |c| c.is_ascii_digit()) {
                self.advance();
            }
        }
        
        let lexeme = &self.source.content[start_pos..self.position];
        
        let kind = if is_float {
            let value = lexeme.parse::<f64>()
                .map_err(|_| LexError::InvalidNumber {
                    number: lexeme.to_string(),
                    span: SourceSpan::new(start_pos.into(), self.position - start_pos),
                })?;
            TokenKind::Number(value)
        } else {
            let value = lexeme.parse::<i64>()
                .map_err(|_| LexError::InvalidNumber {
                    number: lexeme.to_string(),
                    span: SourceSpan::new(start_pos.into(), self.position - start_pos),
                })?;
            TokenKind::Integer(value)
        };
        
        Ok(Token {
            kind,
            lexeme: lexeme.to_string(),
            span: Span::new(start_pos, self.position),
            line: start_line,
            column: start_column,
        })
    }

    fn number_starting_with_dot(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let start_pos = self.position - 1;
        
        // Consume digits after dot
        while self.peek().map_or(false, |c| c.is_ascii_digit()) {
            self.advance();
        }
        
        let lexeme = &self.source.content[start_pos..self.position];
        let value = lexeme.parse::<f64>()
            .map_err(|_| LexError::InvalidNumber {
                    number: lexeme.to_string(),
                    span: SourceSpan::new(start_pos.into(), self.position - start_pos),
                })?;
        
        Ok(Token {
            kind: TokenKind::Number(value),
            lexeme: lexeme.to_string(),
            span: Span::new(start_pos, self.position),
            line: start_line,
            column: start_column,
        })
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let start_pos = self.position - 1;
        
        while self.peek().map_or(false, |c| self.is_identifier_subsequent(c)) {
            self.advance();
        }
        
        let lexeme = &self.source.content[start_pos..self.position];
        
        Ok(Token {
            kind: TokenKind::Identifier(lexeme.to_string()),
            lexeme: lexeme.to_string(),
            span: Span::new(start_pos, self.position),
            line: start_line,
            column: start_column,
        })
    }

    fn hash_syntax(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        
        match self.peek() {
            Some('t') => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Boolean(true),
                    lexeme: "#t".to_string(),
                    span: Span::new(self.position - 2, self.position),
                    line: start_line,
                    column: start_column,
                })
            },
            Some('f') => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Boolean(false),
                    lexeme: "#f".to_string(),
                    span: Span::new(self.position - 2, self.position),
                    line: start_line,
                    column: start_column,
                })
            },
            Some('\\') => {
                self.advance(); // consume backslash
                self.character()
            },
            Some(c) => Err(LexError::InvalidHashSyntax {
                character: c,
                span: SourceSpan::new((self.position - 1).into(), 2),
            }),
            None => Err(LexError::UnexpectedEof {
                span: SourceSpan::new(self.position.into(), 0),
            }),
        }
    }

    fn character(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 2; // account for #\
        
        match self.peek() {
            Some(c) => {
                self.advance();
                
                // Handle named characters
                let char_value = match c {
                    's' if self.source.content[self.position..].starts_with("pace") => {
                        self.position += 4; // skip "pace"
                        self.column += 4;
                        ' '
                    },
                    'n' if self.source.content[self.position..].starts_with("ewline") => {
                        self.position += 6; // skip "ewline"
                        self.column += 6;
                        '\n'
                    },
                    't' if self.source.content[self.position..].starts_with("ab") => {
                        self.position += 2; // skip "ab"
                        self.column += 2;
                        '\t'
                    },
                    _ => c,
                };
                
                let char_display = match char_value {
                    ' ' => "space".to_string(),
                    '\n' => "newline".to_string(),
                    '\t' => "tab".to_string(),
                    _ => char_value.to_string(),
                };
                
                Ok(Token {
                    kind: TokenKind::Character(char_value),
                    lexeme: format!("#\\{}", char_display),
                    span: Span::new(self.position.saturating_sub(char_display.len() + 2), self.position),
                    line: start_line,
                    column: start_column,
                })
            },
            None => Err(LexError::UnexpectedEof {
                span: SourceSpan::new(self.position.into(), 0),
            }),
        }
    }

    fn is_identifier_start(&self, c: char) -> bool {
        c.is_alphabetic() || "!$%&*+-./:<=>?@^_~".contains(c)
    }

    fn is_identifier_subsequent(&self, c: char) -> bool {
        self.is_identifier_start(c) || c.is_ascii_digit()
    }
}

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum LexError {
    #[error("Unexpected character '{character}'")]
    #[diagnostic(help("This character is not valid in Scheme source code at this position"))]
    UnexpectedCharacter {
        character: char,
        #[label("unexpected character here")]
        span: SourceSpan,
    },
    
    #[error("Unterminated string literal")]
    #[diagnostic(help("Add a closing quote or escape the newline"))]
    UnterminatedString {
        #[label("string starts here")]
        span: SourceSpan,
    },
    
    #[error("Invalid number literal '{number}'")]
    #[diagnostic(help("Check the number format - only decimal integers and floats are supported"))]
    InvalidNumber {
        number: String,
        #[label("invalid number here")]
        span: SourceSpan,
    },
    
    #[error("Invalid hash syntax '#{character}'")]
    #[diagnostic(help("Valid hash syntax includes #t, #f, #\\char"))]
    InvalidHashSyntax {
        character: char,
        #[label("invalid hash syntax here")]
        span: SourceSpan,
    },
    
    #[error("Unexpected end of file")]
    #[diagnostic(help("The input ended unexpectedly"))]
    UnexpectedEof {
        #[label("input ends here")]
        span: SourceSpan,
    },
}

impl LexError {
    pub fn span(&self) -> SourceSpan {
        match self {
            LexError::UnexpectedCharacter { span, .. } => *span,
            LexError::UnterminatedString { span, .. } => *span,
            LexError::InvalidNumber { span, .. } => *span,
            LexError::InvalidHashSyntax { span, .. } => *span,
            LexError::UnexpectedEof { span, .. } => *span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::SourceSpan;

    #[test]
    fn test_lexer_basic_tokens() {
        let mut lexer = Lexer::new("()[]");
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens.len(), 5); // 4 delimiters + EOF
        assert_eq!(tokens[0].kind, TokenKind::LeftParen);
        assert_eq!(tokens[1].kind, TokenKind::RightParen);
        assert_eq!(tokens[2].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[3].kind, TokenKind::RightBracket);
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_lexer_numbers() {
        let mut lexer = Lexer::new("42 3.14 -17 .5");
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::Integer(42)));
        assert!(matches!(tokens[1].kind, TokenKind::Number(n) if (n - 3.14).abs() < f64::EPSILON));
        assert!(matches!(tokens[2].kind, TokenKind::Integer(-17)));
        assert!(matches!(tokens[3].kind, TokenKind::Number(n) if (n - 0.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_lexer_strings() {
        let mut lexer = Lexer::new(r#""hello" "world\n""#);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::String(ref s) if s == "hello"));
        assert!(matches!(tokens[1].kind, TokenKind::String(ref s) if s == "world\n"));
    }

    #[test]
    fn test_lexer_identifiers() {
        let mut lexer = Lexer::new("foo bar-baz? +");
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::Identifier(ref s) if s == "foo"));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(ref s) if s == "bar-baz?"));
        assert!(matches!(tokens[2].kind, TokenKind::Identifier(ref s) if s == "+"));
    }

    #[test]
    fn test_lexer_booleans_and_chars() {
        let mut lexer = Lexer::new("#t #f #\\a #\\space #\\newline");
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::Boolean(true)));
        assert!(matches!(tokens[1].kind, TokenKind::Boolean(false)));
        assert!(matches!(tokens[2].kind, TokenKind::Character('a')));
        assert!(matches!(tokens[3].kind, TokenKind::Character(' ')));
        assert!(matches!(tokens[4].kind, TokenKind::Character('\n')));
    }

    #[test]
    fn test_lexer_quote_syntax() {
        let mut lexer = Lexer::new("' ` , ,@");
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].kind, TokenKind::Quote);
        assert_eq!(tokens[1].kind, TokenKind::Quasiquote);
        assert_eq!(tokens[2].kind, TokenKind::Unquote);
        assert_eq!(tokens[3].kind, TokenKind::UnquoteSplicing);
    }

    #[test]
    fn test_lexer_comments_ignored() {
        let mut lexer = Lexer::new("; this is a comment\n42 ; another comment\n");
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens.len(), 2); // integer + EOF
        assert!(matches!(tokens[0].kind, TokenKind::Integer(42)));
        assert_eq!(tokens[1].kind, TokenKind::Eof);
    }

    #[test]
    fn test_span_tracking() {
        let input = "(+ 1 2)";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens[0].span, Span::new(0, 1)); // "("
        assert_eq!(tokens[1].span, Span::new(1, 2)); // "+"
        assert_eq!(tokens[2].span, Span::new(3, 4)); // "1"
        assert_eq!(tokens[3].span, Span::new(5, 6)); // "2"
        assert_eq!(tokens[4].span, Span::new(6, 7)); // ")"
    }

    #[test]
    fn test_error_spans() {
        // Test lexical error with span (using a character that isn't valid in identifiers)
        let mut lexer = Lexer::new("\\");  // backslash by itself should cause error
        let err = lexer.tokenize().unwrap_err();
        assert_eq!(err.span(), SourceSpan::new(0.into(), 1));
        
        // Test unterminated string
        let mut lexer2 = Lexer::new("\"unterminated");
        let err2 = lexer2.tokenize().unwrap_err();
        assert_eq!(err2.span(), SourceSpan::new(0.into(), 13));
    }

    #[test]
    fn test_string_escapes() {
        let mut lexer = Lexer::new(r#""hello\nworld\t\"quoted\"""#);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::String(ref s) if s == "hello\nworld\t\"quoted\""));
    }

    #[test]
    fn test_character_tokens() {
        let mut lexer = Lexer::new("#\\newline #\\tab #\\space #\\x");
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].kind, TokenKind::Character('\n')));
        assert!(matches!(tokens[1].kind, TokenKind::Character('\t')));
        assert!(matches!(tokens[2].kind, TokenKind::Character(' ')));
        assert!(matches!(tokens[3].kind, TokenKind::Character('x')));
    }
}