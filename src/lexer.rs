use crate::num::Num;
use crate::span::{SourceFile, Span};
use miette::{Diagnostic, SourceSpan};
use std::collections::HashMap;
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
    Number(Num),
    String(String),
    Character(char),
    Boolean(bool),

    // Identifiers and symbols
    Identifier(String),

    // Special syntax
    Quote,           // '
    Quasiquote,      // `
    Unquote,         // ,
    UnquoteSplicing, // ,@
    Dot,             // .

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
    intern_cache: HashMap<String, String>, // Simple string interning for identifiers
}

pub struct TokenIterator {
    lexer: Lexer,
}

impl std::iter::IntoIterator for Lexer {
    type Item = <TokenIterator as Iterator>::Item;

    type IntoIter = TokenIterator;

    fn into_iter(self) -> TokenIterator {
        TokenIterator { lexer: self }
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            source: SourceFile::anonymous(input.to_string()),
            position: 0,
            line: 1,
            column: 1,
            intern_cache: HashMap::new(),
        }
    }

    pub fn with_source(source: SourceFile) -> Self {
        Self {
            source,
            position: 0,
            line: 1,
            column: 1,
            intern_cache: HashMap::new(),
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
                if self.peek().is_some_and(|c| c.is_ascii_digit()) {
                    return self.number_or_identifier(self.position - 1);
                }
                // A `.` followed by identifier characters is a peculiar identifier
                // (e.g. the `...` ellipsis used by syntax-rules); a lone `.` is the
                // dotted-pair marker.
                if self
                    .peek()
                    .is_some_and(|c| self.is_identifier_subsequent(c))
                {
                    return self.identifier();
                }
                TokenKind::Dot
            }
            ',' => {
                if self.peek() == Some('@') {
                    self.advance();
                    TokenKind::UnquoteSplicing
                } else {
                    TokenKind::Unquote
                }
            }
            '"' => return self.string(),
            '#' => return self.hash_syntax(),
            // A sign starts a number when a digit or one of the `.`, `i`, `n` that open
            // `.5`, `+i` and `+nan.0` follows; otherwise it is a peculiar identifier.
            _ if c.is_ascii_digit()
                || ((c == '-' || c == '+')
                    && self
                        .peek()
                        .is_some_and(|p| p.is_ascii_digit() || ".iInN".contains(p))) =>
            {
                return self.number_or_identifier(self.position - 1);
            }
            _ if self.is_identifier_start(c) => return self.identifier(),
            _ => {
                return Err(LexError::UnexpectedCharacter {
                    character: c,
                    span: SourceSpan::new((self.position - c.len_utf8()).into(), c.len_utf8()),
                });
            }
        };

        let start_pos = self.position - c.len_utf8();
        let lexeme = Self::token_lexeme(&kind);

        Ok(Token {
            kind,
            lexeme,
            span: Span::new(start_pos, self.position),
            line: start_line,
            column: start_column,
        })
    }

    fn advance(&mut self) -> char {
        // Callers always check `is_at_end()` first, so `current_char()` is `Some` here;
        // the `'\0'` fallback only guards against a logic error and never advances past EOF.
        let Some(c) = self.current_char() else {
            return '\0';
        };
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
                }
                Some(';') => {
                    // Skip line comment
                    while self.peek().is_some_and(|c| c != '\n') {
                        self.advance();
                    }
                }
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
                    Some('n') => {
                        self.advance();
                        value.push('\n');
                    }
                    Some('t') => {
                        self.advance();
                        value.push('\t');
                    }
                    Some('r') => {
                        self.advance();
                        value.push('\r');
                    }
                    Some('\\') => {
                        self.advance();
                        value.push('\\');
                    }
                    Some('"') => {
                        self.advance();
                        value.push('"');
                    }
                    Some(c) => {
                        self.advance();
                        value.push(c);
                    }
                    None => {
                        return Err(LexError::UnterminatedString {
                            span: SourceSpan::new(start_pos.into(), self.position - start_pos),
                        });
                    }
                }
            } else {
                value.push(self.advance());
            }
        }

        Err(LexError::UnterminatedString {
            span: SourceSpan::new(start_pos.into(), self.position - start_pos),
        })
    }

    /// Lexes a run that starts like a number. Numeric syntax is too context-dependent to
    /// scan character by character (`1/3`, `+inf.0`, `1e2+3i`, `#x-ff`), so the run is cut
    /// at the next delimiter and classified whole: a number, else a peculiar identifier
    /// such as `+soup+`, else a malformed number.
    fn number_or_identifier(&mut self, start_pos: usize) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;

        while self.peek().is_some_and(|c| !self.is_delimiter(c)) {
            self.advance();
        }

        let text = self.source.content[start_pos..self.position].to_string();
        let span = Span::new(start_pos, self.position);

        if let Some(n) = Num::parse(&text, 10) {
            return Ok(Token {
                kind: TokenKind::Number(n),
                lexeme: text,
                span,
                line: start_line,
                column: start_column,
            });
        }

        if self.is_identifier(&text) {
            let interned = self.intern_string(&text);
            return Ok(Token {
                kind: TokenKind::Identifier(interned.clone()),
                lexeme: interned,
                span,
                line: start_line,
                column: start_column,
            });
        }

        Err(LexError::InvalidNumber {
            number: text,
            span: SourceSpan::new(start_pos.into(), self.position - start_pos),
        })
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_column = self.column - 1;
        let start_pos = self.position - 1;

        while self
            .peek()
            .is_some_and(|c| self.is_identifier_subsequent(c))
        {
            self.advance();
        }

        let lexeme = self.source.content[start_pos..self.position].to_string();
        let interned = self.intern_string(&lexeme);

        Ok(Token {
            kind: TokenKind::Identifier(interned.clone()),
            lexeme: interned,
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
            }
            Some('f') => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Boolean(false),
                    lexeme: "#f".to_string(),
                    span: Span::new(self.position - 2, self.position),
                    line: start_line,
                    column: start_column,
                })
            }
            Some('\\') => {
                self.advance(); // consume backslash
                self.character()
            }
            // Radix and exactness prefixes; the `#` is part of the number's text.
            Some(c) if "xXbBoOdDeEiI".contains(c) => self.number_or_identifier(self.position - 1),
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
                    }
                    'n' if self.source.content[self.position..].starts_with("ewline") => {
                        self.position += 6; // skip "ewline"
                        self.column += 6;
                        '\n'
                    }
                    't' if self.source.content[self.position..].starts_with("ab") => {
                        self.position += 2; // skip "ab"
                        self.column += 2;
                        '\t'
                    }
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
                    span: Span::new(
                        self.position.saturating_sub(char_display.len() + 2),
                        self.position,
                    ),
                    line: start_line,
                    column: start_column,
                })
            }
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

    /// Whether `text` is an R7RS identifier (§7.1.1, without the `|...|` form). This is
    /// stricter than the character set `identifier()` scans with: a malformed number such
    /// as `12abc`, `-1+` or `.5foo` has to be reported, not read as a symbol.
    fn is_identifier(&self, text: &str) -> bool {
        let is_initial = |c: char| c.is_alphabetic() || "!$%&*/:<=>?^_~".contains(c);
        let is_sign = |c: char| c == '+' || c == '-';
        let is_subsequent =
            |c: char| is_initial(c) || c.is_ascii_digit() || is_sign(c) || c == '.' || c == '@';
        let is_sign_subsequent = |c: char| is_initial(c) || is_sign(c) || c == '@';
        let is_dot_subsequent = |c: char| is_sign_subsequent(c) || c == '.';

        let mut chars = text.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if is_initial(first) {
            return chars.all(is_subsequent);
        }
        match first {
            '+' | '-' => match chars.next() {
                None => true,
                Some('.') => {
                    chars.next().is_some_and(is_dot_subsequent) && chars.all(is_subsequent)
                }
                Some(c) => is_sign_subsequent(c) && chars.all(is_subsequent),
            },
            '.' => chars.next().is_some_and(is_dot_subsequent) && chars.all(is_subsequent),
            _ => false,
        }
    }

    fn is_delimiter(&self, c: char) -> bool {
        c.is_whitespace() || "()[]\";|".contains(c)
    }

    fn intern_string(&mut self, s: &str) -> String {
        // Check if we've seen this string before
        if let Some(interned) = self.intern_cache.get(s) {
            interned.clone()
        } else {
            let owned = s.to_string();
            self.intern_cache.insert(owned.clone(), owned.clone());
            owned
        }
    }

    fn token_lexeme(kind: &TokenKind) -> String {
        match kind {
            TokenKind::LeftParen => "(".to_string(),
            TokenKind::RightParen => ")".to_string(),
            TokenKind::LeftBracket => "[".to_string(),
            TokenKind::RightBracket => "]".to_string(),
            TokenKind::Quote => "'".to_string(),
            TokenKind::Quasiquote => "`".to_string(),
            TokenKind::Dot => ".".to_string(),
            TokenKind::Unquote => ",".to_string(),
            TokenKind::UnquoteSplicing => ",@".to_string(),
            TokenKind::Eof => "".to_string(),
            _ => String::new(), // Will be set elsewhere for complex tokens
        }
    }
}

impl Iterator for TokenIterator {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next_token() {
            Ok(token) => {
                if token.kind == TokenKind::Eof {
                    None
                } else {
                    Some(Ok(token))
                }
            }
            Err(err) => Some(Err(err)),
        }
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
    #[diagnostic(help(
        "Numbers may carry #x/#o/#b/#d and #e/#i prefixes and take the form of integers, rationals (1/3), decimals with exponents (1.5e3), +inf.0, +nan.0, or rectangular complexes (1+2i)"
    ))]
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
    use crate::num::Num;
    use miette::SourceSpan;

    fn lex_kinds(input: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        tokens
            .into_iter()
            .map(|t| t.kind)
            .filter(|k| *k != TokenKind::Eof)
            .collect()
    }

    fn lex_err(input: &str) -> LexError {
        let mut lexer = Lexer::new(input);
        lexer.tokenize().unwrap_err()
    }

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
        let kinds = lex_kinds("42 3.14 -17 .5 1/3 #x-10 +inf.0 1+2i #e1.5 .5e3 -i");
        let n = |s: &str| TokenKind::Number(Num::parse(s, 10).unwrap());
        assert_eq!(
            kinds,
            vec![
                n("42"),
                n("3.14"),
                n("-17"),
                n(".5"),
                n("1/3"),
                n("#x-10"),
                n("+inf.0"),
                n("1+2i"),
                n("#e1.5"),
                n(".5e3"),
                n("-i")
            ]
        );
    }

    #[test]
    fn test_lexer_positive_signed_numbers() {
        let kinds = lex_kinds("+5 +1.5 (+ 1 2)");

        // `+5` / `+1.5` lex as numbers, not symbols...
        assert_eq!(kinds[0], TokenKind::Number(Num::int(5)));
        assert_eq!(kinds[1], TokenKind::Number(Num::flo(1.5)));
        // ...but a bare `+` (not followed by a digit) is still the addition identifier.
        assert_eq!(kinds[2], TokenKind::LeftParen);
        assert!(matches!(kinds[3], TokenKind::Identifier(ref s) if s == "+"));
    }

    #[test]
    fn numbers_win_over_peculiar_identifiers_and_delimit_correctly() {
        let kinds = lex_kinds("(+ - ... ->x +soup+ -in i 1/2)[2]");
        // + - ... ->x +soup+ -in i are identifiers; 1/2 and 2 are numbers; brackets delimit.
        assert!(matches!(kinds[1], TokenKind::Identifier(ref s) if s == "+"));
        assert!(matches!(kinds[6], TokenKind::Identifier(ref s) if s == "-in"));
        assert!(matches!(kinds[8], TokenKind::Number(_)));
        assert!(matches!(kinds[10], TokenKind::LeftBracket));
        assert!(matches!(kinds[11], TokenKind::Number(_)));
    }

    #[test]
    fn peculiar_identifiers_are_not_mistaken_for_numbers() {
        for name in [
            "+", "-", "...", "->x", "+soup+", "-in", ".foo", "+.a", "+i2", "-nan.1",
        ] {
            assert_eq!(
                lex_kinds(name),
                vec![TokenKind::Identifier(name.to_string())],
                "{name}"
            );
        }
    }

    #[test]
    fn malformed_numbers_are_one_error() {
        for src in [
            "1+",
            "1x",
            "12abc",
            "1/0",
            "#x1.5",
            "#e1e999999999",
            "1_0",
            "9223372036854775808x",
            "-1+",
            "+1x",
            ".5foo",
            "+.5x",
        ] {
            assert!(
                matches!(lex_err(src), LexError::InvalidNumber { .. }),
                "{src}"
            );
        }
    }

    #[test]
    fn number_tokens_carry_the_whole_run_as_lexeme_and_span() {
        let mut lexer = Lexer::new("(f #x-10 1/3)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[2].lexeme, "#x-10");
        assert_eq!(tokens[2].span, Span::new(3, 8));
        assert_eq!(tokens[2].column, 4);
        assert_eq!(tokens[3].lexeme, "1/3");
        assert_eq!(tokens[3].span, Span::new(9, 12));

        let err = lex_err("(f 12abc)");
        assert_eq!(err.span(), SourceSpan::new(3.into(), 5));
    }

    #[test]
    fn hash_forms_other_than_numbers_are_still_rejected() {
        assert!(matches!(
            lex_err("#z"),
            LexError::InvalidHashSyntax { character: 'z', .. }
        ));
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

        assert_eq!(tokens.len(), 2); // number + EOF
        assert_eq!(tokens[0].kind, TokenKind::Number(Num::int(42)));
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
        let mut lexer = Lexer::new("\\"); // backslash by itself should cause error
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

        assert!(
            matches!(tokens[0].kind, TokenKind::String(ref s) if s == "hello\nworld\t\"quoted\"")
        );
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

    #[test]
    fn test_lexer_ellipsis_is_one_identifier() {
        let mut lexer = Lexer::new("...");
        let tokens = lexer.tokenize().unwrap();

        // The `...` ellipsis is a single identifier token, not three `Dot`s.
        assert_eq!(tokens.len(), 2); // identifier + EOF
        assert!(matches!(tokens[0].kind, TokenKind::Identifier(ref s) if s == "..."));
    }

    #[test]
    fn test_lexer_lone_dot_still_dot() {
        let mut lexer = Lexer::new("(a . b)");
        let tokens = lexer.tokenize().unwrap();

        // A `.` delimited by whitespace remains the dotted-pair marker.
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Dot));
    }
}
