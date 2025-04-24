use std::{iter::Peekable, str::CharIndices};

// Type alias for the scanner's output item for better readability.
type ScanOutput<'s> = (usize, &'s str);

/// A scanner that iterates over a Scheme source string, yielding slices
/// representing potential tokens (like identifiers, literals, delimiters).
/// It skips whitespace and comments.
pub struct Scanner<'s> {
    source: &'s str,
    chars: Peekable<CharIndices<'s>>,
    current_pos: usize, // Keep track of the *byte* index of the start of the next potential token
}

impl<'s> Scanner<'s> {
    /// Creates a new Scanner for the given source string.
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            current_pos: 0,
        }
    }

    /// Advances the internal iterator and updates the current position.
    /// Returns the character and its byte position.
    #[inline]
    fn next_char(&mut self) -> Option<(usize, char)> {
        match self.chars.next() {
            Some((pos, char)) => {
                // Update current_pos to the position *after* the consumed character
                self.current_pos = pos + char.len_utf8();
                Some((pos, char))
            }
            None => None,
        }
    }

    /// Peeks the next character without consuming it.
    /// Returns the character and its byte position.
    #[inline]
    fn peek_char(&mut self) -> Option<(usize, char)> {
        self.chars.peek().map(|(pos, char)| (*pos, *char))
    }

    /// Skips whitespace characters.
    fn skip_whitespace(&mut self) {
        while let Some((_, c)) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char(); // Consume the whitespace
            } else {
                break;
            }
        }
    }

    /// Skips a line comment (starting with ';').
    fn skip_comment(&mut self) {
        // Consume the ';'
        self.next_char();
        // Consume characters until a newline or EOF
        while let Some((_, c)) = self.peek_char() {
            if c == '\n' {
                // Do not consume the newline, let skip_whitespace handle it if needed
                break;
            } else {
                self.next_char(); // Consume the comment character
            }
        }
    }

    /// Scans a string literal enclosed in double quotes.
    /// Handles escaped quotes (\").
    fn scan_string_literal(&mut self) -> Option<ScanOutput<'s>> {
        let (start_pos, _) = self.next_char()?; // Consume opening '"'

        while let Some((pos, c)) = self.peek_char() {
            match c {
                '\\' => {
                    // Handle escape sequence
                    self.next_char(); // Consume '\'
                    if let Some((_, next_c)) = self.peek_char() {
                        // Consume the escaped character (e.g., the '"' in '\"')
                        self.next_char();
                        // If we hit EOF right after backslash, loop will terminate
                    } else {
                        // Unterminated string (ends with '\')
                        break; // Exit loop, return partial string
                    }
                }
                '"' => {
                    // End of string found
                    self.next_char(); // Consume closing '"'
                                      // Slice includes the quotes
                    return self
                        .source
                        .get(start_pos..self.current_pos)
                        .map(|s| (start_pos, s));
                }
                _ => {
                    // Regular character in string
                    self.next_char(); // Consume the character
                }
            }
        }

        // If loop finishes without finding closing quote, it's an unterminated string.
        // Return the slice up to the current position (EOF).
        self.source
            .get(start_pos..self.current_pos)
            .map(|s| (start_pos, s))
    }

    /// Scans an identifier enclosed in vertical bars (|...|).
    /// Does not handle escaped bars currently (Scheme R7RS doesn't specify escapes here).
    fn scan_symbolic_identifier(&mut self) -> Option<ScanOutput<'s>> {
        let (start_pos, _) = self.next_char()?; // Consume opening '|'

        while let Some((pos, c)) = self.peek_char() {
            match c {
                '|' => {
                    // End of identifier found
                    self.next_char(); // Consume closing '|'
                                      // Slice includes the bars
                    return self
                        .source
                        .get(start_pos..self.current_pos)
                        .map(|s| (start_pos, s));
                }
                _ => {
                    // Regular character in identifier
                    self.next_char(); // Consume the character
                }
            }
        }

        // If loop finishes without finding closing bar, it's an unterminated identifier.
        // Return the slice up to the current position (EOF).
        self.source
            .get(start_pos..self.current_pos)
            .map(|s| (start_pos, s))
    }

    /// Scans a regular identifier or number (sequence of non-delimiter, non-whitespace chars).
    fn scan_identifier_or_number(&mut self) -> Option<ScanOutput<'s>> {
        let (start_pos, _) = self.next_char()?; // Consume the first character

        while let Some((_, c)) = self.peek_char() {
            if is_delimiter(c) || c.is_whitespace() {
                // Found the end of the identifier/number
                break;
            } else {
                // Continue consuming characters
                self.next_char();
            }
        }
        // Slice does *not* include the delimiter/whitespace
        self.source
            .get(start_pos..self.current_pos)
            .map(|s| (start_pos, s))
    }
}

impl<'s> Iterator for Scanner<'s> {
    type Item = ScanOutput<'s>; // Using the type alias

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // 1. Skip leading whitespace and comments
            self.skip_whitespace();
            if let Some((_, ';')) = self.peek_char() {
                self.skip_comment();
                continue; // Restart loop to handle potential whitespace after comment
            }

            // 2. Peek at the next character to decide what to scan
            match self.peek_char() {
                Some((pos, char)) => {
                    // Update current_pos to the start of the token we are about to parse
                    self.current_pos = pos;

                    match char {
                        // Single-character delimiters
                        '(' | ')' => {
                            self.next_char(); // Consume the delimiter
                                              // Return the single character slice
                            break self.source.get(pos..self.current_pos).map(|s| (pos, s));
                        }
                        // Start of a string literal
                        '"' => {
                            break self.scan_string_literal();
                        }
                        // Start of a symbolic identifier
                        '|' => {
                            break self.scan_symbolic_identifier();
                        }
                        // Start of a regular identifier or number
                        _ => {
                            // Let scan_identifier_or_number handle non-delimiter chars
                            break self.scan_identifier_or_number();
                        }
                    }
                }
                // End of input
                None => break None,
            }
        }
    }
}

/// Checks if a character is a Scheme delimiter according to R7RS section 2.1.
/// Delimiters are: ( ) " ; | whitespace
#[inline]
fn is_delimiter(c: char) -> bool {
    matches!(c, '(' | ')' | '"' | ';' | '|') || c.is_whitespace()
}

// --- Tests ---
#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to collect all scanned items
    fn scan_all(input: &str) -> Vec<(usize, &str)> {
        Scanner::new(input).collect()
    }

    #[test]
    fn test_simple_identifiers() {
        let input = "define x y";
        let tokens = scan_all(input);
        // Positions are usize now
        assert_eq!(tokens, vec![(0, "define"), (7, "x"), (9, "y"),]);
    }

    #[test]
    fn test_numbers_and_positions() {
        let input = "42 -7 3.14";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "42"), (3, "-7"), (6, "3.14"),]);
    }

    #[test]
    fn test_symbols_and_special_chars() {
        let input = "(lambda (x) (* x x))";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![
                (0, "("),
                (1, "lambda"),
                (8, "("),
                (9, "x"),
                (10, ")"),
                (12, "("),
                (13, "*"),
                (15, "x"),
                (17, "x"),
                (18, ")"),
                (19, ")"),
            ]
        );
    }

    #[test]
    fn test_escaped_identifier() {
        let input = "|two words|";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "|two words|"),]);
    }

    #[test]
    fn test_empty_escaped_identifier() {
        let input = "||";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "||")]);
    }

    #[test]
    fn test_unterminated_escaped_identifier() {
        let input = "|hello world";
        // Scanner should return the partial identifier up to EOF
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "|hello world")]);
    }

    #[test]
    fn test_complex_number() {
        let input = "3+4i 0+1i 5@1.5708";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "3+4i"), (5, "0+1i"), (10, "5@1.5708"),]);
    }

    #[test]
    fn test_booleans_and_chars() {
        let input = "#t #f #\\space #\\newline #\\x03BB"; // Added #\newline
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![
                (0, "#t"),
                (3, "#f"),
                (6, "#\\space"),
                (14, "#\\newline"),
                (24, "#\\x03BB"),
            ]
        );
    }

    #[test]
    fn test_string_with_escape() {
        // Raw string literal r#"..."# helps avoid escaping backslashes in the test code itself
        let input = r#""hello \"world\"""#;
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, r#""hello \"world\"""#),]);
    }

    #[test]
    fn test_string_with_only_escapes() {
        let input = r#""\\\"""#; // Represents the source string "\\\""
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, r#""\\\"""#)]);
    }

    #[test]
    fn test_empty_string() {
        let input = r#""""#;
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, r#""""#)]);
    }

    #[test]
    fn test_unterminated_string() {
        let input = r#""hello world"#;
        // Scanner should return the partial string up to EOF
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, input)]);
    }

    #[test]
    fn test_unterminated_string_ending_with_escape() {
        let input = r#""hello \"#;
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, r#""hello \"#)]);
    }

    #[test]
    fn test_bytevector() {
        let input = "#u8(255 128 0)";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![
                (0, "#u8"),
                (3, "("),
                (4, "255"),
                (8, "128"),
                (12, "0"),
                (13, ")"),
            ]
        );
    }

    #[test]
    fn test_mixed_whitespace_and_newlines() {
        let input = "(define\n  (square x)\n  (* x x))";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![
                (0, "("),      // (
                (1, "define"), // define
                // \n and spaces skipped
                (10, "("),      // (
                (11, "square"), // square
                (18, "x"),      // x
                (19, ")"),      // )
                // \n and spaces skipped
                (23, "("), // (
                (24, "*"), // *
                (26, "x"), // x
                (28, "x"), // x
                (29, ")"), // )
                (30, ")"), // )
            ]
        );
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let tokens = scan_all(input);
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_whitespace_only() {
        let input = "   \n\t  \r\n "; // Added carriage return
        let tokens = scan_all(input);
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_consecutive_delimiters() {
        let input = "()(())";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![(0, "("), (1, ")"), (2, "("), (3, "("), (4, ")"), (5, ")")]
        );
    }

    #[test]
    fn test_comment_at_eof() {
        let input = "hello ; this is a comment";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "hello")]); // Comment is skipped
    }

    #[test]
    fn test_comment_followed_by_token() {
        let input = "hello ; comment \n world";
        let tokens = scan_all(input);
        // Newline after comment is treated as whitespace and skipped
        assert_eq!(tokens, vec![(0, "hello"), (18, "world")]);
    }

    #[test]
    fn test_comment_inline() {
        let input = "(define x ; value\n 10)";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![(0, "("), (1, "define"), (8, "x"), (19, "10"), (21, ")")]
        );
    }

    #[test]
    fn test_multiple_comments() {
        let input = ";;;\n(define x 10) ; comment\n; another comment";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![(4, "("), (5, "define"), (12, "x"), (14, "10"), (16, ")")]
        );
    }

    #[test]
    fn test_identifier_starting_with_special_chars() {
        // Scheme allows identifiers like +, -, ..., etc.
        let input = "+ - ... two-words? *!/ <=>?";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![
                (0, "+"),
                (2, "-"),
                (4, "..."),
                (8, "two-words?"),
                (19, "*!/"),
                (23, "<=>?")
            ]
        );
    }

    #[test]
    fn test_identifier_touching_parenthesis() {
        let input = "(abc)";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "("), (1, "abc"), (4, ")")]);
    }

    #[test]
    fn test_string_touching_parenthesis() {
        let input = r#"("abc")"#;
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "("), (1, r#""abc""#), (6, ")")]);
    }

    #[test]
    fn test_identifier_touching_string() {
        let input = r#"abc"def""#;
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "abc"), (3, r#""def""#)]);
    }

    #[test]
    fn test_identifier_touching_comment() {
        let input = "abc;def";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "abc")]);
    }

    #[test]
    fn test_identifier_touching_escaped_identifier() {
        let input = "abc|def|";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "abc"), (3, "|def|")]);
    }
}
