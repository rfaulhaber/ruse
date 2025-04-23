use std::{iter::Peekable, str::CharIndices};

pub struct Scanner<'s> {
    source: &'s str,
    chars: Peekable<CharIndices<'s>>,
}

impl<'s> Iterator for Scanner<'s> {
    type Item = (u32, &'s str);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.peek() {
                Some((_, char)) => match char {
                    '(' | ')' => {
                        let (pos, _) = self.chars.next().unwrap();
                        break Some((pos as u32, &self.source[pos..=pos]));
                    }
                    ';' => {
                        self.chars.next();
                        while let Some((_, c)) = self.chars.peek() {
                            if *c == '\n' {
                                self.chars.next();
                            }
                        }
                    }
                    '|' => {
                        let (start, _) = self.chars.next().unwrap();
                        while let Some((_, c)) = self.chars.peek() {
                            if *c != '|' {
                                self.chars.next();
                            } else {
                                break;
                            }
                        }

                        let fragment = match self.chars.next() {
                            Some((end, _)) => &self.source[start..=end],
                            None => &self.source[start..],
                        };

                        break Some((start as u32, fragment));
                    }
                    '"' => {
                        let (start, _) = self.chars.next().unwrap();
                        while let Some((_, c)) = self.chars.peek() {
                            if *c == '\\' {
                                self.chars.next();
                                if let Some((_, '"')) = self.chars.peek() {
                                    self.chars.next();
                                }
                            } else if *c != '"' {
                                self.chars.next();
                            } else {
                                break;
                            }
                        }

                        let fragment = match self.chars.next() {
                            Some((end, _)) => &self.source[start..=end],
                            None => &self.source[start..],
                        };

                        break Some((start as u32, fragment));
                    }
                    c if c.is_whitespace() => {
                        self.chars.next();
                        while let Some((_, c)) = self.chars.peek() {
                            if c.is_whitespace() {
                                self.chars.next();
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {
                        let (start_pos, _) = self.chars.next().unwrap();
                        while let Some((_, c)) = self.chars.peek() {
                            if !is_delimeter(*c) {
                                self.chars.next();
                            } else {
                                break;
                            }
                        }
                        let fragment = match self.chars.peek() {
                            Some((end_pos, _)) => &self.source[start_pos..*end_pos],
                            None => &self.source[start_pos..],
                        };

                        break Some((start_pos as u32, fragment));
                    }
                },
                None => break None,
            }
        }
    }
}

impl<'s> Scanner<'s> {
    pub fn new<T: Into<&'s str>>(source: T) -> Self {
        let source_str = source.into();
        let chars = source_str.char_indices().peekable();

        Self {
            source: source_str,
            chars,
        }
    }
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

    fn scan_all(input: &str) -> Vec<(u32, &str)> {
        Scanner::new(input).collect()
    }

    #[test]
    fn test_simple_identifiers() {
        let input = "define x y";
        let tokens = scan_all(input);
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
    fn test_complex_number() {
        let input = "3+4i 0+1i 5@1.5708";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "3+4i"), (5, "0+1i"), (10, "5@1.5708"),]);
    }

    #[test]
    fn test_booleans_and_chars() {
        let input = "#t #f #\\space #\\x03BB";
        let tokens = scan_all(input);
        assert_eq!(
            tokens,
            vec![(0, "#t"), (3, "#f"), (6, "#\\space"), (14, "#\\x03BB"),]
        );
    }

    #[test]
    fn test_string_with_escape() {
        let input = "\"hello \\\"world\\\"\"";
        let tokens = scan_all(input);
        assert_eq!(tokens, vec![(0, "\"hello \\\"world\\\"\""),]);
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
                (0, "("),
                (1, "define"),
                (10, "("),
                (11, "square"),
                (18, "x"),
                (19, ")"),
                (23, "("),
                (24, "*"),
                (26, "x"),
                (28, "x"),
                (29, ")"),
                (30, ")"),
            ]
        );
    }
}
