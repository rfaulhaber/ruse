use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn contains(&self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub content: String,
    pub name: String,
}

impl SourceFile {
    pub fn new(content: String, name: String) -> Self {
        Self { content, name }
    }

    pub fn anonymous(content: String) -> Self {
        Self {
            content,
            name: "<anonymous>".to_string(),
        }
    }

    pub fn get_line_col(&self, pos: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (i, ch) in self.content.char_indices() {
            if i >= pos {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    pub fn get_line(&self, line_num: usize) -> Option<&str> {
        self.content.lines().nth(line_num.saturating_sub(1))
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.content[span.start..span.end.min(self.content.len())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_operations() {
        let span1 = Span::new(0, 5);
        let span2 = Span::new(3, 8);

        assert_eq!(span1.len(), 5);
        assert!(!span1.is_empty());
        assert!(span1.contains(2));
        assert!(!span1.contains(5));

        let merged = span1.merge(span2);
        assert_eq!(merged, Span::new(0, 8));
    }

    #[test]
    fn test_source_file_utilities() {
        let source = SourceFile::new("line1\nline2\nline3".to_string(), "test.scm".to_string());

        // Test line/column calculation
        assert_eq!(source.get_line_col(0), (1, 1)); // start of file
        assert_eq!(source.get_line_col(5), (1, 6)); // end of first line
        assert_eq!(source.get_line_col(6), (2, 1)); // start of second line

        // Test line extraction
        assert_eq!(source.get_line(1), Some("line1"));
        assert_eq!(source.get_line(2), Some("line2"));
        assert_eq!(source.get_line(3), Some("line3"));
        assert_eq!(source.get_line(4), None);

        // Test span slicing
        let span = Span::new(6, 11); // "line2"
        assert_eq!(source.slice(span), "line2");
    }

    #[test]
    fn test_anonymous_source_file() {
        let source = SourceFile::anonymous("test content".to_string());
        assert_eq!(source.name, "<anonymous>");
        assert_eq!(source.content, "test content");
    }
}
