use crate::span::Span;

#[derive(Debug)]
pub struct LineMap<'s> {
    source: &'s str,
    lines: Vec<u32>,
}

impl<'s> LineMap<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut lines: Vec<u32> = source
            .char_indices()
            .filter(|(_, ch)| *ch == '\n')
            .map(|(pos, _)| (pos + 1) as u32)
            .collect();

        lines.insert(0, 0);

        Self { source, lines }
    }

    pub fn line_col(&self, span: Span) -> (u32, u32) {
        let line = self
            .lines
            .iter()
            .enumerate()
            .map(|(line, start)| (line + 1, start))
            .find(|(_, start)| **start >= span.position())
            .unwrap();

        let col = (*line.1 - span.position()) + 1;

        (line.0 as u32, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generates_line_map() {
        let input = "one\ntwo\nthree";

        let line_map = LineMap::new(input);
        let one_span = Span::new("one", 0, 2);
        let two_span = Span::new("two", 4, 3);
        let three_span = Span::new("three", 8, 12);

        assert_eq!(line_map.line_col(one_span), (1, 1));
        assert_eq!(line_map.line_col(two_span), (2, 1));
        assert_eq!(line_map.line_col(three_span), (3, 1));
    }
}
