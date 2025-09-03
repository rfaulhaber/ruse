pub mod span;
pub mod lexer;
pub mod ast;
pub mod parser;

pub use span::{Span, SourceFile};
pub use lexer::{Lexer, Token, TokenKind, LexError};
pub use ast::Expr;
pub use parser::{Parser, ParseError};

// Re-export miette types for convenience
pub use miette::{SourceSpan, NamedSource, Result as MietteResult};

#[cfg(test)]
mod tests {
    use super::*;
    use miette::SourceSpan;

    /// Integration test: Full lexer -> parser pipeline
    #[test]
    fn test_full_pipeline() {
        let input = "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))";
        let exprs = Parser::parse_from_str(input).unwrap();
        
        assert_eq!(exprs.len(), 1);
        // Verify it parsed as a list with 'define' as the first symbol
        if let Expr::List(ref elements, _) = exprs[0] {
            assert!(elements.len() >= 1);
            if let Expr::Symbol(ref sym, _) = elements[0] {
                assert_eq!(sym, "define");
            } else {
                panic!("Expected 'define' symbol");
            }
        } else {
            panic!("Expected list expression");
        }
        
        // Verify span covers the entire input
        assert_eq!(exprs[0].span().start, 0);
        assert_eq!(exprs[0].span().end, input.len());
    }

    /// Integration test: Error reporting through the full pipeline
    #[test]
    fn test_error_reporting_integration() {
        // Test that parser propagates lexer errors correctly
        let result = Parser::parse_from_str("(+ 1 \\)");
        assert!(result.is_err());
        
        if let Err(ParseError::LexError(lex_err)) = result {
            // Should be an error for the backslash
            assert_eq!(lex_err.span(), SourceSpan::new(5.into(), 1));
        } else {
            panic!("Expected lexer error from parser");
        }
    }

    /// Integration test: Multi-expression parsing
    #[test]
    fn test_multiple_expressions() {
        let input = "42 'hello (+ 1 2) #t";
        let exprs = Parser::parse_from_str(input).unwrap();
        
        assert_eq!(exprs.len(), 4);
        assert!(matches!(exprs[0], Expr::Integer(42, _)));
        assert!(matches!(exprs[1], Expr::Quote(_, _)));
        assert!(matches!(exprs[2], Expr::List(_, _)));
        assert!(matches!(exprs[3], Expr::Boolean(true, _)));
    }
}
