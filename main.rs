use ruse::{Parser, Lexer, SourceFile, Span};
use miette::{NamedSource, Result};

fn main() -> Result<()> {
    println!("Ruse - R7RS Scheme Implementation");
    
    // Demo the lexer with span tracking
    let input = "(+ 1 2)";
    println!("\nLexing: {}", input);
    
    let mut lexer = Lexer::new(input);
    match lexer.tokenize() {
        Ok(tokens) => {
            for token in &tokens {
                if token.kind != ruse::TokenKind::Eof {
                    println!("  {:?} at {}", token.kind, token.span);
                }
            }
        },
        Err(e) => {
            return Err(miette::Report::new(e)
                .with_source_code(NamedSource::new("input", input)));
        },
    }
    
    // Demo the parser with spans
    println!("\nParsing: {}", input);
    match Parser::parse_from_str(input) {
        Ok(exprs) => {
            for expr in exprs {
                println!("  {} at {}", expr, expr.span());
            }
        },
        Err(e) => {
            return Err(miette::Report::new(e)
                .with_source_code(NamedSource::new("input", input)));
        },
    }
    
    // Demo error reporting with intentional errors
    println!("\nDemonstrating error reporting:");
    
    // Unterminated string error
    let bad_input1 = "\"unterminated string";
    println!("\nInput: {}", bad_input1);
    let mut lexer = Lexer::new(bad_input1);
    if let Err(e) = lexer.tokenize() {
        let report = miette::Report::new(e)
            .with_source_code(NamedSource::new("bad_input1.scm", bad_input1));
        eprintln!("{}\n", report);
    }
    
    // Invalid character error
    let bad_input2 = "(+ 1 \\ 2)";
    println!("\nInput: {}", bad_input2);
    let mut lexer = Lexer::new(bad_input2);
    if let Err(e) = lexer.tokenize() {
        let report = miette::Report::new(e)
            .with_source_code(NamedSource::new("bad_input2.scm", bad_input2));
        eprintln!("{}\n", report);
    }
    
    // More complex example
    let complex_input = "'(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))";
    println!("\nParsing complex expression: {}", complex_input);
    match Parser::parse_from_str(complex_input) {
        Ok(exprs) => {
            for expr in exprs {
                println!("  {} (span: {})", expr, expr.span());
            }
        },
        Err(e) => {
            let report = miette::Report::new(e)
                .with_source_code(NamedSource::new("complex.scm", complex_input));
            eprintln!("{}", report);
        },
    }
    
    // Demo SourceFile usage
    println!("\nDemonstrating SourceFile with multi-line input:");
    let multiline = "line1\n(+ 1\n   2)";
    let source = SourceFile::new(multiline.to_string(), "example.scm".to_string());
    
    // Show how you can get line/column information
    let span = Span::new(6, 13); // spans "(+ 1\n   2)"
    println!("Source: {}", source.name);
    println!("Content at span {}: '{}'", span, source.slice(span));
    let (line, col) = source.get_line_col(span.start);
    println!("Error would be at line {}, column {}", line, col);
    
    Ok(())
}

// Old error reporting function - now using miette
// fn demonstrate_error_reporting(input: &str, error_span: Span) { ... }
