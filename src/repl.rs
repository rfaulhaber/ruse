use miette::{NamedSource, Result};
use ruse::Parser;
use rustyline::DefaultEditor;

pub fn start_repl() -> Result<()> {
    println!("Ruse - R7RS Scheme REPL");
    println!("Type expressions to parse them. Press Ctrl-D or Ctrl-C to exit.\n");

    let mut rl =
        DefaultEditor::new().map_err(|e| miette::miette!("Failed to initialize REPL: {}", e))?;

    loop {
        let readline = rl.readline("ruse> ");
        match readline {
            Ok(line) => {
                let trimmed = line.trim();

                // Skip empty lines
                if trimmed.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(&line);

                // Parse the input
                match Parser::parse_from_str(trimmed) {
                    Ok(exprs) => {
                        println!("Parsed {} expression(s):", exprs.len());
                        for (i, expr) in exprs.iter().enumerate() {
                            println!("  [{}] {} (at {})", i, expr, expr.span());
                        }
                    }
                    Err(e) => {
                        let report = miette::Report::new(e)
                            .with_source_code(NamedSource::new("repl", trimmed.to_string()));
                        eprintln!("{:?}", report);
                    }
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                println!("Exiting...");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
