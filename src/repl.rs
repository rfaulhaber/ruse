use miette::{NamedSource, Result};
use ruse::rt::write::{Style, value_to_string};
use ruse::{Parser, Vm};
use rustyline::DefaultEditor;

pub fn start_repl() -> Result<()> {
    println!("Ruse - R7RS Scheme REPL");
    println!("Type expressions to evaluate them. Press Ctrl-D or Ctrl-C to exit.\n");

    let mut rl =
        DefaultEditor::new().map_err(|e| miette::miette!("Failed to initialize REPL: {}", e))?;
    let mut vm = Vm::new();

    loop {
        let readline = rl.readline("ruse> ");
        match readline {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(&line);

                // Parse first so a syntax error reports without evaluating anything.
                let exprs = match Parser::parse_from_str(trimmed) {
                    Ok(exprs) => exprs,
                    Err(e) => {
                        report(e, trimmed);
                        continue;
                    }
                };
                for expr in &exprs {
                    match vm.eval_expr(expr) {
                        // The unspecified value is what definitions and effects return;
                        // echoing it would just be noise.
                        Ok(v) if v.is_unspecified() => {}
                        Ok(v) => println!("{}", value_to_string(vm.heap(), v, Style::Write)),
                        Err(e) => {
                            report(e, trimmed);
                            break;
                        }
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

fn report(e: impl miette::Diagnostic + Send + Sync + 'static, source: &str) {
    let report =
        miette::Report::new(e).with_source_code(NamedSource::new("repl", source.to_string()));
    eprintln!("{:?}", report);
}
