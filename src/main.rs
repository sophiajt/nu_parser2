// Ideas:
// Lex+Parse in the same struct?
// Create the delta where the delta is an SOA

mod lexer;
mod parser;
mod parser_delta;
mod shell_error;

use std::io::Read;

use miette::{Diagnostic, EyreContext, MietteHandlerOpts, RgbColors, SourceOffset, SourceSpan};
use thiserror::Error;

use parser::Parser;
use shell_error::ShellErrorType;

#[derive(Diagnostic, Error)]
#[error("Error: expected {label}")]
#[diagnostic()]
struct MyError {
    #[source_code]
    src: String,

    label: String,

    #[label = "expected {label}"]
    span: SourceSpan,
}

impl std::fmt::Debug for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ansi_support = true;

        let miette_handler = MietteHandlerOpts::new()
            // For better support of terminal themes use the ANSI coloring
            .rgb_colors(RgbColors::Never)
            // If ansi support is disabled in the config disable the eye-candy
            .color(ansi_support)
            .unicode(ansi_support)
            .terminal_links(ansi_support)
            .build();

        // Ignore error to prevent format! panics. This can happen if span points at some
        // inaccessible location, for example by calling `report_error()` with wrong working set.
        let _ = miette_handler.debug(self, f);

        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    for filename in std::env::args().skip(1) {
        let span_offset = 0;
        let mut source = vec![];

        let mut f = std::fs::File::open(filename)?;
        f.read_to_end(&mut source)?;

        let mut parser = Parser::new(&source, span_offset, 0);

        parser.parse();

        let result = parser.delta;

        result.print();
        for error in parser.errors {
            match error.error_type {
                ShellErrorType::Expected(msg) => {
                    eprintln!(
                        "{:?}",
                        MyError {
                            src: String::from_utf8_lossy(&source).to_string(),
                            label: msg,
                            span: SourceSpan::new(
                                SourceOffset::from(error.span_start),
                                SourceOffset::from(error.span_end - error.span_start)
                            )
                        }
                    )
                }
            }
        }
    }

    Ok(())
}
