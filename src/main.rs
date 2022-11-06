// Ideas:
// Lex+Parse in the same struct?
// Create the delta where the delta is an SOA

mod lexer;
mod parser;
mod parser_delta;
mod shell_error;

use std::io::Read;

#[cfg(test)]
use std::fs::File;
#[cfg(test)]
use std::path::PathBuf;

use rstest::rstest;

use parser::Parser;

fn main() -> std::io::Result<()> {
    for filename in std::env::args().skip(1) {
        let span_offset = 0;
        let mut source = vec![];

        let mut f = std::fs::File::open(&filename)?;
        f.read_to_end(&mut source)?;

        let mut parser = Parser::new(&source, span_offset, 0);

        parser.parse();

        let result = parser.delta;

        result.print(&mut None);
        for error in parser.errors {
            error.print(&filename, &source);
        }
    }

    Ok(())
}

#[rstest]
#[case("expression/sample6.nu", "expression/sample6.out")]
fn test_sample(#[case] sample_file: &str, #[case] out_file: &str) {
    // Parser output
    let sample_file = PathBuf::from("tests")
        .canonicalize()
        .unwrap()
        .join(sample_file);

    let span_offset = 0;
    let mut source = vec![];

    let mut f = std::fs::File::open(&sample_file).unwrap();
    f.read_to_end(&mut source).unwrap();

    let mut parser = Parser::new(&source, span_offset, 0);

    parser.parse();

    let result = parser.delta;

    let mut buffer = String::new();
    result.print(&mut Some(&mut buffer));

    // Expected output
    let out_file = PathBuf::from("tests")
        .canonicalize()
        .unwrap()
        .join(out_file);

    let mut expected = String::new();

    File::open(out_file)
        .unwrap()
        .read_to_string(&mut expected)
        .unwrap();

    assert_eq!(buffer.trim(), expected.replace("\r\n", "\n").trim());
}
