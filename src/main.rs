// Ideas:
// Lex+Parse in the same struct?
// Create the delta where the delta is an SOA

mod lexer;
mod parser;

use std::io::Read;

use parser::Parser;

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
        if !parser.errors.is_empty() {
            println!("errors: {:?}", parser.errors);
        }
    }

    Ok(())
}
