// Ideas:
// Lex+Parse in the same struct?
// Create the delta where the delta is an SOA

mod lexer;

use lexer::{Lexer, Token, TokenType};

use std::iter::Peekable;

#[derive(Debug)]
enum NodeType {
    Int,
    Variable,
    Garbage,
}

#[derive(Debug)]
pub struct NodeId(usize);

#[derive(Debug)]
pub struct ParserDelta {
    node_id_offset: usize,
    span_start: Vec<usize>,
    span_end: Vec<usize>,
    node_types: Vec<NodeType>,
}

impl ParserDelta {
    pub fn new(node_id_offset: usize) -> Self {
        Self {
            node_id_offset,
            span_start: vec![],
            span_end: vec![],
            node_types: vec![],
        }
    }
}

pub struct Span {
    start: usize,
    end: usize,
}

pub enum ParseErrorType {
    UnexpectedToken,
    Expected(String),
}

pub struct ParseError {
    error_type: ParseErrorType,
    span: Span,
}

struct Parser<'a> {
    delta: ParserDelta,
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<ParseError>,
    content_length: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a [u8], span_offset: usize, node_id_offset: usize) -> Self {
        let content_length = source.len();
        Self {
            delta: ParserDelta::new(node_id_offset),
            lexer: Lexer::new(source, span_offset).peekable(),
            errors: vec![],
            content_length,
        }
    }

    pub fn parse(&mut self) {
        self.skip_whitespace();

        self.statement_or_definition();
    }

    pub fn statement_or_definition(&mut self) {
        if self.is_keyword(b"def") {
            self.definition();
        } else {
            self.statement();
        }
    }

    pub fn statement(&mut self) {
        if self.is_keyword(b"let") {
            self.let_statement();
        } else {
            self.number();
        }
    }

    pub fn is_number(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
        )
    }

    pub fn is_keyword(&mut self, keyword: &[u8]) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == &keyword
        )
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == keyword => {}
            _ => {
                self.error(ParseErrorType::Expected(
                    String::from_utf8_lossy(keyword).to_string(),
                ));
            }
        }
    }

    pub fn symbol(&mut self, symbol: &[u8]) {
        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == symbol => {}
            _ => {
                self.error(ParseErrorType::Expected(
                    String::from_utf8_lossy(symbol).to_string(),
                ));
            }
        }
    }

    pub fn definition(&mut self) {
        self.keyword(b"def");
    }

    pub fn let_statement(&mut self) {
        self.keyword(b"let");
        self.skip_whitespace();
        let variable_node_id = self.variable_name();
        self.skip_whitespace();
        self.symbol(b"=");
        self.skip_whitespace();
        let init_node_id = self.number();
        println!("var: {:?} init: {:?}", variable_node_id, init_node_id);
    }

    pub fn variable_name(&mut self) -> Option<NodeId> {
        // TODO: add support for `$` in front of variable name

        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                ..
            }) => Some(self.create_node(NodeType::Variable, span_start, span_end)),
            _ => {
                self.error(ParseErrorType::Expected("variable name".to_string()));
                None
            }
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();

                self.create_node(NodeType::Int, span_start, span_end)
            }
            _ => panic!("TODO: add errors"),
        }
    }

    pub fn create_node(
        &mut self,
        node_type: NodeType,
        span_start: usize,
        span_end: usize,
    ) -> NodeId {
        self.delta.span_start.push(span_start);
        self.delta.span_end.push(span_end);
        self.delta.node_types.push(node_type);

        NodeId(self.delta.span_start.len() + self.delta.node_id_offset)
    }

    pub fn error(&mut self, error_type: ParseErrorType) {
        if let Some(Token {
            span_start,
            span_end,
            ..
        }) = self.lexer.next()
        {
            self.errors.push(ParseError {
                error_type,
                span: Span {
                    start: span_start,
                    end: span_end,
                },
            })
        } else {
            self.errors.push(ParseError {
                error_type,
                span: Span {
                    start: self.content_length,
                    end: self.content_length,
                },
            })
        }
    }

    pub fn skip_whitespace(&mut self) {
        loop {
            match self.lexer.peek() {
                Some(Token {
                    token_type: TokenType::Space,
                    ..
                })
                | Some(Token {
                    token_type: TokenType::Newline,
                    ..
                }) => {
                    self.lexer.next();
                    // keep going
                }
                _ => return,
            }
        }
    }
}

pub fn parse(source: &[u8], span_offset: usize) -> ParserDelta {
    let mut parser = Parser::new(source, span_offset, 0);

    parser.parse();

    parser.delta
}

fn main() {
    // let source = b"def foo(x: int) {
    //     let x = 3 + 4
    // }";
    // let source = b"301 203";
    let source = b"let x = 3";

    let span_offset = 0;

    let result = parse(source, span_offset);

    println!("Result: {:?}", result)
}
