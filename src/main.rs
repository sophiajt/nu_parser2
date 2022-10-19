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
    VariableDecl(NodeId, NodeId),
    Block(Vec<NodeId>),
    Garbage,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub enum ParseErrorType {
    UnexpectedToken,
    Expected(String),
}

#[derive(Debug)]
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
        self.code();
    }

    pub fn statement_or_definition(&mut self) -> NodeId {
        if self.is_keyword(b"def") {
            self.definition()
        } else {
            self.statement()
        }
    }

    pub fn statement(&mut self) -> NodeId {
        if self.is_keyword(b"let") {
            self.let_statement()
        } else {
            self.number()
        }
    }

    pub fn has_tokens(&mut self) -> bool {
        self.lexer.peek().is_some()
    }

    pub fn is_rcurly(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            })
        )
    }

    pub fn is_whitespace(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Space,
                ..
            }) | Some(Token {
                token_type: TokenType::Newline,
                ..
            })
        )
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

    pub fn code(&mut self) -> NodeId {
        let mut code_body = vec![];
        while self.has_tokens() {
            if self.is_whitespace() {
                self.skip_whitespace();
            } else if self.is_rcurly() {
                break;
            } else {
                let result = self.statement_or_definition();
                code_body.push(result);
            }
        }

        self.create_node(NodeType::Block(code_body), 0, 0)
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

    pub fn equals(&mut self) {
        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Equals,
                ..
            }) => {}
            _ => {
                self.error(ParseErrorType::Expected("equals (=)".to_string()));
            }
        }
    }

    pub fn definition(&mut self) -> NodeId {
        self.keyword(b"def");
        let definition_name = self.variable_name();

        definition_name
    }

    pub fn span_start(&self, node_id: NodeId) -> usize {
        self.delta.span_start[node_id.0]
    }

    pub fn span_end(&self, node_id: NodeId) -> usize {
        self.delta.span_end[node_id.0]
    }

    pub fn let_statement(&mut self) -> NodeId {
        self.keyword(b"let");
        self.skip_whitespace();
        let variable_node = self.variable_name();
        self.skip_whitespace();
        self.equals();
        self.skip_whitespace();
        let initializer_node = self.number();

        self.create_node(
            NodeType::VariableDecl(variable_node, initializer_node),
            self.span_start(variable_node),
            self.span_end(variable_node),
        )
    }

    pub fn variable_name(&mut self) -> NodeId {
        // TODO: add support for `$` in front of variable name

        match self.lexer.next() {
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                ..
            }) => self.create_node(NodeType::Variable, span_start, span_end),
            _ => self.error(ParseErrorType::Expected("variable name".to_string())),
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
            _ => self.error(ParseErrorType::Expected("number".to_string())),
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

        NodeId(self.delta.span_start.len() - 1 + self.delta.node_id_offset)
    }

    pub fn error(&mut self, error_type: ParseErrorType) -> NodeId {
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
            });
            self.create_node(NodeType::Garbage, span_start, span_end)
        } else {
            self.errors.push(ParseError {
                error_type,
                span: Span {
                    start: self.content_length,
                    end: self.content_length,
                },
            });
            self.create_node(NodeType::Garbage, self.content_length, self.content_length)
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
                    // keep going
                    self.lexer.next();
                }
                _ => return,
            }
        }
    }
}

fn main() {
    // let source = b"def foo(x: int) {
    //     let x = 3 + 4
    // }";
    // let source = b"301 203";
    let source = b"
        let x = 3
        let y = 403
    ";

    let span_offset = 0;

    let mut parser = Parser::new(source, span_offset, 0);

    parser.parse();

    let result = parser.delta;

    println!("Result: {:?}", result);
    println!("Errors: {:?}", parser.errors);
}
