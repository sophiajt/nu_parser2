// Ideas:
// Lex+Parse in the same struct?
// Create the delta where the delta is an SOA

mod lexer;

use lexer::{Lexer, Token, TokenType};

use std::iter::Peekable;

#[derive(Debug)]
enum NodeType {
    Int,
    Name,
    Variable,
    VariableDecl {
        variable_name: NodeId,
        initializer: NodeId,
    },
    Definition {
        name: NodeId,
        params: NodeId,
        block: NodeId,
    },
    Params(Vec<NodeId>),
    Param {
        name: NodeId,
        ty: Option<NodeId>,
    },

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

    pub fn print(&self) {
        if self.node_types.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.node_types.len() - 1), 0)
        }
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize) {
        for _ in 0..indent {
            print!(" ")
        }

        match &self.node_types[node_id.0] {
            NodeType::Int => {
                println!(
                    "Int ({}, {})",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
            }
            NodeType::Variable => {
                println!(
                    "Variable ({}, {})",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
            }
            NodeType::Name => {
                println!(
                    "Name ({}, {})",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
            }
            NodeType::VariableDecl {
                variable_name,
                initializer,
            } => {
                println!(
                    "Variable Decl ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::Param { name, ty } => {
                println!(
                    "Param ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            NodeType::Definition {
                name,
                params,
                block,
            } => {
                println!(
                    "Definition ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            NodeType::Block(nodes) => {
                println!(
                    "Block ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                if nodes.is_empty() {
                    println!(" <empty>");
                } else {
                    println!();
                }

                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Garbage => {
                println!(
                    "Garbage ({}, {})",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
            }
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

    fn position(&mut self) -> usize {
        if let Some(Token { span_start, .. }) = self.lexer.peek() {
            *span_start
        } else {
            self.content_length
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
        } else if self.is_lcurly() {
            self.block()
        } else {
            self.number()
        }
    }

    pub fn block(&mut self) -> NodeId {
        let span_start = self.position();

        self.lcurly();
        let output = self.code();
        self.rcurly();

        let span_end = self.position();

        self.delta.span_start[output.0] = span_start;
        self.delta.span_end[output.0] = span_end;

        output
    }

    pub fn code(&mut self) -> NodeId {
        let span_start = self.position();
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
        let span_end = self.position();

        self.create_node(NodeType::Block(code_body), span_start, span_end)
    }

    pub fn definition(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"def");

        self.skip_whitespace();
        let name = self.name();

        self.skip_whitespace();
        let params = self.params();

        self.skip_whitespace();
        let block = self.block();

        let span_end = self.position();

        self.create_node(
            NodeType::Definition {
                name,
                params,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn span_start(&self, node_id: NodeId) -> usize {
        self.delta.span_start[node_id.0]
    }

    pub fn span_end(&self, node_id: NodeId) -> usize {
        self.delta.span_end[node_id.0]
    }

    pub fn let_statement(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"let");

        self.skip_whitespace();
        let variable_name = self.variable_name();

        self.skip_whitespace();
        self.equals();

        self.skip_whitespace();
        let initializer = self.number();

        let span_end = self.position();

        self.create_node(
            NodeType::VariableDecl {
                variable_name,
                initializer,
            },
            span_start,
            span_end,
        )
    }

    pub fn variable_name(&mut self) -> NodeId {
        // TODO: add support for `$` in front of variable name

        let span_start = self.position();

        // Skip the dollar sign if it's there
        if self.is_dollar() {
            self.lexer.next();
        }

        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Bareword,
                span_end,
                ..
            }) => {
                let span_end = *span_end;
                self.lexer.next();
                self.create_node(NodeType::Variable, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("variable name".to_string())),
        }
    }

    pub fn name(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                ..
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;
                self.lexer.next();
                self.create_node(NodeType::Name, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("name".to_string())),
        }
    }

    pub fn params(&mut self) -> NodeId {
        let span_start = self.position();
        let param_list = if self.is_lparen() {
            self.lparen();
            let output = self.param_list();
            self.rparen();

            output
        } else {
            self.lsquare();
            let output = self.param_list();
            self.rsquare();

            output
        };

        let span_end = self.position();

        self.create_node(NodeType::Params(param_list), span_start, span_end)
    }

    pub fn param_list(&mut self) -> Vec<NodeId> {
        let mut params = vec![];
        loop {
            if self.is_rparen() || self.is_rsquare() {
                break;
            }

            // Parse param
            let span_start = self.position();
            let name = self.variable_name();
            self.skip_whitespace();
            if self.is_colon() {
                // Optional type
                self.colon();

                self.skip_whitespace();
                let ty = self.name();

                let span_end = self.position();

                params.push(self.create_node(
                    NodeType::Param { name, ty: Some(ty) },
                    span_start,
                    span_end,
                ))
            } else {
                let span_end = self.position();
                params.push(self.create_node(
                    NodeType::Param { name, ty: None },
                    span_start,
                    span_end,
                ))
            }
        }

        params
    }

    pub fn keyword(&mut self, keyword: &[u8]) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == &keyword => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected(
                    String::from_utf8_lossy(keyword).to_string(),
                ));
            }
        }
    }

    pub fn equals(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Equals,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("equals '='".to_string()));
            }
        }
    }

    pub fn colon(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Colon,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("colon ':'".to_string()));
            }
        }
    }

    pub fn has_tokens(&mut self) -> bool {
        self.lexer.peek().is_some()
    }

    pub fn is_colon(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Colon,
                ..
            })
        )
    }

    pub fn is_dollar(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Dollar,
                ..
            })
        )
    }

    pub fn is_lcurly(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
        )
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

    pub fn is_lparen(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LParen,
                ..
            })
        )
    }

    pub fn is_rparen(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RParen,
                ..
            })
        )
    }

    pub fn is_lsquare(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
        )
    }

    pub fn is_rsquare(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::RSquare,
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

    pub fn lsquare(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::LSquare,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("left brace '['".to_string()));
            }
        }
    }

    pub fn rsquare(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::RSquare,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("right brace ']'".to_string()));
            }
        }
    }

    pub fn lparen(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::LParen,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("left paren '('".to_string()));
            }
        }
    }

    pub fn rparen(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::RParen,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("right paren ')'".to_string()));
            }
        }
    }

    pub fn lcurly(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::LCurly,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("left bracket '{'".to_string()));
            }
        }
    }

    pub fn rcurly(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::RCurly,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ParseErrorType::Expected("right bracket '}'".to_string()));
            }
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
                ..
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;

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
    let source = b"def foo(x: int) {
        let x = 3
    }";
    // let source = b"301 203";
    // let source = b"
    //     let x = 3
    //     let $y = 403
    // ";
    // let source = b"{3}";

    let span_offset = 0;

    let mut parser = Parser::new(source, span_offset, 0);

    parser.parse();

    let result = parser.delta;

    result.print();
    if !parser.errors.is_empty() {
        println!("errors: {:?}", parser.errors);
    }
}
