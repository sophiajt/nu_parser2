use crate::lexer::{Lexer, Token, TokenType};

use std::iter::Peekable;

#[derive(Debug)]
pub enum NodeType {
    Int,
    String,
    Name,
    Bareword,
    Variable,

    // Command-specific
    Flag,
    NamedArg,

    // Booleans
    True,
    False,

    // Operators
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    RegexMatch,
    NotRegexMatch,
    Plus,
    Append,
    Minus,
    Multiply,
    Divide,
    In,
    NotIn,
    Modulo,
    FloorDivision,
    And,
    Or,
    Pow,
    BitOr,
    BitXor,
    BitAnd,
    ShiftLeft,
    ShiftRight,
    StartsWith,
    EndsWith,

    // Statements
    Let {
        variable_name: NodeId,
        initializer: NodeId,
    },
    LetEnv {
        variable_name: NodeId,
        initializer: NodeId,
    },
    Mut {
        variable_name: NodeId,
        initializer: NodeId,
    },

    // Definitions
    Def {
        name: NodeId,
        params: NodeId,
        block: NodeId,
    },
    DefEnv {
        name: NodeId,
        params: NodeId,
        block: NodeId,
    },
    Params(Vec<NodeId>),
    Param {
        name: NodeId,
        ty: Option<NodeId>,
    },

    // Expressions
    Call {
        head: NodeId,
        args: Vec<NodeId>,
    },
    BinaryOp {
        lhs: NodeId,
        op: NodeId,
        rhs: NodeId,
    },
    List(Vec<NodeId>),
    Block(Vec<NodeId>),

    // Shell-specific
    Pipeline {
        from: NodeId,
        to: NodeId,
    },
    Redirection {
        from: NodeId,
        to: NodeId,
    },
    BashAnd {
        lhs: NodeId,
        rhs: NodeId,
    },
    BashOr {
        lhs: NodeId,
        rhs: NodeId,
    },
    If {
        condition: NodeId,
        then_block: NodeId,
        else_expression: Option<NodeId>,
    },
    Garbage,
}

impl NodeType {
    pub fn precedence(&self) -> usize {
        match self {
            NodeType::Pow => 100,
            NodeType::Multiply | NodeType::Divide | NodeType::Modulo | NodeType::FloorDivision => {
                95
            }
            NodeType::Plus | NodeType::Minus => 90,
            NodeType::ShiftLeft | NodeType::ShiftRight => 85,
            NodeType::NotRegexMatch
            | NodeType::RegexMatch
            | NodeType::StartsWith
            | NodeType::EndsWith
            | NodeType::LessThan
            | NodeType::LessThanOrEqual
            | NodeType::GreaterThan
            | NodeType::GreaterThanOrEqual
            | NodeType::Equal
            | NodeType::NotEqual
            | NodeType::In
            | NodeType::NotIn
            | NodeType::Append => 80,
            NodeType::BitAnd => 75,
            NodeType::BitXor => 70,
            NodeType::BitOr => 60,
            NodeType::And => 50,
            NodeType::Or => 40,
            _ => 0,
        }
    }
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
            NodeType::Let {
                variable_name,
                initializer,
            } => {
                println!(
                    "Let ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::LetEnv {
                variable_name,
                initializer,
            } => {
                println!(
                    "LetEnv ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::Mut {
                variable_name,
                initializer,
            } => {
                println!(
                    "Mut ({}, {}):",
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
            NodeType::Def {
                name,
                params,
                block,
            } => {
                println!(
                    "Def ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            NodeType::DefEnv {
                name,
                params,
                block,
            } => {
                println!(
                    "DefEnv ({}, {}):",
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
            NodeType::Call { head, args } => {
                println!(
                    "Call ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            NodeType::List(items) => {
                println!(
                    "List ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                for item in items {
                    self.print_helper(item, indent + 2);
                }
            }
            NodeType::Pipeline { from, to } => {
                println!(
                    "Pipeline ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(from, indent + 2);
                self.print_helper(to, indent + 2)
            }
            NodeType::Redirection { from, to } => {
                println!(
                    "Redirection ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(from, indent + 2);
                self.print_helper(to, indent + 2)
            }
            NodeType::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::BashAnd { lhs, rhs } => {
                println!(
                    "BashAnd ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::BashOr { lhs, rhs } => {
                println!(
                    "BashOr ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!(
                    "If ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0]
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorType {
    Expected(String),
}

#[derive(Debug)]
pub struct ParseError {
    error_type: ParseErrorType,
    span_start: usize,
    span_end: usize,
}

pub struct Parser<'a> {
    pub delta: ParserDelta,
    lexer: Peekable<Lexer<'a>>,
    pub errors: Vec<ParseError>,
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
        self.program();
    }

    pub fn program(&mut self) -> NodeId {
        self.code_block(false)
    }

    pub fn code_block(&mut self, in_block: bool) -> NodeId {
        let span_start = self.position();
        let mut code_body = vec![];
        while self.has_tokens() {
            if self.is_whitespace() {
                self.skip_whitespace();
            } else if (self.is_rcurly() || self.is_rparen()) && in_block {
                break;
            } else if self.is_semicolon() {
                self.lexer.next();
                continue;
            } else {
                let result = self.statement_or_definition();
                code_body.push(result);

                self.skip_space();
                if !self.is_rcurly()
                    && !self.is_rparen()
                    && !self.is_semicolon()
                    && !self.is_newline()
                    && self.has_tokens()
                {
                    self.error(ParseErrorType::Expected(
                        "new line or semicolon".to_string(),
                    ));
                }
            }
        }
        let span_end = self.position();

        self.create_node(NodeType::Block(code_body), span_start, span_end)
    }

    pub fn statement_or_definition(&mut self) -> NodeId {
        if self.is_keyword(b"def") {
            self.def()
        } else if self.is_keyword(b"def-env") {
            self.def_env()
        } else {
            self.statement()
        }
    }

    pub fn statement(&mut self) -> NodeId {
        if self.is_keyword(b"let") {
            self.let_statement()
        } else if self.is_keyword(b"let-env") {
            self.let_env_statement()
        } else if self.is_keyword(b"mut") {
            self.mut_statement()
        } else if self.is_expression() {
            self.expression()
        } else {
            self.pipeline()
        }
    }

    pub fn expression_or_pipeline(&mut self) -> NodeId {
        if self.is_expression() {
            self.expression()
        } else {
            self.pipeline()
        }
    }

    pub fn expression(&mut self) -> NodeId {
        let mut expr_stack = vec![];

        let mut last_prec = 1000000;

        // Check for special forms
        if self.is_keyword(b"if") {
            return self.if_expression();
        }

        // Otherwise assume a math expression
        let lhs = self.simple_expression();

        expr_stack.push(lhs);

        while self.has_tokens() {
            self.skip_space();
            if self.is_operator() {
                let op = self.operator();
                let op_prec = self.operator_precedence(&op);

                self.skip_space();

                let rhs = if self.is_simple_expression() {
                    self.simple_expression()
                } else {
                    self.error(ParseErrorType::Expected(
                        "complete math expression".to_string(),
                    ))
                };

                while op_prec <= last_prec && expr_stack.len() > 1 {
                    let rhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");
                    let op = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    last_prec = self.operator_precedence(&op);

                    if last_prec < op_prec {
                        expr_stack.push(op);
                        expr_stack.push(rhs);
                        break;
                    }

                    let lhs = expr_stack
                        .pop()
                        .expect("internal error: expression stack empty");

                    let (span_start, span_end) = self.spanning(&lhs, &rhs);
                    expr_stack.push(self.create_node(
                        NodeType::BinaryOp { lhs, op, rhs },
                        span_start,
                        span_end,
                    ))
                }

                expr_stack.push(op);
                expr_stack.push(rhs);

                last_prec = op_prec;
            } else {
                break;
            }
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let op = expr_stack
                .pop()
                .expect("internal error: expression stack empty");
            let lhs = expr_stack
                .pop()
                .expect("internal error: expression stack empty");

            let (span_start, span_end) = self.spanning(&lhs, &rhs);

            expr_stack.push(self.create_node(
                NodeType::BinaryOp { lhs, op, rhs },
                span_start,
                span_end,
            ))
        }

        expr_stack
            .pop()
            .expect("internal error: expression stack empty")
    }

    pub fn simple_expression(&mut self) -> NodeId {
        if self.is_lcurly() {
            self.block()
        } else if self.is_lparen() {
            self.subexpression()
        } else if self.is_lsquare() {
            self.list()
        } else if self.is_keyword(b"true") || self.is_keyword(b"false") {
            self.boolean()
        } else if self.is_variable() {
            self.variable()
        } else if self.is_string() {
            self.string()
        } else {
            self.number()
        }
    }

    pub fn operator(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type,
                contents,
                span_start,
                span_end,
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;

                match token_type {
                    TokenType::Plus => {
                        self.lexer.next();
                        self.create_node(NodeType::Plus, span_start, span_end)
                    }
                    TokenType::PlusPlus => {
                        self.lexer.next();
                        self.create_node(NodeType::Append, span_start, span_end)
                    }
                    TokenType::Dash => {
                        self.lexer.next();
                        self.create_node(NodeType::Minus, span_start, span_end)
                    }
                    TokenType::Asterisk => {
                        self.lexer.next();
                        self.create_node(NodeType::Multiply, span_start, span_end)
                    }
                    TokenType::ForwardSlash => {
                        self.lexer.next();
                        self.create_node(NodeType::Divide, span_start, span_end)
                    }
                    TokenType::ForwardSlashForwardSlash => {
                        self.lexer.next();
                        self.create_node(NodeType::FloorDivision, span_start, span_end)
                    }
                    TokenType::LessThan => {
                        self.lexer.next();
                        self.create_node(NodeType::LessThan, span_start, span_end)
                    }
                    TokenType::LessThanEqual => {
                        self.lexer.next();
                        self.create_node(NodeType::LessThanOrEqual, span_start, span_end)
                    }
                    TokenType::GreaterThan => {
                        self.lexer.next();
                        self.create_node(NodeType::GreaterThan, span_start, span_end)
                    }
                    TokenType::GreaterThanEqual => {
                        self.lexer.next();
                        self.create_node(NodeType::GreaterThanOrEqual, span_start, span_end)
                    }
                    TokenType::EqualsEquals => {
                        self.lexer.next();
                        self.create_node(NodeType::Equal, span_start, span_end)
                    }
                    TokenType::ExclamationEquals => {
                        self.lexer.next();
                        self.create_node(NodeType::NotEqual, span_start, span_end)
                    }
                    TokenType::AsteriskAsterisk => {
                        self.lexer.next();
                        self.create_node(NodeType::Pow, span_start, span_end)
                    }
                    TokenType::EqualsTilde => {
                        self.lexer.next();
                        self.create_node(NodeType::RegexMatch, span_start, span_end)
                    }
                    TokenType::ExclamationTilde => {
                        self.lexer.next();
                        self.create_node(NodeType::NotRegexMatch, span_start, span_end)
                    }
                    TokenType::AmpersandAmpersand => {
                        self.lexer.next();
                        self.create_node(NodeType::And, span_start, span_end)
                    }
                    TokenType::PipePipe => {
                        self.lexer.next();
                        self.create_node(NodeType::Or, span_start, span_end)
                    }
                    TokenType::Bareword => {
                        if contents == b"in" {
                            self.lexer.next();
                            self.create_node(NodeType::In, span_start, span_end)
                        } else if contents == b"not-in" {
                            self.lexer.next();
                            self.create_node(NodeType::NotIn, span_start, span_end)
                        } else if contents == b"bit-or" {
                            self.lexer.next();
                            self.create_node(NodeType::BitOr, span_start, span_end)
                        } else if contents == b"bit-and" {
                            self.lexer.next();
                            self.create_node(NodeType::BitAnd, span_start, span_end)
                        } else if contents == b"bit-xor" {
                            self.lexer.next();
                            self.create_node(NodeType::BitXor, span_start, span_end)
                        } else if contents == b"bit-shl" {
                            self.lexer.next();
                            self.create_node(NodeType::ShiftLeft, span_start, span_end)
                        } else if contents == b"bit-shr" {
                            self.lexer.next();
                            self.create_node(NodeType::ShiftRight, span_start, span_end)
                        } else if contents == b"starts-with" {
                            self.lexer.next();
                            self.create_node(NodeType::StartsWith, span_start, span_end)
                        } else if contents == b"ends-with" {
                            self.lexer.next();
                            self.create_node(NodeType::EndsWith, span_start, span_end)
                        } else if contents == b"and" {
                            self.lexer.next();
                            self.create_node(NodeType::And, span_start, span_end)
                        } else if contents == b"or" {
                            self.lexer.next();
                            self.create_node(NodeType::Or, span_start, span_end)
                        } else if contents == b"mod" {
                            self.lexer.next();
                            self.create_node(NodeType::Modulo, span_start, span_end)
                        } else {
                            self.error(ParseErrorType::Expected("operator".to_string()))
                        }
                    }
                    _ => self.error(ParseErrorType::Expected("operator".to_string())),
                }
            }
            _ => self.error(ParseErrorType::Expected("operator".to_string())),
        }
    }

    pub fn operator_precedence(&mut self, operator: &NodeId) -> usize {
        self.delta.node_types[operator.0].precedence()
    }

    pub fn spanning(&mut self, from: &NodeId, to: &NodeId) -> (usize, usize) {
        (self.delta.span_start[from.0], self.delta.span_end[to.0])
    }

    pub fn block(&mut self) -> NodeId {
        let span_start = self.position();

        self.lcurly();
        let output = self.code_block(true);
        self.rcurly();

        let span_end = self.position();

        self.delta.span_start[output.0] = span_start;
        self.delta.span_end[output.0] = span_end;

        output
    }

    pub fn subexpression(&mut self) -> NodeId {
        let span_start = self.position();

        // FIXME: Add line-skipping to subexpressions
        self.lparen();
        let output = self.code_block(true);
        self.rparen();

        let span_end = self.position();

        self.delta.span_start[output.0] = span_start;
        self.delta.span_end[output.0] = span_end;

        output
    }

    pub fn list(&mut self) -> NodeId {
        let span_start = self.position();

        self.lsquare();
        let mut items = vec![];
        while self.has_tokens() {
            self.skip_whitespace();
            if self.is_rsquare() {
                self.lexer.next();
                break;
            }
            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            items.push(self.simple_expression());
        }

        let span_end = self.position();

        self.create_node(NodeType::List(items), span_start, span_end)
    }

    pub fn def(&mut self) -> NodeId {
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
            NodeType::Def {
                name,
                params,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn def_env(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"def-env");

        self.skip_whitespace();
        let name = self.name();

        self.skip_whitespace();
        let params = self.params();

        self.skip_whitespace();
        let block = self.block();

        let span_end = self.position();

        self.create_node(
            NodeType::DefEnv {
                name,
                params,
                block,
            },
            span_start,
            span_end,
        )
    }

    pub fn let_statement(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"let");

        self.skip_whitespace();
        let variable_name = self.variable();

        self.skip_whitespace();
        self.equals();

        self.skip_whitespace();
        let initializer = self.expression_or_pipeline();

        let span_end = self.position();

        self.create_node(
            NodeType::Let {
                variable_name,
                initializer,
            },
            span_start,
            span_end,
        )
    }

    pub fn let_env_statement(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"let-env");

        self.skip_whitespace();
        let variable_name = self.variable();

        self.skip_whitespace();
        self.equals();

        self.skip_whitespace();
        let initializer = self.expression_or_pipeline();

        let span_end = self.position();

        self.create_node(
            NodeType::LetEnv {
                variable_name,
                initializer,
            },
            span_start,
            span_end,
        )
    }

    pub fn mut_statement(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"mut");

        self.skip_whitespace();
        let variable_name = self.variable();

        self.skip_whitespace();
        self.equals();

        self.skip_whitespace();
        let initializer = self.expression_or_pipeline();

        let span_end = self.position();

        self.create_node(
            NodeType::Mut {
                variable_name,
                initializer,
            },
            span_start,
            span_end,
        )
    }

    pub fn if_expression(&mut self) -> NodeId {
        let mut else_expression = None;

        let span_start = self.position();

        self.keyword(b"if");

        self.skip_whitespace();
        let condition = self.expression();

        self.skip_whitespace();
        let then_block = self.block();

        self.skip_whitespace();
        if self.is_keyword(b"else") {
            self.lexer.next();
            self.skip_whitespace();
            else_expression = Some(self.expression());
        }

        let span_end = self.position();

        self.create_node(
            NodeType::If {
                condition,
                then_block,
                else_expression,
            },
            span_start,
            span_end,
        )
    }

    pub fn pipeline(&mut self) -> NodeId {
        let span_start = self.position();
        let mut from = self.call();

        while self.has_tokens() {
            self.skip_space();
            if self.is_pipe() {
                self.lexer.next();
                self.skip_space();
                let to = self.call();
                let span_end = self.position();

                from = self.create_node(NodeType::Pipeline { from, to }, span_start, span_end)
            } else if self.is_greater_than() {
                self.lexer.next();
                self.skip_space();
                let to = self.bareword();
                let span_end = self.position();

                from = self.create_node(NodeType::Redirection { from, to }, span_start, span_end)
            } else if self.is_double_ampersand() {
                self.lexer.next();
                self.skip_space();
                let rhs = self.call();
                let span_end = self.position();

                from = self.create_node(NodeType::BashAnd { lhs: from, rhs }, span_start, span_end)
            } else if self.is_double_pipe() {
                self.lexer.next();
                self.skip_space();
                let rhs = self.call();
                let span_end = self.position();

                from = self.create_node(NodeType::BashOr { lhs: from, rhs }, span_start, span_end)
            } else if self.is_rparen() || self.is_newline() || self.is_rcurly() {
                break;
            } else {
                self.error(ParseErrorType::Expected("pipeline elements".to_string()));
            }
        }

        from
    }

    pub fn call(&mut self) -> NodeId {
        let span_start = self.position();
        let head = self.name();
        self.skip_space();
        if self.is_lparen() {
            // Traditional call syntax
            // eg) foo(2)
            let args = self.traditional_args();
            let span_end = self.position();
            self.create_node(NodeType::Call { head, args }, span_start, span_end)
        } else {
            // Command call syntax
            // eg) foo 2
            let args = self.command_args();
            let span_end = self.position();
            self.create_node(NodeType::Call { head, args }, span_start, span_end)
        }
    }

    pub fn bareword(&mut self) -> NodeId {
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
                self.create_node(NodeType::Bareword, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("bare word".to_string())),
        }
    }

    pub fn traditional_args(&mut self) -> Vec<NodeId> {
        let mut args = vec![];

        if self.is_lparen() {
            self.lexer.next();
        } else {
            args.push(self.error(ParseErrorType::Expected("Left paren '('".to_string())));
        }

        while self.has_tokens() {
            self.skip_whitespace();
            if self.is_rparen() {
                self.lexer.next();
                break;
            }

            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            let span_start = self.position();
            // Parse param
            if self.is_name() {
                // Possibly named param
                let head = self.name();
                if self.is_colon() {
                    // Named param
                    let span_end = self.position();
                    self.lexer.next();
                    args.push(self.create_node(NodeType::NamedArg, span_start, span_end))
                } else {
                    // Traditional call
                    let call_args = self.traditional_args();
                    let span_end = self.position();
                    args.push(self.create_node(
                        NodeType::Call {
                            head,
                            args: call_args,
                        },
                        span_start,
                        span_end,
                    ))
                }
            } else {
                let expr = self.simple_expression();
                args.push(expr)
            }
        }

        args
    }

    pub fn command_args(&mut self) -> Vec<NodeId> {
        let mut args = vec![];
        while self.has_tokens() {
            self.skip_space();

            if self.is_semicolon()
                || self.is_pipe()
                || self.is_rcurly()
                || self.is_newline()
                || self.is_greater_than()
                || self.is_double_ampersand()
                || self.is_double_pipe()
            {
                break;
            }

            if self.is_dash() {
                // Flag
                args.push(self.flag())
            } else {
                // Arg
                args.push(self.simple_expression())
            }
        }

        args
    }

    pub fn flag(&mut self) -> NodeId {
        let span_start = self.position();

        if self.is_dash() {
            self.lexer.next();
        }

        if self.is_dash() {
            self.lexer.next();
        }

        if self.has_tokens() {
            // For now, let's just parse the flag name
            // In the future, we may teach it to parse `=` and the following value
            self.lexer.next();
        }

        let span_end = self.position();

        self.create_node(NodeType::Flag, span_start, span_end)
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
        while self.has_tokens() {
            if self.is_rparen() || self.is_rsquare() {
                break;
            }

            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            // Parse param
            let span_start = self.position();
            let name = self.variable();
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

    pub fn is_operator(&mut self) -> bool {
        match self.lexer.peek() {
            Some(Token {
                token_type,
                contents,
                ..
            }) => match token_type {
                TokenType::Asterisk
                | TokenType::AsteriskAsterisk
                | TokenType::Dash
                | TokenType::EqualsEquals
                | TokenType::EqualsTilde
                | TokenType::ExclamationEquals
                | TokenType::ExclamationTilde
                | TokenType::ForwardSlash
                | TokenType::ForwardSlashForwardSlash
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::Plus
                | TokenType::PlusPlus
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual
                | TokenType::AmpersandAmpersand
                | TokenType::PipePipe => true,

                TokenType::Bareword if contents == b"in" => true,
                TokenType::Bareword if contents == b"not-in" => true,
                TokenType::Bareword if contents == b"starts-with" => true,
                TokenType::Bareword if contents == b"ends-with" => true,
                TokenType::Bareword if contents == b"bit-or" => true,
                TokenType::Bareword if contents == b"bit-xor" => true,
                TokenType::Bareword if contents == b"bit-and" => true,
                TokenType::Bareword if contents == b"bit-shl" => true,
                TokenType::Bareword if contents == b"bit-shr" => true,
                TokenType::Bareword if contents == b"and" => true,
                TokenType::Bareword if contents == b"or" => true,
                TokenType::Bareword if contents == b"mod" => true,

                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_comma(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Comma,
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

    pub fn is_less_than(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::LessThan,
                ..
            })
        )
    }

    pub fn is_greater_than(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::GreaterThan,
                ..
            })
        )
    }

    pub fn is_pipe(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Pipe,
                ..
            })
        )
    }

    pub fn is_double_pipe(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::PipePipe,
                ..
            })
        )
    }

    pub fn is_double_ampersand(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::AmpersandAmpersand,
                ..
            })
        )
    }

    pub fn is_dash(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Dash,
                ..
            })
        )
    }

    pub fn is_semicolon(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Semicolon,
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

    pub fn is_space(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Space,
                ..
            })
        )
    }

    pub fn is_newline(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
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

    pub fn is_variable(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Variable,
                ..
            })
        )
    }

    pub fn is_string(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::String,
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

    pub fn is_name(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Bareword,
                ..
            })
        )
    }

    pub fn is_expression(&mut self) -> bool {
        if self.is_simple_expression() {
            return true;
        }

        matches!(self.lexer.peek(), Some(Token {
            token_type: TokenType::Bareword,
            contents,
            ..
        }) if contents == b"if")
    }

    pub fn is_simple_expression(&mut self) -> bool {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Number,
                ..
            })
            | Some(Token {
                token_type: TokenType::String,
                ..
            })
            | Some(Token {
                token_type: TokenType::LCurly,
                ..
            })
            | Some(Token {
                token_type: TokenType::LSquare,
                ..
            })
            | Some(Token {
                token_type: TokenType::LParen,
                ..
            })
            | Some(Token {
                token_type: TokenType::Variable,
                ..
            }) => true,
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == b"true" => true,
            Some(Token {
                token_type: TokenType::Bareword,
                contents,
                ..
            }) if contents == b"false" => true,
            _ => false,
        }
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

    pub fn boolean(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                contents,
            }) if contents == b"true" => {
                let span_start = *span_start;
                let span_end = *span_end;

                self.lexer.next();
                self.create_node(NodeType::True, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                contents,
            }) if contents == b"false" => {
                let span_start = *span_start;
                let span_end = *span_end;

                self.lexer.next();
                self.create_node(NodeType::False, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("boolean".to_string())),
        }
    }

    pub fn variable(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Variable,
                span_start,
                span_end,
                ..
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;

                self.lexer.next();
                self.create_node(NodeType::Variable, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                ..
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;

                self.lexer.next();
                self.create_node(NodeType::Variable, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("variable".to_string())),
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

    pub fn string(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::String,
                span_start,
                span_end,
                ..
            }) => {
                let span_start = *span_start;
                let span_end = *span_end;

                self.lexer.next();
                self.create_node(NodeType::String, span_start, span_end)
            }
            _ => self.error(ParseErrorType::Expected("string".to_string())),
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
                span_start,
                span_end,
            });
            self.create_node(NodeType::Garbage, span_start, span_end)
        } else {
            self.errors.push(ParseError {
                error_type,
                span_start: self.content_length,
                span_end: self.content_length,
            });
            self.create_node(NodeType::Garbage, self.content_length, self.content_length)
        }
    }

    pub fn skip_space(&mut self) {
        loop {
            match self.lexer.peek() {
                Some(Token {
                    token_type: TokenType::Space,
                    ..
                }) => {
                    // keep going
                    self.lexer.next();
                }
                _ => return,
            }
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
