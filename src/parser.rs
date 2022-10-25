use crate::{
    lexer::{Lexer, Token, TokenType},
    parser_delta::ParserDelta,
    shell_error::{ShellError, ShellErrorType},
};

#[derive(Debug)]
pub enum Unit {
    // Filesize units: metric
    Byte,
    Kilobyte,
    Megabyte,
    Gigabyte,
    Terabyte,
    Petabyte,
    Exabyte,
    Zettabyte,

    // Filesize units: ISO/IEC 80000
    Kibibyte,
    Mebibyte,
    Gibibyte,
    Tebibyte,
    Pebibyte,
    Exbibyte,
    Zebibyte,

    // Duration units
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Week,
}

#[derive(Debug)]
pub enum NodeType {
    Int,
    Float,
    Unit(Unit),
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
    Closure {
        params: NodeId,
        block: NodeId,
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
    Range {
        lhs: NodeId,
        rhs: NodeId,
    },
    Table(Vec<NodeId>), // First element is headers, remainder are cells
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
    ColumnPath {
        head: NodeId,
        path: NodeId,
    },
    RowPath {
        head: NodeId,
        path: NodeId,
    },
    Where(NodeId),
    Garbage,

    // REPL Bashisms
    BangBang,
    BangDollar,
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
pub struct NodeId(pub usize);

pub struct Parser<'a> {
    pub delta: ParserDelta,
    lexer: Lexer<'a>,
    pub errors: Vec<ShellError>,
    content_length: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a [u8], span_offset: usize, node_id_offset: usize) -> Self {
        let content_length = source.len();
        Self {
            delta: ParserDelta::new(node_id_offset),
            lexer: Lexer::new(source, span_offset),
            errors: vec![],
            content_length,
        }
    }

    fn position(&mut self) -> usize {
        if let Some(Token { span_start, .. }) = self.lexer.peek() {
            span_start
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
            if self.is_whitespace_or_comment() {
                self.skip_whitespace_and_comments();
            } else if (self.is_rcurly() || self.is_rparen()) && in_block {
                break;
            } else if self.is_semicolon() {
                self.lexer.next();
                continue;
            } else if self.is_repl_bashism() {
                let result = self.repl_bashism();
                code_body.push(result);
            } else {
                let result = self.statement_or_definition();
                code_body.push(result);

                self.skip_space();
                if !self.is_rcurly()
                    && !self.is_rparen()
                    && !self.is_semicolon()
                    && !self.is_newline()
                    && !self.is_comment()
                    && self.has_tokens()
                {
                    self.error(ShellErrorType::Expected(
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

    pub fn expression_or_call(&mut self) -> NodeId {
        if self.is_expression() {
            self.expression()
        } else {
            self.call()
        }
    }

    pub fn expression(&mut self) -> NodeId {
        let mut expr_stack = vec![];

        let mut last_prec = 1000000;

        // Check for special forms
        if self.is_keyword(b"if") {
            return self.if_expression();
        } else if self.is_keyword(b"where") {
            return self.where_expression();
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
                    self.error(ShellErrorType::Incomplete("math expression".to_string()))
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
        let span_start = self.position();

        let expr = if self.is_lcurly() {
            self.block_or_closure()
        } else if self.is_lparen() {
            self.subexpression()
        } else if self.is_lsquare() {
            self.list_or_table()
        } else if self.is_keyword(b"true") || self.is_keyword(b"false") {
            self.boolean()
        } else if self.is_variable() {
            self.variable()
        } else if self.is_string() {
            self.string()
        } else if self.is_number() {
            self.number()
        } else {
            let bare_string = self.bareword();
            self.delta.node_types[bare_string.0] = NodeType::String;
            return bare_string;
        };

        if self.is_dot() {
            self.cell_path(expr, span_start)
        } else if self.is_dotdot() {
            // Range
            self.lexer.next();

            let rhs = self.simple_expression();
            let span_end = self.position();

            self.create_node(NodeType::Range { lhs: expr, rhs }, span_start, span_end)
        } else {
            expr
        }
    }

    pub fn operator(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type,
                contents,
                span_start,
                span_end,
            }) => match token_type {
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
                        self.error(ShellErrorType::Expected("operator".to_string()))
                    }
                }
                _ => self.error(ShellErrorType::Expected("operator".to_string())),
            },
            _ => self.error(ShellErrorType::Expected("operator".to_string())),
        }
    }

    pub fn operator_precedence(&mut self, operator: &NodeId) -> usize {
        self.delta.node_types[operator.0].precedence()
    }

    pub fn spanning(&mut self, from: &NodeId, to: &NodeId) -> (usize, usize) {
        (self.delta.span_start[from.0], self.delta.span_end[to.0])
    }

    pub fn block_or_closure(&mut self) -> NodeId {
        let span_start = self.position();

        self.lcurly();
        let output = if self.is_pipe() {
            // closure
            let params = self.params();
            let block = self.code_block(true);
            let span_end = self.position();
            self.create_node(NodeType::Closure { params, block }, span_start, span_end)
        } else {
            // block
            self.code_block(true)
        };

        self.rcurly();

        let span_end = self.position();

        self.delta.span_start[output.0] = span_start;
        self.delta.span_end[output.0] = span_end;

        output
    }

    pub fn cell_path(&mut self, mut head: NodeId, span_start: usize) -> NodeId {
        if self.is_dot() {
            while self.is_dot() {
                self.lexer.next();
                match self.lexer.peek() {
                    Some(Token {
                        token_type: TokenType::Number,
                        span_end,
                        ..
                    }) => {
                        let path = self.number();
                        head =
                            self.create_node(NodeType::RowPath { head, path }, span_start, span_end)
                    }
                    Some(Token {
                        token_type: TokenType::Bareword,
                        span_end,
                        ..
                    }) => {
                        let path = self.bareword();
                        head = self.create_node(
                            NodeType::ColumnPath { head, path },
                            span_start,
                            span_end,
                        )
                    }
                    Some(Token {
                        token_type: TokenType::SimpleString,
                        span_end,
                        ..
                    }) => {
                        let path = self.string();
                        head = self.create_node(
                            NodeType::ColumnPath { head, path },
                            span_start,
                            span_end,
                        )
                    }
                    Some(Token {
                        token_type: TokenType::String,
                        span_end,
                        ..
                    }) => {
                        let path = self.string();
                        head = self.create_node(
                            NodeType::ColumnPath { head, path },
                            span_start,
                            span_end,
                        )
                    }
                    _ => {
                        return self.error(ShellErrorType::Expected("compatible cell path".into()))
                    }
                }
            }
            head
        } else {
            self.error(ShellErrorType::Expected("dot '.'".into()))
        }
    }

    pub fn subexpression(&mut self) -> NodeId {
        let span_start = self.position();

        // FIXME: Add line-skipping to subexpressions
        self.lparen();
        self.lexer.newline_is_space = true;
        let output = self.code_block(true);
        self.lexer.newline_is_space = false;
        self.rparen();

        let span_end = self.position();

        self.delta.span_start[output.0] = span_start;
        self.delta.span_end[output.0] = span_end;

        output
    }

    pub fn list_or_table(&mut self) -> NodeId {
        let span_start = self.position();

        let mut is_table = false;
        self.lsquare();
        let mut items = vec![];
        while self.has_tokens() {
            self.skip_whitespace_and_comments();
            if self.is_rsquare() {
                self.lexer.next();
                break;
            }
            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            items.push(self.simple_expression());

            self.skip_whitespace_and_comments();
            if items.len() == 1 && self.is_semicolon() {
                is_table = true;
                self.lexer.next();
            }
        }

        let span_end = self.position();

        if is_table {
            self.create_node(NodeType::Table(items), span_start, span_end)
        } else {
            self.create_node(NodeType::List(items), span_start, span_end)
        }
    }

    pub fn def(&mut self) -> NodeId {
        let span_start = self.position();
        self.keyword(b"def");

        self.skip_whitespace_and_comments();
        let name = self.name();

        self.skip_whitespace_and_comments();
        let params = self.params();

        self.skip_whitespace_and_comments();
        let block = self.block_or_closure();

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

        self.skip_whitespace_and_comments();
        let name = self.name();

        self.skip_whitespace_and_comments();
        let params = self.params();

        self.skip_whitespace_and_comments();
        let block = self.block_or_closure();

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

        self.skip_whitespace_and_comments();
        let variable_name = self.variable();

        self.skip_whitespace_and_comments();
        self.equals();

        self.skip_whitespace_and_comments();
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

        self.skip_whitespace_and_comments();
        let variable_name = self.variable();

        self.skip_whitespace_and_comments();
        self.equals();

        self.skip_whitespace_and_comments();
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

        self.skip_whitespace_and_comments();
        let variable_name = self.variable();

        self.skip_whitespace_and_comments();
        self.equals();

        self.skip_whitespace_and_comments();
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

        self.skip_whitespace_and_comments();
        let condition = self.expression();

        self.skip_whitespace_and_comments();
        let then_block = self.block_or_closure();

        self.skip_whitespace_and_comments();
        if self.is_keyword(b"else") {
            self.lexer.next();
            self.skip_whitespace_and_comments();
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

    pub fn where_expression(&mut self) -> NodeId {
        let span_start = self.position();

        self.keyword(b"where");

        self.skip_whitespace_and_comments();
        let condition = self.expression();

        let span_end = self.position();

        self.create_node(NodeType::Where(condition), span_start, span_end)
    }

    pub fn repl_bashism(&mut self) -> NodeId {
        let span_start = self.position();

        if self.is_bangbang() {
            self.lexer.next();
            let span_end = self.position();
            return self.create_node(NodeType::BangBang, span_start, span_end);
        } else if self.is_bangdollar() {
            self.lexer.next();
            let span_end = self.position();
            return self.create_node(NodeType::BangDollar, span_start, span_end);
        } else {
            return self.error(ShellErrorType::Expected("bashsim elements".to_string()));
        }
    }
    pub fn is_repl_bashism(&mut self) -> bool {
        self.is_bangbang() || self.is_bangdollar()
    }

    pub fn is_bangbang(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::BangBang,
                ..
            })
        )
    }

    pub fn is_bangdollar(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::BangDollar,
                ..
            })
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

                let to = self.expression_or_call();

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
                let rhs = self.expression_or_call();
                let span_end = self.position();

                from = self.create_node(NodeType::BashAnd { lhs: from, rhs }, span_start, span_end)
            } else if self.is_double_pipe() {
                self.lexer.next();
                self.skip_space();
                let rhs = self.expression_or_call();
                let span_end = self.position();

                from = self.create_node(NodeType::BashOr { lhs: from, rhs }, span_start, span_end)
            } else if self.is_rparen() || self.is_newline() || self.is_rcurly() {
                break;
            } else {
                self.error(ShellErrorType::Expected("pipeline elements".to_string()));
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
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();
                self.create_node(NodeType::Bareword, span_start, span_end)
            }
            _ => self.error(ShellErrorType::Expected("bare word".to_string())),
        }
    }

    pub fn traditional_args(&mut self) -> Vec<NodeId> {
        let mut args = vec![];

        if self.is_lparen() {
            self.lexer.next();
        } else {
            args.push(self.error(ShellErrorType::Expected("Left paren '('".to_string())));
        }

        while self.has_tokens() {
            self.skip_whitespace_and_comments();
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
                } else if self.is_colon() {
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
                } else {
                    self.delta.node_types[head.0] = NodeType::String;
                    args.push(head);
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
                || self.is_rparen()
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
            } else if self.is_simple_expression() {
                // Arg
                args.push(self.simple_expression())
            } else {
                args.push(self.bareword())
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
                self.lexer.next();
                self.create_node(NodeType::Name, span_start, span_end)
            }
            _ => self.error(ShellErrorType::Expected("name".to_string())),
        }
    }

    pub fn params(&mut self) -> NodeId {
        let span_start = self.position();
        let param_list = if self.is_lparen() {
            self.lparen();
            let output = self.param_list();
            self.rparen();

            output
        } else if self.is_pipe() {
            self.pipe();
            let output = self.param_list();
            self.pipe();

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
            self.skip_space();
            if self.is_rparen() || self.is_rsquare() || self.is_pipe() {
                break;
            }

            if self.is_comma() {
                self.lexer.next();
                continue;
            }

            // Parse param
            let span_start = self.position();
            let name = self.variable();
            self.skip_whitespace_and_comments();
            if self.is_colon() {
                // Optional type
                self.colon();

                self.skip_whitespace_and_comments();
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
            }) if contents == keyword => {
                self.lexer.next();
            }
            _ => {
                self.error(ShellErrorType::Expected(
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
                self.error(ShellErrorType::Expected("equals '='".to_string()));
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
                self.error(ShellErrorType::Expected("colon ':'".to_string()));
            }
        }
    }

    pub fn pipe(&mut self) {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Pipe,
                ..
            }) => {
                self.lexer.next();
            }
            _ => {
                self.error(ShellErrorType::Expected("pipe '|'".to_string()));
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

    pub fn is_dot(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Dot,
                ..
            })
        )
    }

    pub fn is_dotdot(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::DotDot,
                ..
            })
        )
    }

    pub fn is_whitespace_or_comment(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Space,
                ..
            }) | Some(Token {
                token_type: TokenType::Newline,
                ..
            }) | Some(Token {
                token_type: TokenType::Comment,
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

    pub fn is_comment(&mut self) -> bool {
        matches!(
            self.lexer.peek(),
            Some(Token {
                token_type: TokenType::Comment,
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
            }) | Some(Token {
                token_type: TokenType::SimpleString,
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
            }) if contents == keyword
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
        self.is_simple_expression() || self.is_keyword(b"if") || self.is_keyword(b"where")
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
                token_type: TokenType::SimpleString,
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
                self.error(ShellErrorType::Expected(
                    "left square bracket '['".to_string(),
                ));
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
                self.error(ShellErrorType::Expected(
                    "right square bracket ']'".to_string(),
                ));
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
                self.error(ShellErrorType::Expected("left paren '('".to_string()));
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
                self.error(ShellErrorType::Expected("right paren ')'".to_string()));
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
                self.error(ShellErrorType::Expected("left bracket '{'".to_string()));
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
                self.error(ShellErrorType::Expected("right bracket '}'".to_string()));
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
                self.lexer.next();
                self.create_node(NodeType::True, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                contents,
            }) if contents == b"false" => {
                self.lexer.next();
                self.create_node(NodeType::False, span_start, span_end)
            }
            _ => self.error(ShellErrorType::Expected("boolean".to_string())),
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
                self.lexer.next();
                self.create_node(NodeType::Variable, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::Bareword,
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();
                self.create_node(NodeType::Variable, span_start, span_end)
            }
            _ => self.error(ShellErrorType::Expected("variable".to_string())),
        }
    }

    pub fn number(&mut self) -> NodeId {
        match self.lexer.peek() {
            Some(Token {
                token_type: TokenType::Number,
                span_start,
                span_end,
                contents,
            }) => {
                self.lexer.next();

                match self.lexer.peek() {
                    Some(Token {
                        token_type: TokenType::Bareword,
                        contents,
                        ..
                    }) => {
                        let unit = match &contents.to_ascii_lowercase()[..] {
                            b"b" => Unit::Byte,
                            b"kb" => Unit::Kilobyte,
                            b"kib" => Unit::Kibibyte,
                            b"mb" => Unit::Megabyte,
                            b"mib" => Unit::Mebibyte,
                            b"gb" => Unit::Gigabyte,
                            b"gib" => Unit::Gibibyte,
                            b"tb" => Unit::Terabyte,
                            b"tib" => Unit::Tebibyte,
                            b"pb" => Unit::Petabyte,
                            b"pib" => Unit::Pebibyte,
                            b"eb" => Unit::Exabyte,
                            b"eib" => Unit::Exbibyte,
                            b"zb" => Unit::Zettabyte,
                            b"zib" => Unit::Zebibyte,
                            b"ns" => Unit::Nanosecond,
                            b"us" => Unit::Microsecond,
                            b"ms" => Unit::Millisecond,
                            b"sec" => Unit::Second,
                            b"min" => Unit::Minute,
                            b"hr" => Unit::Hour,
                            b"day" => Unit::Day,
                            b"wk" => Unit::Week,
                            _ => {
                                return self
                                    .error(ShellErrorType::Expected("unit for value".into()))
                            }
                        };
                        self.lexer.next();
                        self.create_node(NodeType::Unit(unit), span_start, span_end)
                    }
                    _ => {
                        if contents.contains(&b'.') {
                            self.create_node(NodeType::Float, span_start, span_end)
                        } else {
                            self.create_node(NodeType::Int, span_start, span_end)
                        }
                    }
                }
            }
            _ => self.error(ShellErrorType::Expected("number".to_string())),
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
                self.lexer.next();
                self.create_node(NodeType::String, span_start, span_end)
            }
            Some(Token {
                token_type: TokenType::SimpleString,
                span_start,
                span_end,
                ..
            }) => {
                self.lexer.next();
                self.create_node(NodeType::String, span_start, span_end)
            }
            _ => self.error(ShellErrorType::Expected("string".to_string())),
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

    pub fn error(&mut self, error_type: ShellErrorType) -> NodeId {
        if let Some(Token {
            span_start,
            span_end,
            ..
        }) = self.lexer.next()
        {
            self.errors.push(ShellError {
                error_type,
                span_start,
                span_end,
            });
            self.create_node(NodeType::Garbage, span_start, span_end)
        } else {
            self.errors.push(ShellError {
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

    pub fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.lexer.peek() {
                Some(Token {
                    token_type: TokenType::Space,
                    ..
                })
                | Some(Token {
                    token_type: TokenType::Newline,
                    ..
                })
                | Some(Token {
                    token_type: TokenType::Comment,
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
