#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
    span_offset: usize,

    // For times when the lexer should treat newlines
    // as spaces
    pub newline_is_space: bool,
}

#[derive(Debug)]
pub enum TokenType {
    Number,
    BinaryLiterial,
    Space,
    Newline,
    Comma,
    Comment,
    SimpleString,
    String,
    Dot,
    DotDot,
    Dollar,
    Variable,
    Interpolation,
    Pipe,
    PipePipe,
    Colon,
    Semicolon,
    Plus,
    PlusPlus,
    Dash,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    ForwardSlash,
    ForwardSlashForwardSlash,
    Equals,
    EqualsEquals,
    EqualsTilde,
    ExclamationTilde,
    ExclamationEquals,
    LParen,
    LSquare,
    LCurly,
    LessThan,
    LessThanEqual,
    RParen,
    RSquare,
    RCurly,
    GreaterThan,
    GreaterThanEqual,
    Ampersand,
    AmpersandAmpersand,
    Bareword,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub contents: &'a [u8],
    pub span_start: usize,
    pub span_end: usize,
}

fn is_symbol(b: u8) -> bool {
    [
        b'+', b'-', b'*', b'/', b'.', b',', b'(', b'[', b'{', b'<', b')', b']', b'}', b'>', b':',
        b';', b'=', b'$', b'|', b'!', b'~', b'&', b'\'', b'"',
    ]
    .contains(&b)
}

fn is_binary_literial(source: &[u8]) -> bool {
    source.len() > 2 && source[0] == b'0' && source[1] == b'b' && source[2] == b'['
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8], span_offset: usize) -> Self {
        Self {
            source,
            span_offset,
            newline_is_space: false,
        }
    }

    pub fn lex_quoted_string(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 1;
        let mut is_escaped = false;
        while token_offset < self.source.len() {
            if is_escaped {
                is_escaped = false;
            } else if self.source[token_offset] == b'\\' {
                is_escaped = true;
            } else if self.source[token_offset] == b'"' {
                token_offset += 1;
                break;
            }
            token_offset += 1;
        }

        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::String,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_single_quoted_string(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 1;
        while token_offset < self.source.len() {
            if self.source[token_offset] == b'\'' {
                token_offset += 1;
                break;
            }
            token_offset += 1;
        }

        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::SimpleString,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_quoted_bareword(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 1;
        while token_offset < self.source.len() {
            if self.source[token_offset] == b'`' {
                token_offset += 1;
                break;
            }
            token_offset += 1;
        }

        self.span_offset += token_offset;

        let contents = &self.source[1..(token_offset - 1)];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Bareword,
            contents,
            span_start: span_start + 1,
            span_end: self.span_offset,
        })
    }

    pub fn lex_number(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 0;
        while token_offset < self.source.len() {
            if !self.source[token_offset].is_ascii_digit() {
                break;
            }
            token_offset += 1;
        }

        // Check to see if we have a hex/octal/binary number
        if token_offset < self.source.len() && self.source[token_offset] == b'x' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !self.source[token_offset].is_ascii_hexdigit() {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len() && self.source[token_offset] == b'o' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !(self.source[token_offset] >= b'0' && self.source[token_offset] <= b'7') {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len() && self.source[token_offset] == b'b' {
            token_offset += 1;
            while token_offset < self.source.len() {
                if !(self.source[token_offset] >= b'0' && self.source[token_offset] <= b'1') {
                    break;
                }
                token_offset += 1;
            }
        } else if token_offset < self.source.len()
            && self.source[token_offset] == b'.'
            && (token_offset + 1 < self.source.len())
            && self.source[token_offset + 1].is_ascii_digit()
        {
            // Looks like a float
            token_offset += 1;
            while token_offset < self.source.len() {
                if !self.source[token_offset].is_ascii_digit() {
                    break;
                }
                token_offset += 1;
            }

            if token_offset < self.source.len()
                && (self.source[token_offset] == b'e' || self.source[token_offset] == b'E')
            {
                token_offset += 1;

                if token_offset < self.source.len() && self.source[token_offset] == b'-' {
                    token_offset += 1;
                }

                while token_offset < self.source.len() {
                    if !self.source[token_offset].is_ascii_digit() {
                        break;
                    }
                    token_offset += 1;
                }
            }
        }

        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Number,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_space(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 0;
        let whitespace: &[u8] = if self.newline_is_space {
            &[b' ', b'\t', b'\r', b'\n']
        } else {
            &[b' ', b'\t', b'\r']
        };
        while token_offset < self.source.len() {
            if !whitespace.contains(&self.source[token_offset]) {
                break;
            }
            token_offset += 1;
        }
        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Space,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_newline(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        self.span_offset += 1;

        let contents = &self.source[..1];
        self.source = &self.source[1..];

        Some(Token {
            token_type: TokenType::Newline,
            contents,
            span_start,
            span_end: span_start + 1,
        })
    }

    pub fn lex_binary_literial(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;

        let mut token_offset = 3;
        while token_offset < self.source.len() {
            if self.source[token_offset] != b'0' && self.source[token_offset] != b'1' {
                break;
            }
            token_offset += 1;
        }
        self.span_offset += token_offset;

        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::BinaryLiterial,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_dollar_expression(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;

        let mut token_offset = 1;
        if self.source.len() > token_offset
            && !self.source[token_offset].is_ascii_whitespace()
            && !is_symbol(self.source[token_offset])
        {
            while token_offset < self.source.len() {
                if self.source[token_offset].is_ascii_whitespace()
                    || is_symbol(self.source[token_offset])
                {
                    break;
                }
                token_offset += 1;
            }
            self.span_offset += token_offset;
            let contents = &self.source[..token_offset];
            self.source = &self.source[token_offset..];

            Some(Token {
                token_type: TokenType::Variable,
                contents,
                span_start,
                span_end: self.span_offset,
            })
        } else if self.source.len() > token_offset && self.source[token_offset] == b'\'' {
            self.span_offset += 1;
            self.source = &self.source[1..];
            self.lex_single_quoted_string().map(|x| Token {
                token_type: TokenType::Interpolation,
                span_start,
                ..x
            })
        } else if self.source.len() > token_offset && self.source[token_offset] == b'"' {
            self.span_offset += 1;
            self.source = &self.source[1..];
            self.lex_quoted_string().map(|x| Token {
                token_type: TokenType::Interpolation,
                span_start,
                ..x
            })
        } else {
            self.span_offset += 1;
            self.source = &self.source[1..];
            Some(Token {
                token_type: TokenType::Dollar,
                span_start,
                span_end: self.span_offset,
                contents: &[b'$'],
            })
        }
    }

    pub fn lex_comment(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        self.span_offset += 1;

        let mut token_offset = 1;
        while token_offset < self.source.len() {
            if self.source[token_offset] == b'\n' {
                break;
            }
            token_offset += 1;
        }
        self.span_offset += token_offset;
        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Comment,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }

    pub fn lex_symbol(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;

        let result = match self.source[0] {
            b'(' => Token {
                token_type: TokenType::LParen,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'[' => Token {
                token_type: TokenType::LSquare,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'{' => Token {
                token_type: TokenType::LCurly,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'<' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::LessThanEqual,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::LessThan,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b')' => Token {
                token_type: TokenType::RParen,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b']' => Token {
                token_type: TokenType::RSquare,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'}' => Token {
                token_type: TokenType::RCurly,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'>' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::GreaterThanEqual,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::GreaterThan,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'+' => {
                if self.source.len() > 1 && self.source[1] == b'+' {
                    Token {
                        token_type: TokenType::PlusPlus,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Plus,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'-' => Token {
                token_type: TokenType::Dash,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'*' => {
                if self.source.len() > 1 && self.source[1] == b'*' {
                    Token {
                        token_type: TokenType::AsteriskAsterisk,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Asterisk,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'/' => {
                if self.source.len() > 1 && self.source[1] == b'/' {
                    Token {
                        token_type: TokenType::ForwardSlashForwardSlash,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::ForwardSlash,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'=' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::EqualsEquals,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > 1 && self.source[1] == b'~' {
                    Token {
                        token_type: TokenType::EqualsTilde,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Equals,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b':' => Token {
                token_type: TokenType::Colon,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b';' => Token {
                token_type: TokenType::Semicolon,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'.' => {
                if self.source.len() > 1 && self.source[1] == b'.' {
                    Token {
                        token_type: TokenType::DotDot,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Dot,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'!' => {
                if self.source.len() > 1 && self.source[1] == b'=' {
                    Token {
                        token_type: TokenType::ExclamationEquals,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else if self.source.len() > 1 && self.source[1] == b'~' {
                    Token {
                        token_type: TokenType::ExclamationTilde,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Exclamation,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'$' => Token {
                token_type: TokenType::Dollar,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'|' => {
                if self.source.len() > 1 && self.source[1] == b'|' {
                    Token {
                        token_type: TokenType::PipePipe,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Pipe,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b'&' => {
                if self.source.len() > 1 && self.source[1] == b'&' {
                    Token {
                        token_type: TokenType::AmpersandAmpersand,
                        contents: &self.source[..2],
                        span_start,
                        span_end: span_start + 2,
                    }
                } else {
                    Token {
                        token_type: TokenType::Ampersand,
                        contents: &self.source[..1],
                        span_start,
                        span_end: span_start + 1,
                    }
                }
            }
            b',' => Token {
                token_type: TokenType::Comma,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            x => {
                panic!(
                    "Internal compiler error: symbol character mismatched in lexer: {}",
                    x as char
                )
            }
        };

        self.span_offset = result.span_end;
        self.source = &self.source[(result.span_end - span_start)..];

        Some(result)
    }

    pub fn lex_bareword(&mut self) -> Option<Token<'a>> {
        let span_start = self.span_offset;
        let mut token_offset = 0;
        while token_offset < self.source.len() {
            if self.source[token_offset].is_ascii_whitespace()
                || self.source[token_offset] == b'{'
                || self.source[token_offset] == b'}'
                || self.source[token_offset] == b')'
                || self.source[token_offset] == b'('
                || self.source[token_offset] == b'['
                || self.source[token_offset] == b']'
                || self.source[token_offset] == b';'
                || self.source[token_offset] == b'|'
                || self.source[token_offset] == b':'
                || self.source[token_offset] == b','
            {
                break;
            }
            token_offset += 1;
        }
        self.span_offset += token_offset;
        let contents = &self.source[..token_offset];
        self.source = &self.source[token_offset..];

        Some(Token {
            token_type: TokenType::Bareword,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }
}

impl<'a> Lexer<'a> {
    pub fn peek(&mut self) -> Option<Token<'a>> {
        let prev_offset = self.span_offset;
        let prev_source = self.source;
        let output = self.next();
        self.span_offset = prev_offset;
        self.source = prev_source;

        output
    }

    // pub fn peek_two_tokens(&mut self) -> (Option<Token<'a>>, Option<Token<'a>>) {
    //     let prev_offset = self.span_offset;
    //     let prev_source = self.source;
    //     let output1 = self.next();
    //     let output2 = self.next();
    //     self.span_offset = prev_offset;
    //     self.source = prev_source;

    //     (output1, output2)
    // }

    pub fn peek_two_tokens_skip_whitespace(&mut self) -> (Option<Token<'a>>, Option<Token<'a>>) {
        let prev_offset = self.span_offset;
        let prev_source = self.source;

        self.skip_whitespace_and_comments();
        let output1 = self.next();

        self.skip_whitespace_and_comments();
        let output2 = self.next();

        self.span_offset = prev_offset;
        self.source = prev_source;

        (output1, output2)
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        if self.source.is_empty() {
            None
        } else if is_binary_literial(self.source) {
            self.lex_binary_literial()
        } else if self.source[0].is_ascii_digit() {
            self.lex_number()
        } else if self.source[0] == b'"' {
            self.lex_quoted_string()
        } else if self.source[0] == b'\'' {
            self.lex_single_quoted_string()
        } else if self.source[0] == b'`' {
            self.lex_quoted_bareword()
        } else if self.source[0] == b' '
            || self.source[0] == b'\t'
            || self.source[0] == b'\r'
            || (self.newline_is_space && self.source[0] == b'\n')
        {
            self.lex_space()
        } else if self.source[0] == b'$' {
            self.lex_dollar_expression()
        } else if is_symbol(self.source[0]) {
            self.lex_symbol()
        } else if self.source[0] == b'\n' {
            self.lex_newline()
        } else if self.source[0] == b'#' {
            self.lex_comment()
        } else {
            self.lex_bareword()
        }
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
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
                    self.next();
                }
                _ => return,
            }
        }
    }
}
