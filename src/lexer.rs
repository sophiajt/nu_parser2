#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a [u8],
    span_offset: usize,
}

#[derive(Debug)]
pub enum TokenType {
    Number,
    Space,
    Newline,
    Comma,
    Dot,
    Dollar,
    Colon,
    Semicolon,
    PlusSign,
    Dash,
    Asterisk,
    ForwardSlash,
    Equals,
    LParen,
    LSquare,
    LCurly,
    LAngle,
    RParen,
    RSquare,
    RCurly,
    RAngle,
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
        b';', b'=', b'$',
    ]
    .contains(&b)
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8], span_offset: usize) -> Self {
        Self {
            source,
            span_offset,
        }
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
        while token_offset < self.source.len() {
            if self.source[token_offset] != b' '
                && self.source[token_offset] != b'\t'
                && self.source[token_offset] != b'\r'
            {
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
            b'<' => Token {
                token_type: TokenType::LAngle,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
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
            b'>' => Token {
                token_type: TokenType::RAngle,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'+' => Token {
                token_type: TokenType::PlusSign,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'-' => Token {
                token_type: TokenType::Dash,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'*' => Token {
                token_type: TokenType::Asterisk,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'/' => Token {
                token_type: TokenType::ForwardSlash,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'=' => Token {
                token_type: TokenType::Equals,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
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
            b'.' => Token {
                token_type: TokenType::Dot,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
            b'$' => Token {
                token_type: TokenType::Dollar,
                contents: &self.source[..1],
                span_start,
                span_end: span_start + 1,
            },
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
            token_type: TokenType::Bareword,
            contents,
            span_start,
            span_end: self.span_offset,
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.source.is_empty() {
            None
        } else if self.source[0].is_ascii_digit() {
            self.lex_number()
        } else if self.source[0] == b' ' || self.source[0] == b'\t' || self.source[0] == b'\r' {
            self.lex_space()
        } else if is_symbol(self.source[0]) {
            self.lex_symbol()
        } else if self.source[0] == b'\n' {
            self.lex_newline()
        } else {
            self.lex_bareword()
        }
    }
}
