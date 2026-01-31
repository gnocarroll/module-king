use crate::scan::{Token, TokenType};

#[derive(Clone)]
pub enum TokenOrString {
    Token(Token),
    String(String),
}

#[derive(Clone, Copy)]
pub struct ExpectedToken {
    pub expected: TokenType,
    pub found: Token,
}

pub struct Tokens {
    file_str: Vec<u8>,
    tokens: Vec<Token>,

    // token not file offset
    offset: usize,
}

impl Tokens {
    pub fn new(file_str: Vec<u8>, tokens: Vec<Token>) -> Self {
        Tokens {
            file_str,
            tokens,
            offset: 0,
        }
    }

    // Token -> &str
    pub fn tok_as_str(&self, token: &Token) -> &str {
        token.as_str(&self.file_str)
    }

    pub fn tok_or_string_to_string(&self, token_or_string: &TokenOrString) -> String {
        match token_or_string {
            TokenOrString::Token(t) => self.tok_as_str(t).to_string(),
            TokenOrString::String(s) => s.clone(),
        }
    }

    pub fn idx(&self) -> u32 {
        self.offset as u32
    }

    // 0-indexed (e.g. 0 is the same as just calling peek())
    fn _peek_nth(&self, idx: isize) -> Token {
        let mut idx = idx + (self.offset as isize);

        if idx < 0 {
            idx = 0;
        }

        match self.tokens.get(idx as usize) {
            Some(t) => *t,
            None => match self.tokens.last() {
                Some(t) => *t,
                None => Token::default(),
            },
        }
    }

    pub fn peek_nth(&self, idx: usize) -> Token {
        self._peek_nth(idx as isize)
    }

    pub fn peek(&self) -> Token {
        self.peek_nth(0)
    }

    pub fn next(&mut self) -> Token {
        if self.offset < self.tokens.len() - 1 {
            self.offset += 1;
        }

        self._peek_nth(-1)
    }

    pub fn expect(&mut self, ttype: TokenType) -> Result<Token, ExpectedToken> {
        let maybe_ret = self.peek();

        if maybe_ret.ttype != ttype {
            return Err(ExpectedToken {
                expected: ttype,
                found: maybe_ret,
            });
        }

        self.next();

        Ok(maybe_ret)
    }

    // advance until you find one of the provided token types (or EOF)
    // but DO NOT consume it, so ret will be peek()
    pub fn sync(&mut self, ttypes: &[TokenType]) -> Token {
        loop {
            let tok = self.peek();

            if tok.ttype == TokenType::Eof || ttypes.iter().any(|ttype| *ttype == tok.ttype) {
                return tok;
            }

            self.next();
        }
    }
}
