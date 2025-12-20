// error structs for syntax, semantic analysis

use crate::scan::{Token, TokenType};

pub struct ExpectedToken<'a> {
    pub expected: TokenType,
    pub found: Token<'a>,
}

// e.g. function beginning, end name does not match
pub struct NameMismatch<'a> {
    pub line: usize,
    pub column: usize,

    pub expected: &'a str,
    pub found: &'a str,
}

pub enum ParseError<'a> {
    ExpectedToken(ExpectedToken<'a>),
    NameMismatch(NameMismatch<'a>),
}

pub struct SemanticError {
    msg: String,
}