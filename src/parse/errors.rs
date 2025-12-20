// error structs for syntax, semantic analysis

use crate::scan::{Token, TokenType};

pub struct ExpectedToken {
    pub expected: TokenType,
    pub found: Token,
}

// e.g. function beginning, end name does not match
pub struct NameMismatch {
    pub line: usize,
    pub column: usize,

    pub expected: String,
    pub found: String,
}

pub enum ParseError {
    ExpectedToken(ExpectedToken),
    NameMismatch(NameMismatch),
}

pub struct SemanticError {
    msg: String,
}