// error structs for syntax, semantic analysis

use crate::scan::{Token, TokenType};

#[derive(Clone, Copy)]
pub struct ExpectedToken {
    pub expected: TokenType,
    pub found: Token,
}

// e.g. function beginning, end name does not match
#[derive(Clone, Copy)]
pub struct NameMismatch {
    pub expected: Token,
    pub found: Token,
}

pub enum ParseError {
    ExpectedToken(ExpectedToken),
    NameMismatch(NameMismatch),
}

pub struct SemanticError {
    msg: String,
}