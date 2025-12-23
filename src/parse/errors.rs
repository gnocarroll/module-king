// error structs for syntax, semantic analysis

use crate::{parse::ExprReturns, scan::{Token, TokenType}};

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

#[derive(Clone)]
pub struct ExpectedExprReturns {
    pub expr: u32,
    pub expected: ExprReturns,
    pub found: ExprReturns,
}