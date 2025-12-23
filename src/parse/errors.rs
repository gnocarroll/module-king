// error structs for syntax, semantic analysis

use crate::{parse::{Expr, ExprReturns, Operation}, scan::{Token, TokenType}};

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

// an expr could return different things e.g. a module or a type
// so if you tried to assign a module to an Integer or something
// this should be recorded
#[derive(Clone)]
pub struct ExpectedExprReturns {
    pub expr: u32,
    pub expected: ExprReturns,
    pub found: ExprReturns,
}

#[derive(Clone)]
pub struct InvalidOperation {
    pub operation: Expr,
    pub msg: String,
}

#[derive(Clone)]
pub struct ExpectedType {
    pub expected: u32,
    pub found: u32,
}

#[derive(Clone)]
pub struct MissingOperand {
    pub operation: Expr,

    // for this member do 1-indexed
    pub operand_missing: u32,
}

#[derive(Clone)]
pub enum SemanticError {
    ExpectedExprReturns(ExpectedExprReturns),
    InvalidOperation(InvalidOperation),
    ExpectedType(ExpectedType),
    MissingOperand(MissingOperand),
}