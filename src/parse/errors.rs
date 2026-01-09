// error structs for syntax, semantic analysis

use crate::{parse::{Expr, ExprReturns, Operation, ast_contents::{ExprID, TypeID}}, scan::{Token, TokenType}, tokens::ExpectedToken};

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
    pub expr: ExprID,
    pub expected: ExprReturns,
    pub found: ExprReturns,
}

#[derive(Clone)]
pub struct InvalidOperation {
    pub operation: ExprID, // expr id
    pub msg: &'static str,
}

#[derive(Clone)]
pub struct ExpectedType {
    pub expected: TypeID,
    pub found: TypeID,
}

#[derive(Clone)]
pub struct MissingOperand {
    pub operation: ExprID, // expr id

    // for this member do 1-indexed
    pub operand_missing: u32,
}

// maybe in the future should add more context
// i.e. why is expr unexpected there
#[derive(Clone)]
pub struct UnexpectedExpr {
    pub expr: ExprID,
}

#[derive(Clone)]
pub enum SemanticError {
    ExpectedExprReturns(ExpectedExprReturns),
    InvalidOperation(InvalidOperation),
    ExpectedType(ExpectedType),
    MissingOperand(MissingOperand),
    UnexpectedExpr(UnexpectedExpr),
    PatternError(PatternError),
}

#[derive(Clone, Copy)]
pub struct ExprAndType {
    pub expr: ExprID,
    pub type_id: TypeID,
}


#[derive(Clone)]
pub enum PatternError {
    ParenMismatch(ExprAndType),
    TypeMissing(ExprID),
    IdentMissing(TypeID),
    MatchingUnsupported(ExprID),
}