// error structs for syntax, semantic analysis

use crate::{
    parse::{
        ExprReturns,
        ast_contents::{ExprID, MemberID, TypeID},
    },
    scan::Token,
    tokens::ExpectedToken,
};

// e.g. function beginning, end name does not match
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Debug)]
pub struct ExpectedExprReturns {
    pub expr: ExprID,
    pub expected: ExprReturns,
    pub found: ExprReturns,
}

#[derive(Clone, Debug)]
pub struct InvalidOperation {
    pub operation: ExprID, // expr id
    pub msg: &'static str,
}

#[derive(Clone, Debug)]
pub struct ExpectedType {
    pub expr: ExprID,
    pub expected: TypeID,
    pub found: TypeID,
}

#[derive(Clone, Debug)]
pub struct MissingOperand {
    pub operation: ExprID, // expr id

    // for this member do 1-indexed
    pub operand_missing: u32,
}

#[derive(Clone, Debug)]
pub struct InvalidExpr {
    pub expr: ExprID,
    pub msg: &'static str,
}

// maybe in the future should add more context
// i.e. why is expr unexpected there
#[derive(Clone, Debug)]
pub struct UnexpectedExpr {
    pub expr: ExprID,
}

#[derive(Clone, Debug)]
pub struct DuplicateName {
    pub name: String,

    // old member was already in scope and there was an attempt to add new
    // member with same name

    pub old_member: MemberID,
    pub new_member: MemberID,
}

#[derive(Clone, Debug)]
pub enum SemanticError {
    ExpectedExprReturns(ExpectedExprReturns),
    InvalidOperation(InvalidOperation),
    ExpectedType(ExpectedType),
    MissingOperand(MissingOperand),
    UnexpectedExpr(UnexpectedExpr),
    PatternError(PatternError),
    InvalidExpr(InvalidExpr),
    DuplicateName(DuplicateName)
}

#[derive(Clone, Copy, Debug)]
pub struct ExprAndType {
    pub expr: ExprID,
    pub type_id: TypeID,
}

#[derive(Clone, Debug)]
pub enum PatternError {
    ParenMismatch(ExprAndType),
    TypeMissing(ExprID),
    IdentMissing(TypeID),
    MatchingUnsupported(ExprID),
}
