use crate::parse::ast_contents::ExprID;

pub struct RuntimeException {
    pub expr: ExprID,
    pub variant: RuntimeErrorVariant,
}

pub enum RuntimeErrorVariant {
    IntegerOverflow,
    NotImplemented,
    InvalidOperation,
    RootExprMissing,
    MemberDNE,
    BadIdent,
    UnexpectedType,
    BuiltinFailed,
}