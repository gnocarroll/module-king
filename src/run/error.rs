use crate::parse::ast_contents::ExprID;

pub struct RuntimeError {
    pub expr: ExprID,
    pub variant: RuntimeErrorVariant,
}

pub enum RuntimeErrorVariant {
    IntegerOverflow,
    NotImplemented,
    InvalidOperation,
}