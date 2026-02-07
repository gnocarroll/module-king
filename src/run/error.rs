use crate::parse::{AST, ast_contents::ExprID};

#[derive(Debug)]
pub enum RuntimeErrorVariant {
    IntegerOverflow,
    NotImplemented,
    InvalidOperation,
    MemberDNE,
    BadIdent,
    UnexpectedType,
    IndexOutOfBounds,
}

#[derive(Debug)]
pub struct RuntimeException {
    pub expr: ExprID,
    pub variant: RuntimeErrorVariant,
}

impl RuntimeException {
    pub fn to_string(&self, ast: &AST) -> String {
        let expr_string = ast.expr_to_string(self.expr);

        format!("{:?}: {}", self.variant, expr_string,)
    }
}
