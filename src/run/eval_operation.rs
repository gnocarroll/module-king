use crate::{parse::{AST, Operation, ast_contents::ExprID}, run::{ExecutionContext, Value, error::RuntimeError}};

pub fn eval_operation(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operation: Operation,
) -> Result<Value, RuntimeError> {

}