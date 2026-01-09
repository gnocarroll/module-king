use crate::{
    parse::{AST, ExprVariant, Operation, TypeOrModule, ast_contents::ExprID},
    run::{
        ExecutionContext, Value, ValueVariant, error::{RuntimeError, RuntimeErrorVariant}, eval
    },
    scan::TokenType,
};

pub fn eval_operation(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operation: Operation,
) -> Result<Value, RuntimeError> {
    match (operation.operand1, operation.operand2) {
        (Some(operand), None) => eval_operation_unary(ast, ctx, expr, operation.op, operand),
        (Some(operand1), Some(operand2)) => {
            eval_operation_binary(ast, ctx, expr, operation.op, operand1, operand2)
        }
        _ => Err(RuntimeError {
            expr,
            variant: RuntimeErrorVariant::InvalidOperation,
        }),
    }
}

fn eval_operation_unary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand: ExprID,
) -> Result<Value, RuntimeError> {
    let invalid_op = Err(RuntimeError { expr, variant: RuntimeErrorVariant::InvalidOperation });

    let type_id = match ast.objs.expr(operand).type_or_module {
        TypeOrModule::Type(t) => t,
        TypeOrModule::Module(_) => {
            return invalid_op;
        }
    };
    let operand_value = eval(ast, ctx, operand)?;

    let ret = match (op, &operand_value.variant) {
        (TokenType::Plus, ValueVariant::Integer(_) | ValueVariant::Float(_)) => {
            // no-op
            return Ok(operand_value);
        }
        (TokenType::Minus, ValueVariant::Integer(i)) => ValueVariant::Integer(-i),
        (TokenType::Minus, ValueVariant::Float(f)) => ValueVariant::Float(-f),
        _ => {
            return invalid_op;
        }
    };

    Ok(Value { type_id: Some(type_id), variant: ret })
}

fn eval_operation_binary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<Value, RuntimeError> {
    Err(RuntimeError { expr, variant: RuntimeErrorVariant::NotImplemented })
}
