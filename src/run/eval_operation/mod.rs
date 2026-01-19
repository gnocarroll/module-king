mod binary;

use crate::{
    parse::{AST, Operation, ast_contents::ExprID},
    run::{
        ExecutionContext, Value, ValueVariant,
        context_contents::RuntimeReference,
        error::{RuntimeErrorVariant, RuntimeException},
        eval,
        eval_operation::binary::eval_operation_binary,
        expr_to_unit,
    },
    scan::TokenType,
};

pub fn eval_eager(
    _ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeReference,
) -> Result<RuntimeReference, RuntimeException> {
    let mut runtime_ref = runtime_ref;

    loop {
        match ctx.objs.ref_get(runtime_ref).variant {
            ValueVariant::Unit
            | ValueVariant::Boolean(_)
            | ValueVariant::Float(_)
            | ValueVariant::Function(_)
            | ValueVariant::Integer(_)
            | ValueVariant::Module(_)
            | ValueVariant::Record(_)
            | ValueVariant::String(_)
            | ValueVariant::Tuple(_)
            | ValueVariant::Type(_) => return Ok(runtime_ref),
            ValueVariant::Identifier(ident) => {
                return Ok(ctx.objs.instance_get(ident).expect("MEMBER NOT ALLOCATED"));
            }
            ValueVariant::Ref(r) | ValueVariant::ImplicitRef(r) => {
                runtime_ref = r;
            }
        }
    }
}

pub fn eval_operation(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operation: Operation,
) -> Result<RuntimeReference, RuntimeException> {
    match (operation.operand1, operation.operand2) {
        (Some(operand), None) => eval_operation_unary(ast, ctx, expr, operation.op, operand),
        (Some(operand1), Some(operand2)) => {
            eval_operation_binary(ast, ctx, expr, operation.op, operand1, operand2)
        }
        _ => Err(RuntimeException {
            expr,
            variant: RuntimeErrorVariant::InvalidOperation,
        }),
    }
}

fn eval_operation_begin(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    let block_scope = ctx.switch_to_child_scope();

    // now contained expr is evaluated inside new scope

    eval(ast, ctx, operand)?;

    ctx.objs.runtime_scope_delete(block_scope);

    return Ok(expr_to_unit(ast, ctx, expr))
}

fn eval_operation_unary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    match op {
        TokenType::Begin => return eval_operation_begin(ast, ctx, expr, operand),
        _ => (),
    }

    let invalid_op = Err(RuntimeException {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });

    let type_id = ast.objs.expr(operand).type_id;

    let operand_ref = eval(ast, ctx, operand)?;

    if op == TokenType::LParen {
        return Ok(operand_ref);
    }

    let operand_ref = eval_eager(ast, ctx, operand_ref)?;

    let operand_value = ctx.objs.ref_get(operand_ref);

    if op == TokenType::Return {
        let operand_value = operand_value.clone();

        // get location where return value should go and then overwrite its current value

        let ret_ref = ctx.objs.ret_location_top();

        ctx.objs
            .runtime_scope_mut(ret_ref.scope)
            .value_overwrite(ret_ref.value_id, operand_value);

        // set flag indicating that function execution should terminate

        ctx.return_now = true;

        // ret of return expr is Unit

        return Ok(expr_to_unit(ast, ctx, expr));
    }

    let ret = match (op, &operand_value.variant) {
        // unary +
        (TokenType::Plus, ValueVariant::Integer(_) | ValueVariant::Float(_)) => {
            // no-op
            return Ok(operand_ref);
        }

        // unary -
        (TokenType::Minus, ValueVariant::Integer(i)) => ValueVariant::Integer(-i),
        (TokenType::Minus, ValueVariant::Float(f)) => ValueVariant::Float(-f),

        // unrecognized
        _ => {
            return invalid_op;
        }
    };

    Ok(Value {
        type_id: Some(type_id),
        variant: ret,
    }
    .to_runtime_ref(ctx, ctx.curr_scope))
}
