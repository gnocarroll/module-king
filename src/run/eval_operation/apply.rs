use crate::{
    parse::{
        AST,
        ast_contents::{ExprID, FunctionID, TypeID},
    },
    run::{
        ExecutionContext,
        context_contents::RuntimeReference,
        error::{RuntimeErrorVariant, RuntimeException},
        eval, expr_to_unit,
        util::{allocate_instances_from_pattern, runtime_ref_to_function, runtime_ref_to_type},
    },
};

pub fn eval_operation_apply(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    let runtime_ref = eval(ast, ctx, operand1)?;

    if let Some(function_id) = runtime_ref_to_function(ast, ctx, runtime_ref) {
        return eval_operation_apply_function(ast, ctx, expr, function_id, operand2);
    } else if let Some(type_id) = runtime_ref_to_type(ast, ctx, runtime_ref) {
        return eval_operation_apply_cast(ast, ctx, expr, type_id, operand2);
    }

    return Err(RuntimeException {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });
}

fn eval_operation_apply_function(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    function_id: FunctionID,
    args: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    let function_scope = ctx.switch_to_child_scope();

    let function_struct = ast.objs.function(function_id);

    for pattern_id in &function_struct.params {
        allocate_instances_from_pattern(ast, ctx, *pattern_id, function_struct.scope);
    }

    let args_ref = eval(ast, ctx, args)?;

    Ok(expr_to_unit(ast, ctx, expr))
}

fn eval_operation_apply_cast(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    type_id: TypeID,
    args: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    // TODO: implement

    Ok(expr_to_unit(ast, ctx, expr))
}
