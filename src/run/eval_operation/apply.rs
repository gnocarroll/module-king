use crate::{
    parse::{
        AST,
        ast_contents::{ExprID, FunctionID},
    },
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
        eval, expr_to_unit,
        util::allocate_instances_from_pattern,
    },
};

pub fn eval_operation_apply(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    let func = eval(ast, ctx, operand1)?;

    match ctx.objs.ref_get(func).variant {
        ValueVariant::Function(function_id) => {
            eval_operation_apply_function(ast, ctx, expr, function_id, operand2)
        }
        _ => Err(RuntimeException {
            expr,
            variant: RuntimeErrorVariant::InvalidOperation,
        }),
    }
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

    let new_value_ref = eval(ast, ctx, operand2)?;

    Ok(expr_to_unit(ast, ctx, expr))
}
