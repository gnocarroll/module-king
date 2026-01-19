use crate::{
    parse::{AST, ast_contents::ExprID},
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

    let function_id = match ctx.objs.ref_get(func).variant {
        ValueVariant::Function(function_id) => function_id,
        _ => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::InvalidOperation,
            });
        }
    };

    let function_scope = ctx.switch_to_child_scope();

    let function_struct = ast.objs.function(function_id);

    for pattern_id in &function_struct.params {
        allocate_instances_from_pattern(ast, ctx, *pattern_id, function_struct.scope);
    }

    let new_value_ref = eval(ast, ctx, operand2)?;

    Ok(expr_to_unit(ast, ctx, expr))
}
