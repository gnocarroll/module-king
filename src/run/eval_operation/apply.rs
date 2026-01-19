use crate::{
    parse::{
        AST, TypeVariant,
        ast_contents::{ExprID, FunctionID, TypeID},
    },
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, Value, ValueVariant},
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
    let args_ref = eval(ast, ctx, operand2)?;

    if let Some(function_id) = runtime_ref_to_function(ast, ctx, runtime_ref) {
        return eval_operation_apply_function(ast, ctx, expr, function_id, args_ref);
    } else if let Some(type_id) = runtime_ref_to_type(ast, ctx, runtime_ref) {
        return eval_operation_apply_cast(ast, ctx, expr, type_id, args_ref);
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
    args: RuntimeReference,
) -> Result<RuntimeReference, RuntimeException> {
    let function_scope = ctx.switch_to_child_scope();

    let function_struct = ast.objs.function(function_id);

    for pattern_id in &function_struct.params {
        allocate_instances_from_pattern(ast, ctx, *pattern_id, function_struct.scope);
    }

    Ok(expr_to_unit(ast, ctx, expr))
}

fn eval_operation_apply_cast(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    type_id: TypeID,
    args: RuntimeReference,
) -> Result<RuntimeReference, RuntimeException> {
    let value_variant = match ast.type_get_variant(type_id) {
        Some(TypeVariant::Integer) => {
            let i = match ctx.objs.ref_get(args).variant {
                ValueVariant::Integer(i) => i,
                ValueVariant::Float(f) => f as i64,
                ValueVariant::Boolean(bool_value) => {
                    // presumably this is how bool cast works anyways so probably no point in doing this

                    if bool_value { 1 } else { 0 }
                }
                _ => {
                    return Err(RuntimeException {
                        expr,
                        variant: RuntimeErrorVariant::InvalidOperation,
                    });
                }
            };

            Some(ValueVariant::Integer(i))
        }
        Some(TypeVariant::Float) => {
            let f = match ctx.objs.ref_get(args).variant {
                ValueVariant::Float(f) => f,
                ValueVariant::Integer(i) => i as f64,
                ValueVariant::Boolean(bool_value) => {
                    if bool_value {
                        1.0
                    } else {
                        0.0
                    }
                }
                _ => {
                    return Err(RuntimeException {
                        expr,
                        variant: RuntimeErrorVariant::InvalidOperation,
                    });
                }
            };

            Some(ValueVariant::Float(f))
        }
        Some(TypeVariant::Record) => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::InvalidOperation,
            });
        }
        _ => None,
    };

    if let Some(variant) = value_variant {
        return Ok(Value {
            type_id: Some(type_id),
            variant,
        }
        .to_runtime_ref(ctx, ctx.curr_scope));
    }

    Ok(expr_to_unit(ast, ctx, expr))
}
