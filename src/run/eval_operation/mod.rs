mod binary;

use crate::{
    constants::INTEGER_TYPE,
    parse::{AST, Operation, ast_contents::ExprID},
    run::{
        ExecutionContext, Value, ValueVariant,
        context_contents::RuntimeRef,
        error::{RuntimeErrorVariant, RuntimeException},
        eval,
        eval_operation::binary::eval_operation_binary,
        expr_to_unit,
    },
    scan::TokenType,
};

pub fn eval_eager(
    ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeRef,
) -> Result<RuntimeRef, RuntimeException> {
    let mut runtime_ref = runtime_ref;

    loop {
        match &ctx.objs.ref_get(runtime_ref).variant {
            // Ref types are returned by value
            // However for IMPLICIT ref types to eager evaluate get value out
            ValueVariant::Unit
            | ValueVariant::Boolean(_)
            | ValueVariant::Float(_)
            | ValueVariant::Function(_)
            | ValueVariant::Integer(_)
            | ValueVariant::Module(_)
            | ValueVariant::Record(_)
            | ValueVariant::String(_)
            | ValueVariant::Type(_)
            | ValueVariant::Builtin(_)
            | ValueVariant::List(_)
            | ValueVariant::Map(_)
            | ValueVariant::Ref(_)
            | ValueVariant::Ptr(_)
            | ValueVariant::CharRef(_) => return Ok(runtime_ref),
            ValueVariant::Tuple(value_ids) => {
                let value_ids = value_ids.clone();

                let type_id = ctx.objs.ref_get(runtime_ref).type_id;

                // will eager evaluate each element in tuple and place new value ids in this Vec

                let mut new_value_ids = Vec::new();

                for value_id in value_ids {
                    let mut rref = RuntimeRef {
                        scope: runtime_ref.scope,
                        value_id,
                    };

                    // eager evaluate element
                    
                    rref = eval_eager(ast, ctx, rref)?;

                    // also if result is not in current scope then duplicate into current scope

                    if rref.scope != ctx.curr_scope {
                        rref = rref.dup_in_scope_get_rref(ast, ctx, ctx.curr_scope);
                    }

                    new_value_ids.push(rref.value_id);
                }

                // now push new tuple value, get ID, and then return rref

                let value_id = ctx
                    .objs
                    .runtime_scope_mut(ctx.curr_scope)
                    .value_push(Value {
                        type_id,
                        variant: ValueVariant::Tuple(new_value_ids),
                    });

                return Ok(RuntimeRef {
                    scope: ctx.curr_scope,
                    value_id: value_id,
                });
            }
            ValueVariant::Identifier(ident) => {
                return Ok(ctx.objs.instance_get(*ident).expect("MEMBER NOT ALLOCATED"));
            }
            ValueVariant::ImplicitRef(r) => {
                runtime_ref = *r;
            }
            ValueVariant::ImplicitCharRef(char_ref) => {
                // get value out of char ref and then convert to i64 for ValueVariant::Integer

                let type_id = Some(ast.get_builtin_type_id(INTEGER_TYPE));
                let variant = ValueVariant::Integer(char_ref.load(ctx) as i64);

                // push value to CURRENT SCOPE

                let value_id = ctx
                    .objs
                    .runtime_scope_mut(ctx.curr_scope)
                    .value_push(Value { type_id, variant });

                return Ok(RuntimeRef {
                    scope: ctx.curr_scope,
                    value_id: value_id,
                });
            }
        }
    }
}

pub fn eval_operation(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operation: Operation,
) -> Result<RuntimeRef, RuntimeException> {
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
) -> Result<RuntimeRef, RuntimeException> {
    ctx.switch_to_child_scope();

    // now contained expr is evaluated inside new scope

    eval(ast, ctx, operand)?;

    ctx.pop_curr_scope();

    return Ok(expr_to_unit(ast, ctx, expr));
}

fn eval_operation_unary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
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

        // deref
        (TokenType::Star, ValueVariant::Ref(rref) | ValueVariant::Ptr(rref)) => {
            ValueVariant::ImplicitRef(*rref)
        }
        (TokenType::Star, ValueVariant::CharRef(char_ref)) => {
            ValueVariant::ImplicitCharRef(*char_ref)
        }

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
