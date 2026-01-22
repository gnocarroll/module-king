use std::path::Path;

use crate::{
    parse::{AST, ast_contents::ExprID},
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
    },
};

pub fn generic_push(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    args: RuntimeReference,
) -> Result<ValueVariant, RuntimeException> {
    
}

pub fn get_wd(expr: ExprID) -> Result<ValueVariant, RuntimeException> {
    let s = match std::env::current_dir() {
        Ok(s) => match s.to_str() {
            Some(s) => s.to_string(),
            None => {
                return Err(RuntimeException {
                    expr,
                    variant: RuntimeErrorVariant::BuiltinFailed,
                });
            }
        },
        Err(_) => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::BuiltinFailed,
            });
        }
    };

    Ok(ValueVariant::String(s))
}

pub fn set_wd(
    _ast: &AST,
    ctx: &mut ExecutionContext,
    _expr: ExprID,
    args: RuntimeReference,
) -> Result<ValueVariant, RuntimeException> {
    let path = match &ctx.objs.ref_get(args).variant {
        ValueVariant::String(s) => Path::new(s),
        _ => {
            return Ok(ValueVariant::Boolean(false));
        }
    };

    let success = std::env::set_current_dir(path).is_ok();

    Ok(ValueVariant::Boolean(success))
}
