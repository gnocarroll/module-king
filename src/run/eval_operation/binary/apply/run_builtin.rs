use std::path::Path;

use crate::{
    parse::{AST, ast_contents::ExprID, builtin::Builtin},
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
    },
};

pub fn container_generic(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    builtin: Builtin,
    args: RuntimeReference,
) -> Result<ValueVariant, RuntimeException> {
    let args_tuple = match ctx.objs.ref_get(args).variant {
        ValueVariant::Tuple(args_tuple) => args_tuple,
        _ => panic!("should be tuple of args"),
    };

    let container_ref_value_id = match args_tuple.get(0) {
        Some(id) => *id,
        None => panic!("for generic there should be first arg which is container ref"),
    };

    let container = match ctx
        .objs
        .ref_get(RuntimeReference {
            scope: args.scope,
            value_id: container_ref_value_id,
        })
        .variant
    {
        ValueVariant::Ref(container) => container,
        _ => panic!("first arg should be Ref for Generic"),
    };

    let value_variant = match &ctx.objs.ref_get(args).variant {
        ValueVariant::String(s) => match builtin {

        }
    };

    Ok(value_variant)
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
