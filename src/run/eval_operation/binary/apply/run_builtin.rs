use std::{collections::HashMap, path::Path};

use crate::{
    parse::{AST, ast_contents::ExprID, builtin::Builtin},
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, ValueID, ValueVariant},
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
    let args_tuple = match &ctx.objs.ref_get(args).variant {
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

    let arg_count_err_msg = "should be one arg for len and two for other generics";

    let arg2 = match (builtin, args_tuple.get(1)) {
        (Builtin::GenericLen, None) => None, // Ok
        (_, Some(value_id)) => Some(RuntimeReference {
            scope: args.scope,
            value_id: *value_id,
        }),
        _ => panic!("{arg_count_err_msg}"),
    };

    let builtin_variant_err_msg =
        "should be one of the builtin container-related functions e.g. push";

    let container_variant = &ctx.objs.ref_get(container).variant;

    let value_variant = match &container_variant {
        ValueVariant::String(_) => {
            if builtin == Builtin::GenericLen {
                let map = match container_variant {
                    ValueVariant::Map(map) => map,
                    _ => panic!("expected Map"),
                };

                ValueVariant::Integer(map.len() as i64)
            } else {

                ValueVariant::Unit
            }
        }
        ValueVariant::List(_) => {
            if builtin == Builtin::GenericLen {
                let map = match container_variant {
                    ValueVariant::Map(map) => map,
                    _ => panic!("expected Map"),
                };

                ValueVariant::Integer(map.len() as i64)
            } else {

                ValueVariant::Unit
            }
        }
        ValueVariant::Map(_) => {
            if builtin == Builtin::GenericLen {
                let map = match container_variant {
                    ValueVariant::Map(map) => map,
                    _ => panic!("expected Map"),
                };

                ValueVariant::Integer(map.len() as i64)
            } else {

                ValueVariant::Unit
            }
        }
        _ => panic!("should be container ValueVariant (e.g. String, List, Map"),
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
