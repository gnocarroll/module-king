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

    let arg2_rref = match (builtin, args_tuple.get(1)) {
        (Builtin::GenericLen, None) => None, // Ok
        (_, Some(value_id)) => Some(RuntimeReference {
            scope: args.scope,
            value_id: *value_id,
        }),
        _ => panic!("{arg_count_err_msg}"),
    };

    let arg2 = arg2_rref.map(|rref| ctx.objs.ref_get(rref).variant.clone());

    let container_variant = &ctx.objs.ref_get(container).variant;

    let value_variant = match &container_variant {
        ValueVariant::String(s) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(s.len() as i64)
            } else {
                let arg2 = arg2.expect("should be arg2 for this Generic");

                match (builtin, arg2) {
                    (Builtin::GenericExists, ValueVariant::Integer(i)) => ValueVariant::Unit,
                    (Builtin::GenericGet, ValueVariant::Integer(i)) => ValueVariant::Unit,
                    (Builtin::GenericPush, ValueVariant::Integer(i)) => {
                        let c = i as u8 as char;

                        ValueVariant::Unit
                    }
                    _ => panic!(""),
                }
            }
        }
        ValueVariant::List(vec) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(vec.len() as i64)
            } else {
                let arg2 = arg2.expect("should be arg2 for this Generic");

                ValueVariant::Unit
            }
        }
        ValueVariant::Map(map) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(map.len() as i64)
            } else {
                let arg2 = arg2.expect("should be arg2 for this Generic");

                ValueVariant::Unit
            }
        }
        _ => panic!("should be container ValueVariant (e.g. String, List, Map"),
    };

    Ok(value_variant)
}

// get working directory
// return of empty string indicates problem occurred
pub fn get_wd() -> Result<ValueVariant, RuntimeException> {
    let empty_string = Ok(ValueVariant::String(b"".to_vec()));

    let s = match std::env::current_dir() {
        Ok(s) => match s.to_str() {
            Some(s) => s.to_string(),
            None => {
                return empty_string;
            }
        },
        Err(_) => {
            return empty_string;
        }
    };

    let path_vec = match crate::util::string_to_ascii(s) {
        Ok(vec) => vec,
        Err(_) => {
            return empty_string;
        }
    };

    Ok(ValueVariant::String(path_vec))
}

pub fn set_wd(
    ctx: &mut ExecutionContext,
    args: RuntimeReference,
) -> Result<ValueVariant, RuntimeException> {
    let path = match &ctx.objs.ref_get(args).variant {
        ValueVariant::String(s) => Path::new(
            std::str::from_utf8(s)
                .expect("user of interpreter was able to create invalid utf8, indicates bug"),
        ),
        _ => {
            return Ok(ValueVariant::Boolean(false));
        }
    };

    let success = std::env::set_current_dir(path).is_ok();

    Ok(ValueVariant::Boolean(success))
}
