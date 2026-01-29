use std::{collections::HashMap, path::Path};

use crate::{
    parse::{
        AST,
        ast_contents::{ExprID, TypeID},
        builtin::Builtin,
    },
    run::{
        ExecutionContext,
        context_contents::{CharRef, RuntimeRef, Value, ValueID, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
    },
};

pub fn container_generic(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    builtin: Builtin,
    args: RuntimeRef,
) -> Result<ValueVariant, RuntimeException> {
    let args_tuple: Vec<ValueID> = args.to_tuple_value_iterator(&ctx.objs).collect();

    let container_ref_value_id = match args_tuple.get(0) {
        Some(id) => *id,
        None => panic!("for generic there should be first arg which is container ref"),
    };

    let container = match ctx
        .objs
        .ref_get(RuntimeRef {
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
        (_, Some(value_id)) => Some(RuntimeRef {
            scope: args.scope,
            value_id: *value_id,
        }),
        _ => panic!("{arg_count_err_msg}"),
    };

    let arg2_value_id = arg2_rref.map(|rref| rref.dup_in_scope_get_id(ast, ctx, container.scope));

    let arg2_rref = arg2_value_id.map(|value_id| RuntimeRef {
        scope: container.scope,
        value_id,
    });

    let arg2_variant = arg2_rref.map(|rref| ctx.objs.ref_get(rref).variant.clone());

    let container_variant = &mut ctx.objs.ref_get_mut(container).variant;

    let value_variant = match container_variant {
        ValueVariant::String(s) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(s.len() as i64)
            } else {
                let arg2_variant = arg2_variant.expect("should have checked for arg2");

                match (builtin, arg2_variant) {
                    (Builtin::GenericExists, ValueVariant::Integer(i)) => {
                        ValueVariant::Boolean(i >= 0 && (i as usize) < s.len())
                    }
                    (Builtin::GenericGet, ValueVariant::Integer(i)) => {
                        ValueVariant::CharRef(CharRef {
                            string_rref: container, // container is the String itself
                            idx: i as usize,
                        })
                    }
                    (Builtin::GenericPush, ValueVariant::Integer(i)) => {
                        let c = match i.try_into() {
                            Ok(c) => c,
                            Err(_) => {
                                return Err(RuntimeException {
                                    expr,
                                    variant: RuntimeErrorVariant::IntegerOverflow,
                                });
                            }
                        };

                        s.push(c);

                        ValueVariant::Unit
                    }
                    _ => panic!(
                        "arg type invalid, should have been checked during semantic analysis"
                    ),
                }
            }
        }
        ValueVariant::List(vec) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(vec.len() as i64)
            } else {
                let (arg2_value_id, arg2_variant) = (
                    arg2_value_id.expect("should have checked for arg2"),
                    arg2_variant.expect("should have checked for arg2"),
                );

                match (builtin, arg2_variant) {
                    (Builtin::GenericExists, ValueVariant::Integer(i)) => {
                        ValueVariant::Boolean(i >= 0 && (i as usize) < vec.len())
                    }
                    (Builtin::GenericGet, ValueVariant::Integer(i)) => {
                        ValueVariant::Ref(RuntimeRef {
                            scope: container.scope,
                            value_id: match vec.get(i as usize) {
                                Some(id) => *id,
                                None => {
                                    return Err(RuntimeException {
                                        expr,
                                        variant: RuntimeErrorVariant::IndexOutOfBounds,
                                    });
                                }
                            },
                        })
                    }
                    (Builtin::GenericPush, _) => {
                        vec.push(arg2_value_id);

                        ValueVariant::Unit
                    }
                    _ => panic!(
                        "arg type invalid, should have been checked during semantic analysis"
                    ),
                }
            }
        }
        ValueVariant::Map(map) => {
            if builtin == Builtin::GenericLen {
                ValueVariant::Integer(map.len() as i64)
            } else {
                let (arg2_rref, arg2_value_id, arg2_variant) = (
                    arg2_rref.expect("should have checked for arg2"),
                    arg2_value_id.expect("should have checked for arg2"),
                    arg2_variant.expect("should have checked for arg2"),
                );

                match (builtin, arg2_variant) {
                    (Builtin::GenericExists, ValueVariant::String(s)) => {
                        let s = crate::util::ascii_to_string(s);

                        ValueVariant::Boolean(map.contains_key(&s))
                    }
                    (Builtin::GenericGet, ValueVariant::String(s)) => {
                        let s = crate::util::ascii_to_string(s);

                        ValueVariant::Ref(RuntimeRef {
                            scope: container.scope,
                            value_id: match map.get(&s) {
                                Some(id) => *id,
                                None => {
                                    return Err(RuntimeException {
                                        expr,
                                        variant: RuntimeErrorVariant::IndexOutOfBounds,
                                    });
                                }
                            },
                        })
                    }
                    (Builtin::GenericPush, ValueVariant::Tuple(vec)) => {
                        let err_msg = "semantic analysis should have checked that (String, Value) tuple is being pushed";

                        if vec.len() != 2 {
                            panic!("{err_msg}")
                        }

                        let string_rref = RuntimeRef {
                            scope: arg2_rref.scope,
                            value_id: vec[0],
                        };
                        let push_id = vec[1];

                        let key = match &ctx.objs.ref_get(string_rref).variant {
                            ValueVariant::String(s) => s.clone(),
                            _ => panic!("{err_msg}"),
                        };

                        let key = crate::util::ascii_to_string(key);

                        // get container again and perform push

                        let map = match &mut ctx.objs.ref_get_mut(container).variant {
                            ValueVariant::Map(map) => map,
                            _ => panic!("should have already checked it is map"),
                        };

                        // note: if value already exists this will overwrite which is intended behaviour

                        map.insert(key, push_id);

                        ValueVariant::Unit
                    }
                    _ => panic!(
                        "arg type invalid, should have been checked during semantic analysis"
                    ),
                }
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
    args: RuntimeRef,
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

fn args_to_string(ctx: &ExecutionContext, args: RuntimeRef) -> String {
    let args_tuple: Vec<ValueID> = args.to_tuple_value_iterator(&ctx.objs).collect();

    let filepath = match &ctx
        .objs
        .ref_get(RuntimeRef {
            scope: args.scope,
            value_id: args_tuple[0],
        })
        .variant
    {
        ValueVariant::String(s) => s.clone(),
        _ => panic!("should be passing String to dir list"),
    };

    crate::util::ascii_to_string(filepath)
}

pub fn dir_list(ctx: &mut ExecutionContext, args: RuntimeRef) -> ValueVariant {
    let filepath = args_to_string(ctx, args);

    // if problem occurs then will just return empty list

    let get_list = || {
        let mut ret_strings: Vec<Vec<u8>> = Vec::new();

        let dir_iter = match std::fs::read_dir(filepath) {
            Ok(dir_iter) => dir_iter,
            Err(_) => {
                return Vec::new();
            }
        };

        for entry in dir_iter {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => {
                    return Vec::new();
                }
            };

            ret_strings.push(entry.file_name().as_encoded_bytes().into());
        }

        ret_strings
    };

    let ret_strings = get_list();

    let mut value_ids = Vec::new();

    for ret_string in ret_strings {
        value_ids.push(
            ctx.objs
                .runtime_scope_mut(ctx.curr_scope)
                .value_push(Value {
                    type_id: Some(TypeID::string()),
                    variant: ValueVariant::String(ret_string),
                }),
        );
    }

    ValueVariant::Tuple(value_ids)
}

pub fn file_read(ctx: &mut ExecutionContext, args: RuntimeRef) -> ValueVariant {
    let filepath = args_to_string(ctx, args);

    let contents = match std::fs::read_to_string(filepath) {
        Ok(s) => s,
        Err(_) => "".to_string(),
    };

    ValueVariant::String(contents.into())
}

// check if provided string is a filepath to some file
pub fn is_file(ctx: &ExecutionContext, args: RuntimeRef) -> ValueVariant {
    let filepath = args_to_string(ctx, args);

    ValueVariant::Boolean(std::path::Path::new(&filepath).is_file())
}

// check if provided string is a filepath to some dir
pub fn is_dir(ctx: &ExecutionContext, args: RuntimeRef) -> ValueVariant {
    let filepath = args_to_string(ctx, args);

    ValueVariant::Boolean(std::path::Path::new(&filepath).is_dir())
}

pub fn builtin_print(
    ast: &AST,
    ctx: &ExecutionContext,
    args: RuntimeRef,
    is_println: bool,
) -> ValueVariant {
    print!("{}", args.to_string(ast, ctx));

    if is_println {
        println!();
    }

    ValueVariant::Unit
}

pub fn m_alloc(ctx: &ExecutionContext, args: RuntimeRef) -> ValueVariant {
    let args_tuple: Vec<ValueID> = args.to_tuple_value_iterator(&ctx.objs).collect();

    let type_id = match ctx
        .objs
        .ref_get(RuntimeRef {
            scope: args.scope,
            value_id: args_tuple[0],
        })
        .variant
    {
        ValueVariant::Type(type_id) => type_id,
        _ => panic!("argument to memory allocator should be type"),
    };
}
