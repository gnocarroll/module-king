mod run_builtin;

use std::iter::zip;

use crate::{
    parse::{
        AST, TypeVariant,
        ast_contents::{ExprID, FunctionID, TypeID},
        builtin::Builtin,
    },
    run::{
        ExecutionContext,
        context_contents::{RuntimeRef, Value, ValueID, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
        eval,
        eval_operation::{binary::do_assignment, eval_eager},
        expr_to_unit,
        util::{
            allocate_instances_from_pattern, runtime_ref_to_builtin, runtime_ref_to_function,
            runtime_ref_to_type,
        },
    },
};

pub fn eval_operation_apply(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let runtime_ref = eval(ast, ctx, operand1)?;

    let args_ref = eval(ast, ctx, operand2)?;

    // eager evaluate args tuple, reflects how programming languages tend to work
    // so e.g. if var x is passed as arg then get value of x to send to function

    let args_ref = eval_eager(ast, ctx, args_ref)?;

    if let Some(function_id) = runtime_ref_to_function(ast, ctx, runtime_ref) {
        return eval_operation_apply_function(ast, ctx, expr, function_id, args_ref);
    } else if let Some(type_id) = runtime_ref_to_type(ast, ctx, runtime_ref) {
        return eval_operation_apply_cast(ast, ctx, expr, type_id, args_ref);
    } else if let Some(builtin) = runtime_ref_to_builtin(ast, ctx, runtime_ref) {
        return eval_operation_apply_builtin(ast, ctx, expr, builtin, args_ref);
    }

    return Err(RuntimeException {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });
}

fn eval_operation_apply_function(
    ast: &AST,
    ctx: &mut ExecutionContext,
    _expr: ExprID,
    function_id: FunctionID,
    args: RuntimeRef,
) -> Result<RuntimeRef, RuntimeException> {
    // save ID of outside scope, switch to function scope

    let outside_scope = ctx.curr_scope;
    let function_scope = ctx.switch_to_child_scope();

    // set up ret location for function using function scope

    let ret_location = ctx.objs.ret_location_push(function_scope);

    let function_struct = ast.objs.function(function_id);

    // allocate space for param variables

    for pattern_id in &function_struct.params {
        allocate_instances_from_pattern(ast, ctx, *pattern_id, function_struct.scope);
    }

    // copy values of args into function scope

    let args_value_ids: Vec<ValueID> = args.to_tuple_value_iterator(&ctx.objs).collect();

    let args_refs: Vec<RuntimeRef> = args_value_ids
        .into_iter()
        .map(|value_id| {
            RuntimeRef {
                value_id,
                scope: args.scope,
            }
            .dup_in_scope(ast, ctx, function_scope)
            .to_runtime_ref(ctx, function_scope)
        })
        .collect();

    // assign values to args

    for ((name, _), new_value_ref) in zip(
        function_struct
            .params
            .iter()
            .flat_map(|pattern_id| pattern_id.to_pattern_iterator(ast)),
        args_refs.iter(),
    ) {
        let name = ctx.tokens.tok_as_str(&name).to_string();

        let member_id = ast
            .objs
            .scope(function_struct.scope)
            .members
            .get(&name)
            .expect("member should exist");

        let assign_to_ref = ctx
            .objs
            .instance_get(member_id)
            .expect("should be allocated");

        do_assignment(ast, ctx, assign_to_ref, *new_value_ref);
    }

    // eval body and then get return value

    eval(ast, ctx, function_struct.body)?;

    let ret_ref = ctx.objs.ret_location(ret_location);

    let ret_value = ret_ref.dup_in_scope(ast, ctx, outside_scope);

    // destroy ret value location + function scope

    ctx.objs.ret_location_delete(ret_location);
    ctx.pop_curr_scope();

    Ok(ret_value.to_runtime_ref(ctx, outside_scope))
}

fn eval_operation_apply_cast(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    type_id: TypeID,
    args: RuntimeRef,
) -> Result<RuntimeRef, RuntimeException> {
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

fn eval_operation_apply_builtin(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    builtin: Builtin,
    args: RuntimeRef,
) -> Result<RuntimeRef, RuntimeException> {
    let type_id = ast.objs.expr(expr).type_id;

    let type_id = ast.type_resolve_aliasing(type_id);

    let variant = match builtin {
        // These two return types so no real corresponding Rust function
        Builtin::Map | Builtin::List => ValueVariant::Type(type_id),

        Builtin::GenericPush
        | Builtin::GenericGet
        | Builtin::GenericExists
        | Builtin::GenericLen => run_builtin::container_generic(ast, ctx, expr, builtin, args)?,

        // TODO: start working on these allocation-related builtins
        Builtin::Malloc => ValueVariant::Unit,
        Builtin::Mfree => ValueVariant::Unit,

        Builtin::GetWD => run_builtin::get_wd()?,
        Builtin::SetWD => run_builtin::set_wd(ctx, args)?,

        // TODO: start working on these file-related builtins
        Builtin::DirList => run_builtin::dir_list(ctx, args),
        Builtin::FileRead => run_builtin::file_read(ctx, args),

        Builtin::IsFile => run_builtin::is_file(ctx, args),
        Builtin::IsDir => run_builtin::is_dir(ctx, args),

        Builtin::Print | Builtin::Println => {
            run_builtin::builtin_print(ast, ctx, args, builtin == Builtin::Println)
        }

        Builtin::BuiltinCount => {
            panic!("should not have builtin with value BuiltinCount in interpreter")
        }
    };

    Ok(Value {
        type_id: Some(type_id),
        variant,
    }
    .to_runtime_ref(ctx, ctx.curr_scope))
}
