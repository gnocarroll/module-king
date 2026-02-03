pub mod apply;

use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    constants::UNIT_TYPE,
    parse::{
        AST, HasFileModule, ScopeRefersTo, ScopeVariant, Type, TypeVariant, ast_contents::ExprID,
    },
    run::{
        ExecutionContext, Value, ValueVariant,
        context_contents::{RuntimeRef, RuntimeScopeID, ValueID},
        error::{RuntimeErrorVariant, RuntimeException},
        eval,
        eval_operation::{binary::apply::eval_operation_apply, eval_eager},
        expr_to_unit,
    },
    scan::TokenType,
};

fn integer_op(ttype: TokenType) -> Option<fn(i64, i64) -> i64> {
    let i64_pow = |lhs: i64, rhs: i64| lhs.pow(rhs as u32);

    match ttype {
        TokenType::Plus => Some(i64::add),
        TokenType::Minus => Some(i64::sub),
        TokenType::Star => Some(i64::mul),
        TokenType::FSlash => Some(i64::div),
        TokenType::Percent => Some(i64::rem),
        TokenType::StarStar => Some(i64_pow),

        // bit ops
        TokenType::Pipe => Some(|lhs, rhs| lhs | rhs),
        TokenType::Ampersand => Some(|lhs, rhs| lhs & rhs),
        TokenType::Carrot => Some(|lhs, rhs| lhs ^ rhs),
        TokenType::LtLt => Some(|lhs, rhs| lhs << rhs),
        TokenType::GtGt => Some(|lhs, rhs| lhs >> rhs),

        _ => None,
    }
}

fn integer_cmp(ttype: TokenType) -> Option<fn(i64, i64) -> bool> {
    match ttype {
        TokenType::Gt => Some(|lhs, rhs| lhs > rhs),
        TokenType::Lt => Some(|lhs, rhs| lhs < rhs),
        TokenType::Ge => Some(|lhs, rhs| lhs >= rhs),
        TokenType::Le => Some(|lhs, rhs| lhs <= rhs),
        TokenType::EqEq => Some(|lhs, rhs| lhs == rhs),
        TokenType::BangEq => Some(|lhs, rhs| lhs != rhs),
        _ => None,
    }
}

fn float_op(ttype: TokenType) -> Option<fn(f64, f64) -> f64> {
    match ttype {
        TokenType::Plus => Some(f64::add),
        TokenType::Minus => Some(f64::sub),
        TokenType::Star => Some(f64::mul),
        TokenType::FSlash => Some(f64::div),
        TokenType::Percent => Some(f64::rem),
        TokenType::StarStar => Some(f64::powf),
        _ => None,
    }
}

fn float_cmp(ttype: TokenType) -> Option<fn(f64, f64) -> bool> {
    match ttype {
        TokenType::Gt => Some(|lhs, rhs| lhs > rhs),
        TokenType::Lt => Some(|lhs, rhs| lhs < rhs),
        TokenType::Ge => Some(|lhs, rhs| lhs >= rhs),
        TokenType::Le => Some(|lhs, rhs| lhs <= rhs),
        TokenType::EqEq => Some(|lhs, rhs| lhs == rhs),
        TokenType::BangEq => Some(|lhs, rhs| lhs != rhs),
        _ => None,
    }
}

fn allocate_instances_from_ref(ast: &AST, ctx: &mut ExecutionContext, runtime_ref: RuntimeRef) {
    match ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Identifier(member_id) => {
            ctx.objs.instance_alloc(ast, ctx.curr_scope, member_id);
        }
        _ => todo!("e.g. create instances from more complicated pattern"),
    }
}

fn eval_operation_colon(
    ast: &AST,
    ctx: &mut ExecutionContext,
    _expr: ExprID,
    operand1: ExprID,
    _operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let ret = eval(ast, ctx, operand1);

    if let Ok(lhs_ref) = ret {
        allocate_instances_from_ref(ast, ctx, lhs_ref);
    }

    ret
}

fn eval_operation_period(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let expr_type_id = ast.objs.expr(expr).type_id;

    let mut value_ref = eval(ast, ctx, operand1)?;

    // auto deref as much as possible like in Rust

    while value_ref.is_ref_variant(ctx) {
        value_ref = value_ref.deref(ast, ctx);
    }

    let member_ref = eval(ast, ctx, operand2)?;

    let value_to_access = ctx.objs.ref_get(value_ref);
    let member = ctx.objs.ref_get(member_ref);

    match (&value_to_access.variant, &member.variant) {
        (ValueVariant::Type(type_id), ValueVariant::Identifier(member_id)) => {
            let scope_id = match ast.objs.type_get(*type_id) {
                Type::Scope(scope_id) => scope_id,
                _ => panic!("type being accessed with period should have scope"),
            };

            let scope_struct = ast.objs.scope(*scope_id);

            let discriminant = scope_struct
                .members
                .member_idx(*member_id)
                .expect("member not found in Type");

            match scope_struct.variant {
                ScopeVariant::Type(TypeVariant::Enum) => {
                    return Ok(Value {
                        type_id: Some(expr_type_id),
                        variant: ValueVariant::Enum(discriminant),
                    }
                    .to_runtime_ref(ctx, ctx.curr_scope));
                }
                _ => {
                    panic!("only implemented global members for enum so far");
                }
            }
        }
        (ValueVariant::Record(map), ValueVariant::Identifier(member_id)) => {
            let name = ast.objs.member(*member_id).name.clone();

            let name = member_id.get_tokens(ast).tok_or_string_to_string(&name);

            return match map.get(&name) {
                Some(value_id) => Ok(RuntimeRef {
                    scope: ctx.curr_scope,
                    value_id: *value_id,
                }),
                None => Err(RuntimeException {
                    expr,
                    variant: RuntimeErrorVariant::InvalidOperation,
                }),
            };
        }
        (ValueVariant::Identifier(member_id), ValueVariant::Identifier(field_member_id)) => {
            let runtime_ref = match ctx.objs.instance_get(*member_id) {
                Some(r) => r,
                None => {
                    return Err(RuntimeException {
                        expr,
                        variant: RuntimeErrorVariant::MemberDNE,
                    });
                }
            };

            let name = ast.objs.member(*field_member_id).name.clone();

            let name = field_member_id
                .get_tokens(ast)
                .tok_or_string_to_string(&name);

            let value_id = *match &ctx.objs.ref_get(runtime_ref).variant {
                ValueVariant::Record(map) => match map.get(&name) {
                    Some(id) => id,
                    None => {
                        return Err(RuntimeException {
                            expr,
                            variant: RuntimeErrorVariant::BadIdent,
                        });
                    }
                },
                _ => panic!(),
            };

            return Ok(RuntimeRef {
                scope: ctx.curr_scope,
                value_id,
            });
        }
        _ => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::InvalidOperation,
            });
        }
    }
}

fn do_assignment(
    ast: &AST,
    ctx: &mut ExecutionContext,
    assign_to_ref: RuntimeRef,
    new_value_ref: RuntimeRef,
) -> Result<(), RuntimeErrorVariant> {
    // eager evaluate new value e.g. to pull value out of implicit ref

    let new_value_ref = match eval_eager(ast, ctx, new_value_ref) {
        Ok(rref) => rref,
        Err(e) => {
            return Err(e.variant);
        }
    };

    let new_value = new_value_ref.dup_in_scope(ast, ctx, assign_to_ref.scope);

    // if lhs is identifier then find + overwrite value

    match ctx.objs.ref_get(assign_to_ref).variant {
        ValueVariant::Identifier(member_id) => {
            ctx.objs.instance_set(member_id, new_value);
            return Ok(());
        }
        ValueVariant::ImplicitRef(rref) => {
            ctx.objs.ref_set(rref, new_value);
            return Ok(());
        }
        ValueVariant::ImplicitCharRef(char_ref) => {
            let i_value = match new_value.variant {
                ValueVariant::Integer(i) => i,
                _ => panic!("value being assigned to char ref does not have type Integer"),
            };

            let value: u8 = match i_value.try_into() {
                Ok(i) => i,
                Err(_) => {
                    return Err(RuntimeErrorVariant::IntegerOverflow);
                }
            };

            // now place new value at index indicated by CharRef

            match &mut ctx.objs.ref_get_mut(char_ref.string_rref).variant {
                ValueVariant::String(s) => {
                    s[char_ref.idx] = value;
                }
                _ => panic!("should be String ref"),
            }

            return Ok(());
        }
        _ => (),
    }

    // otherwise use value id to replace value

    ctx.objs
        .runtime_scope_mut(assign_to_ref.scope)
        .value_overwrite(assign_to_ref.value_id, new_value);

    Ok(())
}

fn eval_operation_eq(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let assign_to_ref = eval(ast, ctx, operand1)?;
    let new_value_ref = eval(ast, ctx, operand2)?;

    match do_assignment(ast, ctx, assign_to_ref, new_value_ref) {
        Ok(_) => (),
        Err(variant) => {
            return Err(RuntimeException { expr, variant });
        }
    }

    Ok(expr_to_unit(ast, ctx, expr))
}

fn eval_operation_semi(
    ast: &AST,
    ctx: &mut ExecutionContext,
    _expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let unit_type = ast.get_builtin_type_id(UNIT_TYPE);

    let ret = Ok(Value {
        type_id: Some(unit_type),
        variant: ValueVariant::Unit,
    }
    .to_runtime_ref(ctx, ctx.curr_scope));

    eval(ast, ctx, operand1)?;

    if ctx.return_now {
        return ret;
    }

    let operand_ref2 = eval(ast, ctx, operand2)?;

    eprintln!("RHS OF SEMI: {}", operand_ref2.to_string(ast, ctx));

    ret
}

fn eval_operation_comma(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let operand1_ref = eval(ast, ctx, operand1)?;
    let operand2_ref = eval(ast, ctx, operand2)?;

    let mut value_id_vec = vec![operand1_ref.value_id];

    match &ctx.objs.ref_get(operand2_ref).variant {
        ValueVariant::Unit => (),
        ValueVariant::Tuple(value_id_vec2) => {
            let operand2_type_id = ast.objs.expr(operand2).type_id;

            match ast.objs.type_get(operand2_type_id) {
                Type::Tuple(_) => {
                    value_id_vec.push(operand2_ref.value_id);
                }
                Type::RestOfTuple(_) => {
                    // if is RestOfTuple then we extend the Tuple's vec because
                    // second value id vec is rest of tuple rather than a second element
                    // in the tuple

                    value_id_vec.extend_from_slice(value_id_vec2);
                }
                _ => panic!("type should be guaranteed to be Tuple or RestOfTuple"),
            }
        }
        _ => {
            value_id_vec.push(operand2_ref.value_id);
        }
    }

    let type_id = ast.objs.expr(expr).type_id;

    Ok(Value {
        type_id: Some(type_id),
        variant: ValueVariant::Tuple(value_id_vec),
    }
    .to_runtime_ref(ctx, ctx.curr_scope))
}

fn eval_operation_braces(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let type_id = Some(ast.objs.expr(expr).type_id);

    let mut operand1_ref = eval(ast, ctx, operand1)?;

    match &ctx.objs.ref_get(operand1_ref).variant {
        ValueVariant::ImplicitRef(rref) => {
            operand1_ref = *rref;
        }
        _ => (),
    }

    let operand2_ref = eval(ast, ctx, operand2)?;

    let index_value_id_vec =
        |scope: RuntimeScopeID, value_id_vec: &Vec<ValueID>, idx: usize| -> Result<ValueVariant, RuntimeException> {
            let value_id = match value_id_vec.get(idx) {
                Some(value_id) => *value_id,
                None => {
                    return Err(RuntimeException {
                        expr,
                        variant: RuntimeErrorVariant::IndexOutOfBounds,
                    });
                }
            };

            Ok(ValueVariant::ImplicitRef(RuntimeRef { scope, value_id }))
        };

    let variant = match (
        &ctx.objs.ref_get(operand1_ref).variant,
        &ctx.objs.ref_get(operand2_ref).variant,
    ) {
        // index array with Integer
        (ValueVariant::Array(value_id_vec), ValueVariant::Integer(idx)) => {
            // scope is array's scope, then get particular value id of member

            index_value_id_vec(operand1_ref.scope, value_id_vec, (*idx) as usize)?
        }

        // index array with Enum
        (ValueVariant::Array(value_id_vec), ValueVariant::Enum(idx)) => {
            index_value_id_vec(operand1_ref.scope, value_id_vec, *idx)?
        }

        _ => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::InvalidOperation,
            });
        }
    };

    Ok(Value { type_id, variant }.to_runtime_ref(ctx, ctx.curr_scope))
}

pub fn eval_operation_binary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    let invalid_op = Err(RuntimeException {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });

    let type_ids = [
        ast.objs.expr(operand1).type_id,
        ast.objs.expr(operand2).type_id,
    ];

    match op {
        // some operations are only relevant to semantic analysis and can be skipped here
        TokenType::Is => return Ok(expr_to_unit(ast, ctx, expr)),

        // for colon lhs is identifier(s), just return them
        TokenType::Colon => return eval_operation_colon(ast, ctx, expr, operand1, operand2),
        TokenType::Period => return eval_operation_period(ast, ctx, expr, operand1, operand2),

        TokenType::LParen => return eval_operation_apply(ast, ctx, expr, operand1, operand2),

        TokenType::LBrace => return eval_operation_braces(ast, ctx, expr, operand1, operand2),

        TokenType::Semicolon => return eval_operation_semi(ast, ctx, expr, operand1, operand2),

        TokenType::Comma => return eval_operation_comma(ast, ctx, expr, operand1, operand2),

        // currently same code can be used for Eq, ColonEq
        TokenType::Eq => return eval_operation_eq(ast, ctx, expr, operand1, operand2),
        TokenType::ColonEq => {
            let lhs = eval(ast, ctx, operand1)?;

            match ctx.objs.ref_get(lhs).variant {
                ValueVariant::Identifier(member_id) => {
                    ctx.objs.instance_alloc(ast, ctx.curr_scope, member_id);

                    let rhs = eval(ast, ctx, operand2)?;

                    match do_assignment(ast, ctx, lhs, rhs) {
                        Ok(_) => (),
                        Err(variant) => {
                            return Err(RuntimeException { expr, variant });
                        }
                    }
                }
                _ => todo!("other assignment"),
            }

            return Ok(expr_to_unit(ast, ctx, expr));
        }

        // other case
        _ => (),
    }

    let operand_refs = [eval(ast, ctx, operand1)?, eval(ast, ctx, operand2)?];

    // eagerly evaluate LHS, RHS before performing operation so e.g.
    // if one side is a var we will get the value

    let operand_refs = [
        eval_eager(ast, ctx, operand_refs[0])?,
        eval_eager(ast, ctx, operand_refs[1])?,
    ];

    let operand_values = [
        ctx.objs.ref_get(operand_refs[0]),
        ctx.objs.ref_get(operand_refs[1]),
    ];

    let ret = match (&operand_values[0].variant, &operand_values[1].variant) {
        (ValueVariant::Integer(lhs), ValueVariant::Integer(rhs)) => {
            if let Some(i64_op) = integer_op(op) {
                ValueVariant::Integer(i64_op(*lhs, *rhs))
            } else if let Some(i64_cmp) = integer_cmp(op) {
                ValueVariant::Boolean(i64_cmp(*lhs, *rhs))
            } else {
                return invalid_op;
            }
        }
        (ValueVariant::Float(lhs), ValueVariant::Float(rhs)) => {
            if let Some(f64_op) = float_op(op) {
                ValueVariant::Float(f64_op(*lhs, *rhs))
            } else if let Some(f64_cmp) = float_cmp(op) {
                ValueVariant::Boolean(f64_cmp(*lhs, *rhs))
            } else {
                return invalid_op;
            }
        }
        _ => {
            return invalid_op;
        }
    };

    Ok(Value {
        type_id: Some(type_ids[0]),
        variant: ret,
    }
    .to_runtime_ref(ctx, ctx.curr_scope))
}
