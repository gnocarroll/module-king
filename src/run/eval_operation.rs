use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    parse::{AST, Operation, TypeOrModule, ast_contents::ExprID},
    run::{
        ExecutionContext, Value, ValueVariant,
        context_contents::RuntimeReference,
        error::{RuntimeError, RuntimeErrorVariant},
        eval,
    },
    scan::TokenType,
};

pub fn eval_operation(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operation: Operation,
) -> Result<RuntimeReference, RuntimeError> {
    match (operation.operand1, operation.operand2) {
        (Some(operand), None) => eval_operation_unary(ast, ctx, expr, operation.op, operand),
        (Some(operand1), Some(operand2)) => {
            eval_operation_binary(ast, ctx, expr, operation.op, operand1, operand2)
        }
        _ => Err(RuntimeError {
            expr,
            variant: RuntimeErrorVariant::InvalidOperation,
        }),
    }
}

fn eval_operation_unary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand: ExprID,
) -> Result<RuntimeReference, RuntimeError> {
    let invalid_op = Err(RuntimeError {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });

    let type_id = match ast.objs.expr(operand).type_or_module {
        TypeOrModule::Type(t) => t,
        TypeOrModule::Module(_) => {
            return invalid_op;
        }
    };

    let operand_ref = eval(ast, ctx, operand)?;
    let operand_value = ctx.objs.ref_get(operand_ref);

    let ret = match (op, &operand_value.variant) {
        // unary +
        (TokenType::Plus, ValueVariant::Integer(_) | ValueVariant::Float(_)) => {
            // no-op
            return Ok(operand_ref);
        }

        // unary -
        (TokenType::Minus, ValueVariant::Integer(i)) => ValueVariant::Integer(-i),
        (TokenType::Minus, ValueVariant::Float(f)) => ValueVariant::Float(-f),

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

fn eval_operation_period(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeReference, RuntimeError> {
    let value_ref = eval(ast, ctx, operand1)?;
    let member_ref = eval(ast, ctx, operand2)?;

    let value_to_access = ctx.objs.ref_get(value_ref);
    let member = ctx.objs.ref_get(member_ref);

    match (&value_to_access.variant, &member.variant) {
        (ValueVariant::Record(map), ValueVariant::Identifier(member_id)) => {
            let name = ctx
                .tokens
                .tok_or_string_to_string(&ast.objs.member(*member_id).name);

            return match map.get(&name) {
                Some(value_id) => Ok(RuntimeReference { scope: ctx.curr_scope, value_id: *value_id }),
                None => Err(RuntimeError {
                    expr,
                    variant: RuntimeErrorVariant::InvalidOperation,
                }),
            };
        }
        (ValueVariant::Identifier(member_id), ValueVariant::Identifier(field_member_id)) => {
            let runtime_ref = match ctx.objs.instance_get(*member_id) {
                Some(r) => r,
                None => {
                    return Err(RuntimeError { expr, variant: RuntimeErrorVariant::MemberDNE });
                },
            };

            let name = ctx.tokens.tok_or_string_to_string(&ast.objs.member(*field_member_id).name);

            let value_id = *match &ctx.objs.ref_get(runtime_ref).variant {
                ValueVariant::Record(map) => match map.get(&name) {
                    Some(id) => id,
                    None => {
                        return Err(RuntimeError { expr, variant: RuntimeErrorVariant::BadIdent });
                    }
                }
                _ => panic!(),
            };

            return Ok(RuntimeReference { scope: ctx.curr_scope, value_id });
        }
        _ => {
            return Err(RuntimeError {
                expr,
                variant: RuntimeErrorVariant::InvalidOperation,
            });
        }
    }
}

fn eval_operation_eq(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeReference, RuntimeError> {
}

fn eval_operation_binary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<RuntimeReference, RuntimeError> {
    let invalid_op = Err(RuntimeError {
        expr,
        variant: RuntimeErrorVariant::InvalidOperation,
    });

    let type_ids = [
        match ast.objs.expr(operand1).type_or_module {
            TypeOrModule::Type(t) => t,
            TypeOrModule::Module(_) => return invalid_op,
        },
        match ast.objs.expr(operand2).type_or_module {
            TypeOrModule::Type(t) => t,
            TypeOrModule::Module(_) => return invalid_op,
        },
    ];

    match op {
        TokenType::Period => return eval_operation_period(ast, ctx, expr, operand1, operand2),
        TokenType::Eq => return eval_operation_eq(ast, ctx, expr, operand1, operand2),
        _ => (),
    }

    let operand_refs = [eval(ast, ctx, operand1)?, eval(ast, ctx, operand2)?];
    let operand_values = [ctx.objs.ref_get(operand_refs[0]), ctx.objs.ref_get(operand_refs[1])];

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
    }.to_runtime_ref(ctx, ctx.curr_scope))
}
