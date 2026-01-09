use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::{
    parse::{
        AST, ExprVariant, Operation, TypeOrModule,
        ast_contents::{ExprID, TypeID},
    },
    run::{
        ExecutionContext, Value, ValueVariant,
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
) -> Result<Value, RuntimeError> {
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
) -> Result<Value, RuntimeError> {
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
    let operand_value = eval(ast, ctx, operand)?;

    let ret = match (op, &operand_value.variant) {
        // unary +
        (TokenType::Plus, ValueVariant::Integer(_) | ValueVariant::Float(_)) => {
            // no-op
            return Ok(operand_value);
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
    })
}

fn eval_operation_binary(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    op: TokenType,
    operand1: ExprID,
    operand2: ExprID,
) -> Result<Value, RuntimeError> {
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

    let operand_values = [eval(ast, ctx, operand1)?, eval(ast, ctx, operand2)?];

    let i64_pow = |lhs: i64, rhs: i64| lhs.pow(rhs as u32);

    let integer_op = |ttype: TokenType| -> Option<fn(i64, i64) -> i64> {
        match ttype {
            TokenType::Plus => Some(i64::add),
            TokenType::Minus => Some(i64::sub),
            TokenType::Star => Some(i64::mul),
            TokenType::FSlash => Some(i64::div),
            TokenType::Percent => Some(i64::rem),
            TokenType::StarStar => Some(i64_pow),
            _ => None,
        }
    };

    let ret = match (&operand_values[0].variant, &operand_values[1].variant) {
        (ValueVariant::Integer(lhs), ValueVariant::Integer(rhs)) => {
            if let Some(i64_op) = integer_op(op) {
                ValueVariant::Integer(i64_op(*lhs, *rhs))
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
    })
}
