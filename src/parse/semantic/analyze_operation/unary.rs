use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, UNIT_TYPE},
    parse::{
        AST, ScopeVariant, Type, TypeVariant,
        ast_contents::{ExprID, ScopeID},
        errors::{InvalidOperation, SemanticError},
        operator,
        semantic::SemanticContext,
    },
    scan::TokenType,
};

use operator::OperatorVariant::*;

impl AST {
    pub fn analyze_operation_unary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        op: TokenType,
        operand: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand);

        if !self.expr(operand).finalized {
            return;
        }

        if operator::get_bp(op, Prefix).is_none()
            && operator::get_bp(op, Postfix).is_none()
            && operator::get_bp(op, Around).is_none()
        {
            self.invalid_operation(expr, "not a supported unary operator");
            return;
        }

        let operand_struct = self.expr(operand);

        let operand_is_var = operand_struct.is_var;
        let operand_type_id = operand_struct.type_id;

        // can surround type or module with parentheses

        if op == TokenType::LParen {
            let expr_mut = self.objs.expr_mut(expr);

            expr_mut.type_id = operand_type_id;
            expr_mut.is_var = operand_is_var;
            expr_mut.finalized = true;

            return;
        }

        match self.objs.type_get(operand_type_id) {
            Type::Module(_) => {
                self.invalid_operation(expr, "unary operation may not be applied to a module");
                return;
            }
            Type::Type(t) => {
                let expr_returns = match op {
                    TokenType::Star => self.objs.type_push(Type::Ptr(*t)),
                    TokenType::Ampersand => self.objs.type_push(Type::Ref(*t)),
                    _ => {
                        self.invalid_operation(expr, "this operation is not supported for types");
                        return;
                    }
                };

                let type_id = self.objs.type_push(Type::Type(expr_returns));

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = type_id;
                expr_mut.finalized = true;

                return;
            }
            _ => (),
        }

        match op {
            TokenType::Plus | TokenType::Minus | TokenType::PlusPlus | TokenType::MinusMinus => {
                let err_msg = "this unary operation is only supported for integers and floats";

                let type_variant = match self.objs.type_get(operand_type_id) {
                    Type::Scope(scope) => match self.objs.scope(*scope).variant {
                        ScopeVariant::Type(variant) => Some(variant),
                        _ => None,
                    },
                    _ => None,
                };

                let type_variant = match type_variant {
                    Some(v) => v,
                    None => {
                        self.invalid_operation(expr, err_msg);
                        return;
                    }
                };

                if type_variant != TypeVariant::Integer && type_variant != TypeVariant::Float {
                    self.invalid_operation(expr, err_msg);
                    return;
                }

                let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);

                let expr_type = match op {
                    TokenType::Plus | TokenType::Minus => operand_type_id,
                    _ => unit_type_id,
                };

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = expr_type;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Ampersand => {
                let err_msg = "you may only create a reference to a variable";

                if !self.expr(operand).is_var {
                    self.invalid_operation(expr, err_msg);
                    return;
                }

                // type of finalized type is reference to whatever type operand is
                let expr_type = self.objs.type_push(Type::Ref(operand_type_id));

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = expr_type;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Star => {
                // i.e. deref
                let err_msg = "you may only dereference a pointer or reference";

                let expr_type = match self.objs.type_get(operand_type_id) {
                    Type::Ptr(t) | Type::Ref(t) => *t,
                    _ => {
                        self.invalid_operation(expr, err_msg);
                        return;
                    }
                };

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = expr_type;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Tilde => {
                let err_msg = "this unary operation is only supported for integers";

                let type_variant = match self.objs.type_get(operand_type_id) {
                    Type::Scope(scope) => match self.objs.scope(*scope).variant {
                        ScopeVariant::Type(variant) => Some(variant),
                        _ => None,
                    },
                    _ => None,
                };

                let type_variant = match type_variant {
                    Some(v) => v,
                    None => {
                        self.invalid_operation(expr, err_msg);
                        return;
                    }
                };

                if type_variant != TypeVariant::Integer {
                    self.invalid_operation(expr, err_msg);
                    return;
                }

                let expr_mut = &mut self.objs.expr_mut(expr);

                expr_mut.type_id = operand_type_id;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Bang => {
                let err_msg = "logical NOT may only be applied to a boolean expression";

                // check if operand is a boolean

                let boolean_type = self.get_builtin_type_id(BOOLEAN_TYPE);

                if operand_type_id != boolean_type {
                    self.invalid_operation(expr, err_msg);
                    return;
                }

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = boolean_type;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Return => {
                if ctx.curr_func.is_none() {
                    self.invalid_operation(
                        expr,
                        "cannot have return expression outside of any function",
                    );
                    return;
                }

                let err_type = self.get_builtin_type_id(ERROR_TYPE);
                let curr_ret_type = self.get_curr_return_type(ctx);

                if curr_ret_type == err_type {
                    return;
                }

                if curr_ret_type != operand_type_id {
                    self.invalid_operation(
                        expr,
                        "type of return operand must match function return type",
                    );
                    return;
                }

                self.set_expr_returns_unit(ctx, expr);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.finalized = true;

                return;
            }
            TokenType::Begin => {
                self.set_expr_returns_unit(ctx, expr);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.finalized = true;

                return;
            }
            _ => (),
        }

        self.semantic_errors
            .push(SemanticError::InvalidOperation(InvalidOperation {
                operation: expr,
                msg: "this unary operator is not supported for this type",
            }));
    }
}
