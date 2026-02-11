use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprVariant, ScopeVariant, Type, TypeVariant,
        ast_contents::{ExprID, ScopeID, TypeID},
        errors::{InvalidOperation, SemanticError},
        operator,
        semantic::SemanticContext,
    },
    scan::TokenType,
};

use operator::OperatorVariant::*;

impl AST {
    fn analyze_operation_from(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand);

        if !self.expr(operand).finalized {
            return;
        }

        let operand_type_id = self.objs.expr(operand).type_id;

        match self.objs.type_get(operand_type_id) {
            Type::ImportTarget(_) => {
                self.invalid_operation(expr, "no nested from");
                return;
            }
            _ => (),
        }

        let expr_scope_id = match self.expr_get_scope_id(operand) {
            Some(scope_id) => scope_id,
            None => {
                self.invalid_operation(
                    expr,
                    "target of \"from\" does not have a corresponding scope to import from",
                );
                return;
            }
        };

        let scope_struct = self.objs.scope(expr_scope_id);

        match scope_struct.variant {
            ScopeVariant::Scope => {
                self.invalid_operation(expr, "cannot import from this scope");
                return;
            }
            ScopeVariant::Type(TypeVariant::Enum | TypeVariant::Variant) => (), // Ok
            ScopeVariant::Type(_) => {
                self.invalid_operation(expr, "you may only import from an enum or variant type");
                return;
            }
            ScopeVariant::Module | ScopeVariant::FileModule(_) => (), // Ok
        }

        let type_id = self.objs.type_push(Type::ImportTarget(expr_scope_id));

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = type_id;
        expr_mut.finalized = true;
    }

    fn analyze_operation_unary_import(
        &mut self,
        _ctx: &mut SemanticContext,
        _scope: ScopeID,
        expr: ExprID,
        _operand: ExprID,
    ) {
        self.invalid_operation(
            expr,
            "unary import e.g. \"import some_module\" not implemented yet",
        );
    }

    // loop control is e.g. break, continue
    pub fn analyze_operation_loop_control(
        &mut self,
        ctx: &mut SemanticContext,
        _scope: ScopeID,
        expr: ExprID,
        _op: TokenType,
        operand: ExprID,
    ) {
        if ctx.curr_loop.is_none() {
            self.invalid_operation(expr, "cannot do loop control (e.g. break, continue) outside of any loop");
            return;
        }

        match self.objs.expr(operand).variant {
            ExprVariant::Unit => { // Ok
                let operand_mut = self.expr_mut(operand);

                operand_mut.type_id = TypeID::unit();
                operand_mut.finalized = true;
            }

            // currently cannot provide argument (e.g. label) to break or continue
            _ => {
                self.invalid_operation(expr, "it is not supported to provide arg to break or continue");
                return;
            }
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = TypeID::unit();
        expr_mut.finalized = true;
    }

    pub fn analyze_operation_unary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        op: TokenType,
        operand: ExprID,
    ) {
        match op {
            TokenType::From => {
                self.analyze_operation_from(ctx, scope, expr, operand);
                return;
            }
            TokenType::Import => {
                self.analyze_operation_unary_import(ctx, scope, expr, operand);
                return;
            }
            _ => (),
        }

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

        match (op, &self.expr(operand).variant) {
            (TokenType::Type, ExprVariant::Unit) => {
                let expr_mut = self.expr_mut(expr);

                expr_mut.variant = ExprVariant::KWType;
                expr_mut.type_id = TypeID::any_type();
                expr_mut.finalized = true;

                return;
            }
            (TokenType::Module, ExprVariant::Unit) => {
                let expr_mut = self.expr_mut(expr);

                expr_mut.variant = ExprVariant::KWModule;
                expr_mut.type_id = TypeID::any_module();
                expr_mut.finalized = true;

                return;
            }
            _ => (),
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

                // TODO: for ++ and -- check if operand is var

                let type_variant = match self.type_get_variant(operand_type_id) {
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

                if !self.type_eq(curr_ret_type, operand_type_id) {
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
            TokenType::Break | TokenType::Continue => {
                self.analyze_operation_loop_control(ctx, scope, expr, op, operand);
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
