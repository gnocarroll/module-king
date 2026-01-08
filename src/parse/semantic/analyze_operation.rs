use crate::{
    constants::BOOLEAN_TYPE,
    parse::{
        AST, ExprReturns, ExprVariant, Operation, ScopeVariant, Type, TypeOrModule, TypeVariant,
        ast_contents::{ExprID, ScopeID, TypeID},
        errors::{InvalidOperation, SemanticError},
        operator,
        semantic::{AnalyzingNow, IsEnum, SemanticContext},
    },
    scan::TokenType,
};

use operator::OperatorVariant::*;

impl AST {
    fn analyze_operation_func_params(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operation: Operation,
    ) {
        match operation.op {
            TokenType::Comma => {
                if let Some(lhs) = operation.operand1 {
                    // scope remains same and still analyzing func params
                    self.analyze_expr(ctx, scope, lhs);
                } else {
                    // should be param on lhs
                    self.missing_operand(expr, 1);
                }

                // it is fine for rhs to be missing e.g. function f(0 : Integer,) ...
                if let Some(rhs) = operation.operand2 {
                    self.analyze_expr(ctx, scope, rhs);
                }
            }
            TokenType::Colon => {
                if let Ok(pattern) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    operation.operand1,
                    operation.operand2,
                ) {}
            }
            TokenType::Eq | TokenType::ColonEq => {
                // arg with default provided
            }
            _ => {
                self.invalid_operation(
                    expr,
                    "cannot perform this operation in/for a function parameter",
                );
            }
        }
    }

    fn analyze_operation_type_body(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operation: Operation,
    ) {
        match operation.op {
            TokenType::Begin => {
                // Block
                let child_expr = operation
                    .operand1
                    .expect("Block should always contain Expr");

                match self.objs.expr(expr).variant {
                    ExprVariant::Operation(Operation {
                        op: TokenType::Begin,
                        ..
                    }) => {
                        self.semantic_errors.push(SemanticError::InvalidOperation(
                            InvalidOperation {
                                operation: child_expr,
                                msg: "no nested blocks in type body",
                            },
                        ));
                        return;
                    }
                    _ => (),
                }

                self.analyze_expr(ctx, scope, child_expr);
            }
            TokenType::Semicolon | TokenType::Comma => {
                match (operation.op, ctx.analyzing_now) {
                    (TokenType::Semicolon, AnalyzingNow::TypeBody(IsEnum::Enum))
                    | (TokenType::Comma, AnalyzingNow::TypeBody(IsEnum::Other)) => {
                        self.invalid_operation(
                            expr,
                            "members of enum should be comma-separated, otherwise use semicolons",
                        );
                    }
                    _ => (),
                }

                for operand in [operation.operand1, operation.operand2] {
                    // if second operand is Unit then ignore it (otherwise would cause error)

                    if operand == operation.operand2
                        && match self
                            .objs
                            .expr(operation.operand2.expect("RHS should be present"))
                            .variant
                        {
                            ExprVariant::Unit => true,
                            _ => false,
                        }
                    {
                        break;
                    }

                    self.analyze_expr(
                        ctx,
                        scope,
                        operand.expect("both operands should always be present here"),
                    );
                }
            }
            TokenType::Colon => {
                self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    operation.operand1,
                    operation.operand2,
                );
            }
            _ => {}
        }
    }

    fn analyze_operation_unary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        op: TokenType,
        operand: ExprID,
    ) {
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

        let operand_type_or_module = self.expr(operand).type_or_module.clone();

        // can surround type or module with parentheses

        if op == TokenType::LParen {
            let expr_returns = self.expr(operand).expr_returns;

            let expr_mut = self.objs.expr_mut(expr);

            expr_mut.expr_returns = expr_returns;
            expr_mut.type_or_module = operand_type_or_module;
            expr_mut.finalized = true;

            return;
        }

        let operand_type = match self.expr(operand).type_or_module {
            TypeOrModule::Type(t) => t,
            TypeOrModule::Module(_) => {
                self.invalid_operation(expr, "unary operation may not be applied to a module");
                return;
            }
        };

        match self.expr(operand).expr_returns {
            ExprReturns::Module => {
                self.invalid_operation(expr, "unary operation may not be applied to a module");
                return;
            }
            ExprReturns::Unit => {
                self.invalid_operation(expr, "unary operation may not be applied to Unit");
                return;
            }
            ExprReturns::Type => {
                let type_id = match op {
                    TokenType::Star => self.objs.type_push(Type::Ptr(operand_type)),
                    TokenType::Ampersand => self.objs.type_push(Type::Ref(operand_type)),
                    _ => {
                        self.invalid_operation(expr, "this operation is not supported for types");
                        return;
                    }
                };

                let expr_mut = &mut self.objs.expr_mut(expr);

                expr_mut.expr_returns = ExprReturns::Type;
                expr_mut.type_or_module = TypeOrModule::Type(type_id);
                expr_mut.finalized = true;

                return;
            }
            _ => (),
        }

        match op {
            TokenType::Plus | TokenType::Minus | TokenType::PlusPlus | TokenType::MinusMinus => {
                let err_msg = "this unary operation is only supported for integers and floats";

                let type_variant = match self.objs.type_get(operand_type) {
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

                let (expr_type, expr_returns) = match op {
                    TokenType::Plus | TokenType::Minus => (operand_type, ExprReturns::Value),
                    _ => (TypeID::default(), ExprReturns::Unit),
                };

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_or_module = TypeOrModule::Type(expr_type);
                expr_mut.expr_returns = expr_returns;
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
                let expr_type = self.objs.type_push(Type::Ref(operand_type));

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_or_module = TypeOrModule::Type(expr_type);
                expr_mut.expr_returns = ExprReturns::Value;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Star => {
                // i.e. deref
                let err_msg = "you may only dereference a pointer or reference";

                let expr_type = match self.objs.type_get(operand_type) {
                    Type::Ptr(t) | Type::Ref(t) => *t,
                    _ => {
                        self.invalid_operation(expr, err_msg);
                        return;
                    }
                };

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_or_module = TypeOrModule::Type(expr_type);
                expr_mut.expr_returns = ExprReturns::Value;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Tilde => {
                let err_msg = "this unary operation is only supported for integers";

                let type_variant = match self.objs.type_get(operand_type) {
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

                expr_mut.type_or_module = TypeOrModule::Type(operand_type);
                expr_mut.expr_returns = ExprReturns::Value;
                expr_mut.finalized = true;

                return;
            }
            TokenType::Bang => {
                let err_msg = "logical NOT may only be applied to a boolean expression";

                // check if operand is a boolean

                let boolean_type = self.get_builtin_type_id(BOOLEAN_TYPE);

                if operand_type != boolean_type {
                    self.invalid_operation(expr, err_msg);
                    return;
                }

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_or_module = TypeOrModule::Type(boolean_type);
                expr_mut.expr_returns = ExprReturns::Value;
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

    fn analyze_operation_binary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        op: TokenType,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        // both operands must already be finalized

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            return;
        }

        if operator::get_bp(op, Infix).is_none() && operator::get_bp(op, PostfixAround).is_none() {
            self.invalid_operation(expr, "not a supported binary operator");
            return;
        }

        match op {
            TokenType::Plus => {}
            TokenType::Minus => {}
            TokenType::Star => {}
            TokenType::FSlash => {}
            TokenType::Percent => {}
            _ => {}
        }
    }

    pub fn analyze_operation(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let operation = match &self.objs.expr(expr).variant {
            ExprVariant::Operation(operation) => operation.clone(),
            _ => panic!(),
        };

        let mut return_early = true;

        match ctx.analyzing_now {
            AnalyzingNow::FuncParams => {
                self.analyze_operation_func_params(ctx, scope, expr, operation);
            }
            AnalyzingNow::TypeBody(_) => {
                self.analyze_operation_type_body(ctx, scope, expr, operation);
            }
            _ => {
                return_early = false;
            }
        }

        if return_early {
            return;
        }

        let mut operands_finalized = true;

        for operand in [operation.operand1, operation.operand2] {
            if let Some(operand) = operand {
                self.analyze_expr(ctx, scope, operand);

                if !self.objs.expr(operand).finalized {
                    operands_finalized = false;
                }
            }
        }

        if !operands_finalized {
            return;
        }

        match operation.op {
            TokenType::Type => {
                self.analyze_type_def(
                    ctx,
                    scope,
                    expr,
                    operation.operand1.expect("typedef missing lhs"),
                    operation.operand2.expect("typedef missing rhs"),
                );
                return;
            }
            _ => (),
        }

        // Now take care of most general cases of operations

        match (operation.operand1, operation.operand2) {
            (Some(operand), None) => {
                self.analyze_operation_unary(ctx, scope, expr, operation.op, operand);
            }
            (Some(operand1), Some(operand2)) => {
                self.analyze_operation_binary(ctx, scope, expr, operation.op, operand1, operand2);
            }
            _ => self
                .semantic_errors
                .push(SemanticError::InvalidOperation(InvalidOperation {
                    operation: expr,
                    msg: "operation has no operands",
                })),
        }
    }
}
