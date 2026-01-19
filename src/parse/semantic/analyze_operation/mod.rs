mod binary;
mod unary;

use crate::{
    parse::{
        AST, ExprVariant, Operation,
        ast_contents::{ExprID, ScopeID},
        errors::{InvalidOperation, SemanticError},
        semantic::{AnalyzingNow, IsEnum, SemanticContext},
    },
    scan::TokenType,
};

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
                let mut finalized = true;

                if let Some(lhs) = operation.operand1 {
                    // scope remains same and still analyzing func params
                    self.analyze_expr(ctx, scope, lhs);

                    if !self.objs.expr(lhs).finalized {
                        finalized = false;
                    }
                } else {
                    // should be param on lhs
                    finalized = false;

                    self.missing_operand(expr, 1);
                }

                // it is fine for rhs to be missing e.g. function f(0 : Integer,) ...
                if let Some(rhs) = operation.operand2 {
                    self.analyze_expr(ctx, scope, rhs);

                    if !self.objs.expr(rhs).finalized {
                        finalized = false;
                    }
                }

                self.set_expr_returns_unit(ctx, expr);

                let expr_mut = self.expr_mut(expr);

                expr_mut.finalized = finalized;
            }
            TokenType::Colon => {
                let (pattern, err) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    operation.operand1,
                    operation.operand2,
                );

                let finalized = err.is_none();

                // add param
                let curr_func = ctx.curr_func.expect("should be current func recorded");

                let func_scope = self.objs.function(curr_func).scope;

                self.scope_create_members_from_pattern(ctx, func_scope, pattern);

                self.objs.function_mut(curr_func).params.push(pattern);

                if finalized {
                    self.analyze_expr(ctx, scope, operation.operand1.expect("should be LHS"));

                    self.set_expr_returns_unit(ctx, expr);

                    let expr_mut = self.objs.expr_mut(expr);

                    expr_mut.finalized = finalized;
                }
            }
            TokenType::Eq | TokenType::ColonEq => {
                // arg with default provided

                self.invalid_operation(expr, "default values for params not implemented");
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

                match self.objs.expr(child_expr).variant {
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
                if operation.op == TokenType::Comma {
                    self.invalid_operation(
                        expr,
                        "members of type literal should be semicolon-separated",
                    );
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
                let (pattern, _) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    operation.operand1,
                    operation.operand2,
                );

                self.scope_create_members_from_pattern(ctx, scope, pattern);
            }
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
