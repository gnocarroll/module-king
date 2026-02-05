mod binary;
mod unary;

use crate::{
    parse::{
        AST, Block, ExprVariant, Operation, ast_contents::{ExprID, ScopeID, TypeID}, errors::{InvalidOperation, SemanticError}, semantic::{AnalyzingNow, SemanticContext}
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
        let operand1 = operation.operand1.expect("should be LHS");
        let operand2 = operation.operand2.expect("should be RHS");

        match operation.op {
            TokenType::Comma => {
                let mut finalized = true;

                // scope remains same and still analyzing func params
                self.analyze_expr(ctx, scope, operand1);

                if !self.objs.expr(operand1).finalized {
                    finalized = false;
                }

                // it is fine for rhs to be missing e.g. function f(x: Integer,) ...

                if matches!(self.expr(operand2).variant, ExprVariant::Unit) {
                    let operand2_mut = self.expr_mut(operand2);

                    operand2_mut.type_id = TypeID::unit();
                    operand2_mut.finalized = true;
                } else {
                    self.analyze_expr(ctx, scope, operand2);

                    if !self.objs.expr(operand2).finalized {
                        finalized = false;
                    }
                }

                let expr_mut = self.expr_mut(expr);

                expr_mut.type_id = TypeID::unit();
                expr_mut.finalized = finalized;
            }
            TokenType::Colon => {
                let (pattern, err) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    Some(operand1),
                    Some(operand2),
                );

                let mut finalized = err.is_none();

                // add param
                let curr_func = ctx.curr_func.expect("should be current func recorded");

                let func_scope = self.objs.function(curr_func).scope;

                if self
                    .scope_create_members_from_pattern(ctx, func_scope, pattern)
                    .is_err()
                {
                    finalized = false;
                }

                let function_mut = self.objs.function_mut(curr_func);

                // check if expr is already associated with some param idx
                // if it is => set pattern there to whatever pattern we have now
                // if not => record param index this expr will correspond to and push pattern

                if let Some(param_idx) = function_mut.expr_to_param_idx.get(&expr).map(|idx| *idx) {
                    function_mut.params[param_idx] = pattern;
                } else {
                    function_mut
                        .expr_to_param_idx
                        .insert(expr, function_mut.params.len());

                    function_mut.params.push(pattern);
                }

                if finalized {
                    self.analyze_expr(ctx, scope, operand1);

                    if !self.objs.expr(operand1).finalized {
                        finalized = false;
                    }

                    let expr_mut = self.objs.expr_mut(expr);

                    expr_mut.type_id = TypeID::unit();
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
            TokenType::Semicolon | TokenType::Comma => {
                if operation.op == TokenType::Comma {
                    self.invalid_operation(
                        expr,
                        "members of type literal should be semicolon-separated",
                    );
                }

                let (operand1, operand2) = (
                    operation.operand1.expect("LHS should be present"),
                    operation.operand2.expect("RHS should be present"),
                );

                for operand in [operand1, operand2] {
                    // if second operand is Unit then ignore it (otherwise would cause error)

                    if operand == operand2
                        && matches!(self.objs.expr(operand2).variant, ExprVariant::Unit)
                    {
                        let operand2_mut = self.expr_mut(operand2);

                        operand2_mut.type_id = TypeID::unit();
                        operand2_mut.finalized = true;

                        break;
                    }

                    self.analyze_expr(ctx, scope, operand);
                }

                let finalized = self.expr(operand1).finalized && self.expr(operand2).finalized;

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::unit();
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

                let finalized = self
                    .scope_create_members_from_pattern(ctx, scope, pattern)
                    .is_ok()
                    && err.is_none();

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::unit();
                expr_mut.is_var = true;
                expr_mut.finalized = finalized;
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
