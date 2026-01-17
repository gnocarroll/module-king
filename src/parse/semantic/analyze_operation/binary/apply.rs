use crate::{
    constants::ERROR_TYPE,
    parse::{
        AST, Type,
        ast_contents::{ExprID, FunctionID, ScopeID, TypeID},
        semantic::SemanticContext,
    },
};

// if doing apply operator (e.g. "f()") what are you doing
// (e.g. are you calling a function or casting to other type)
#[derive(Clone, Copy, PartialEq)]
enum ApplyCase {
    Cast(TypeID),
    Function(FunctionID),
}

impl AST {
    // apply a function e.g. f()
    // TokenType is actually LParen
    pub fn analyze_operation_apply(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand1);

        let old_analyzing_now = ctx.analyzing_now;

        // ctx.analyzing_now = AnalyzingNow::FuncArgs;
        self.analyze_expr(ctx, scope, operand2);
        ctx.analyzing_now = old_analyzing_now;

        eprintln!(
            "{}",
            self.type_to_string(ctx.tokens, self.objs.expr(operand2).type_id)
        );

        panic!("PRINTED");

        let mut finalized = true;

        let operand1_struct = self.expr(operand1);

        let mut ret_type = self.get_builtin_type_id(ERROR_TYPE);
        let apply_case;

        match (
            operand1_struct.finalized,
            self.objs.type_get(operand1_struct.type_id),
        ) {
            (false, _) => (),

            // type cast
            (true, Type::Type(t)) => {
                apply_case = ApplyCase::Cast(*t);
                ret_type = *t;
            }

            // function call, check if type is function
            (true, Type::Function((_, ret_t))) => {
                let function_id = self
                    .expr_get_function_id(operand1)
                    .expect("expr should be guaranteed to be function");

                apply_case = ApplyCase::Function(function_id);
                ret_type = *ret_t;
            }
            _ => {
                self.invalid_operation(expr, "should be type cast or function call");
                finalized = false;
                return;
            }
        }

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            finalized = false;
        }

        if finalized {
            // TODO: analyze if apply operation can actually be done
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.finalized = finalized;
        expr_mut.type_id = ret_type;
    }
}
