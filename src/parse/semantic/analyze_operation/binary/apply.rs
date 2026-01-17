use crate::{
    constants::ERROR_TYPE,
    parse::{
        AST, ScopeVariant, Type, TypeVariant, ast_contents::{ExprID, FunctionID, ScopeID, TypeID}, semantic::SemanticContext
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

        let mut finalized = true;

        let operand1_struct = self.expr(operand1);

        let mut apply_case = ApplyCase::Cast(TypeID::error());

        match (
            operand1_struct.finalized,
            self.objs.type_get(operand1_struct.type_id),
        ) {
            (false, _) => (),

            // type cast
            (true, Type::Type(t)) => {
                apply_case = ApplyCase::Cast(*t);
            }

            // function call, check if type is function
            (true, Type::Function(_)) => {
                let function_id = self
                    .expr_get_function_id(operand1)
                    .expect("expr should be guaranteed to be function");

                apply_case = ApplyCase::Function(function_id);
            }
            _ => {
                self.invalid_operation(expr, "should be type cast or function call");
                finalized = false;
            }
        }

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            finalized = false;
        }

        if finalized {
            match apply_case {
                ApplyCase::Cast(cast_to) => {
                    self.analyze_cast(ctx, scope, expr, cast_to, operand2);
                }
                ApplyCase::Function(function) => {
                    self.analyze_function_call(ctx, scope, expr, function, operand2);
                }
            }
        }
    }

    // ret value indicates success
    pub fn analyze_cast(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        cast_to: TypeID,
        args: ExprID, // args
    ) {
        let cast_to = self.type_resolve_aliasing(cast_to);

        let arg_type = self.type_resolve_aliasing(self.objs.expr(args).type_id);

        if cast_to == TypeID::error() || arg_type == TypeID::error() {
            return;
        }

        // simple cast means args type must match type being casted to

        let mut simple_cast = true;

        match self.objs.type_get(cast_to) {
            Type::Type(_) | Type::Unit => {
                self.invalid_operation(expr, "cannot cast to this type");
                return;
            }
            Type::Scope(scope_id) => {
                match self.objs.scope(*scope_id).variant {
                    ScopeVariant::Type(TypeVariant::Integer) => {
                        simple_cast = false;

                        // can be some other integer type being casted

                        match self.type_get_variant(arg_type) {
                            Some(TypeVariant::Integer) | Some(TypeVariant::Float) => (),
                            _ => {
                                self.invalid_operation(expr, "when casting to integer type argument must be integer or float type");
                                return;
                            }
                        }
                    }
                    ScopeVariant::Type(TypeVariant::Float) => {
                        simple_cast = false;

                        // can be some other float type being casted

                        match self.type_get_variant(arg_type) {
                            Some(TypeVariant::Integer) | Some(TypeVariant::Float) => (),
                            _ => {
                                self.invalid_operation(expr, "when casting to float type argument must be integer or float type");
                                return;
                            }
                        }
                    }
                    ScopeVariant::Type(TypeVariant::Record) => {
                        simple_cast = false;

                        // TODO: args should be provided in same order as record fields
                    }
                    _ => (),
                }
            }
            _ => (),
        }

        if simple_cast && cast_to != arg_type {
            self.invalid_operation(expr, "type of args does not match type you are casting to");
            return;
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = cast_to;
    }

    // ret value indicates success
    pub fn analyze_function_call(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        function: FunctionID,
        args: ExprID, // args
    ) {
        let function = self.objs.function(function);

        let expr_type_id = function.return_type;

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = expr_type_id;
    }
}
