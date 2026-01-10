mod analyze_operation;
mod util;

use std::collections::HashMap;

use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, IdentifierVariant, Member, MemberVariant, Operation,
        Pattern, PatternVariant, Scope, ScopeRefersTo, ScopeVariant, TokenOrString, Tokens, Type,
        TypeOrModule, TypeVariant, Visibility,
        ast_contents::{ExprID, MemberID, PatternID, ScopeID, TypeID},
        errors::{
            ExpectedExprReturns, ExprAndType, InvalidOperation, MissingOperand, PatternError,
            SemanticError, UnexpectedExpr,
        },
        operator,
    },
    scan::TokenType,
};

#[derive(Clone, Copy, PartialEq)]
pub enum IsEnum {
    Enum,
    Other,
}

#[derive(Clone, Copy, PartialEq)]
pub enum AnalyzingNow {
    Type,
    TypeBody(IsEnum),
    FuncParams,
    Pattern, // e.g. (x, y) so certain ops not permitted
    Expr,    // if specification is unnecessary
}

pub struct SemanticContext<'a> {
    tokens: &'a Tokens<'a>,
    analyzing_now: AnalyzingNow,

    // scope id of current function
    curr_func: Option<ScopeID>,
}

use operator::OperatorVariant::*;

impl AST {
    fn scope_create_members_from_pattern(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        pattern: PatternID,
    ) {
        let mut pattern_stack = vec![pattern];

        while let Some(pattern) = pattern_stack.pop() {
            let pattern = pattern;

            let pattern_ref = self.objs.pattern(pattern);

            let type_id = pattern_ref.type_id;

            match pattern_ref.variant {
                PatternVariant::Ident(token) => {
                    // instance is added from identifier piece of pattern
                    self.scope_add_instance(ctx, scope, TokenOrString::Token(token), Some(type_id));
                }
                PatternVariant::Tuple((lhs, rhs)) | PatternVariant::Slice((lhs, rhs)) => {
                    if let Some(rhs) = rhs {
                        pattern_stack.push(rhs);
                    }

                    pattern_stack.push(lhs);
                }
                PatternVariant::RestOfTuple((lhs, rhs))
                | PatternVariant::RestOfSlice((lhs, rhs)) => {
                    pattern_stack.push(rhs);
                    pattern_stack.push(lhs);
                }
                _ => (),
            }
        }
    }

    // process provided pattern for func params
    // - append correct param information to func
    // - add correct instances to function scope
    fn pattern_process_for_func_params(&mut self, ctx: &mut SemanticContext, pattern: PatternID) {
        let func_scope = if let Some(curr_func) = ctx.curr_func {
            curr_func
        } else {
            return;
        };

        let func_expr = match self.objs.scope(func_scope).refers_to {
            Some(ScopeRefersTo::Expr(expr)) => expr,
            _ => {
                return;
            }
        };

        // add pattern to vec for func

        match &mut self.objs.expr_mut(func_expr).variant {
            ExprVariant::FunctionLiteral(func_literal) => func_literal.param_info.push(pattern),
            _ => (),
        }

        self.scope_create_members_from_pattern(ctx, func_scope, pattern);
    }

    // function to attempt pattern matching between identifier(s) in pattern and type
    // also should add any new identifiers to scope
    // ret should indicate what problems occurred if any
    fn pattern_matching(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        ident_expr: ExprID,

        type_id: TypeID,
    ) -> Result<PatternID, PatternError> {
        let mut ident_expr = ident_expr;

        let mut ident_has_parens = false;

        while match self.objs.expr(ident_expr).variant {
            ExprVariant::Operation(Operation {
                op: TokenType::LParen,
                operand1: Some(expr),
                operand2: None,
            }) => {
                ident_has_parens = true;
                ident_expr = expr;

                true
            }
            _ => false,
        } {}

        let type_val = self.objs.type_get(type_id).clone();

        match self.objs.expr(ident_expr).variant {
            ExprVariant::Identifier(ident) => {
                return Ok(self.objs.pattern_push(Pattern {
                    type_id: type_id,
                    variant: PatternVariant::Ident(ident.name),
                }));
            }
            // Tuple
            ExprVariant::Operation(Operation {
                op: TokenType::Comma,
                operand1: lhs,
                operand2: rhs,
            }) => {
                match type_val {
                    Type::Tuple((lhs_type, rhs_type)) => {
                        if !ident_has_parens {
                            return Err(self.pattern_error_push(PatternError::ParenMismatch(ExprAndType {
                                expr: ident_expr,
                                type_id,
                            })));
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        if rhs_type.is_some() && self.objs.expr(rhs).is_unit() {
                            return Err(self.pattern_error_push(PatternError::IdentMissing(rhs_type.expect("impossible"))));
                        } else if rhs_type.is_none() && !self.expr(rhs).is_unit() {
                            return Err(self.pattern_error_push(PatternError::TypeMissing(rhs)));
                        }

                        // first pattern match on lhs of tuple
                        let lhs_pattern = self.pattern_matching(ctx, scope, lhs, lhs_type)?;

                        let rhs_pattern = if let Some(rhs_type) = rhs_type {
                            Some(self.pattern_matching(ctx, scope, rhs, rhs_type)?)
                        } else {
                            None
                        };

                        return Ok(self.objs.pattern_push(Pattern {
                            type_id,
                            variant: PatternVariant::Tuple((lhs_pattern, rhs_pattern)),
                        }));
                    }
                    Type::RestOfTuple((lhs_type, rhs_type)) => {
                        if ident_has_parens {
                            return Err(self.pattern_error_push(PatternError::ParenMismatch(ExprAndType {
                                expr: ident_expr,
                                type_id,
                            })));
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        if self.objs.expr(rhs).is_unit() {
                            return Err(PatternError::IdentMissing(rhs_type));
                        }

                        // first pattern match on lhs of tuple
                        let lhs_pattern = self.pattern_matching(ctx, scope, lhs, lhs_type)?;

                        let rhs_pattern = self.pattern_matching(ctx, scope, rhs, rhs_type)?;

                        return Ok(self.objs.pattern_push(Pattern {
                            type_id,
                            variant: PatternVariant::RestOfTuple((lhs_pattern, rhs_pattern)),
                        }));
                    }
                    _ => (),
                }
            }
            _ => {}
        }

        Err(PatternError::MatchingUnsupported(ident_expr))
    }

    fn expected_expr_returns(
        &mut self,
        expr: ExprID,
        expected: ExprReturns,
        found: ExprReturns,
    ) -> SemanticError {
        let ret = SemanticError::ExpectedExprReturns(ExpectedExprReturns {
            expr,
            expected,
            found,
        });

        self.semantic_errors.push(ret.clone());

        ret
    }

    // ret pattern or error
    fn analyze_instance_creation(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: Option<ExprID>,
        operand2: Option<ExprID>,
    ) -> Result<PatternID, SemanticError> {
        let old_analyzing_now = ctx.analyzing_now;
        let mut err: Option<SemanticError> = None;

        if let Some(pattern) = operand1 {
            ctx.analyzing_now = AnalyzingNow::Pattern;
            self.analyze_expr(ctx, scope, pattern);
            ctx.analyzing_now = old_analyzing_now;
        } else {
            err = Some(self.missing_operand(expr, 1));
        }

        if let Some(pattern) = operand2 {
            ctx.analyzing_now = AnalyzingNow::Type;
            self.analyze_expr(ctx, scope, pattern);
            ctx.analyzing_now = old_analyzing_now;
        } else {
            err = Some(self.missing_operand(expr, 2));
        }

        let mut finalized = false;
        let mut pattern = PatternID::default();

        if let (Some(ident_expr), Some(type_expr)) = (operand1, operand2) {
            let type_expr_ref = self.objs.expr(type_expr);

            if type_expr_ref.finalized {
                // ensure that analyze "type expr" is actually a type

                let expr_returns = type_expr_ref.expr_returns;

                if expr_returns != ExprReturns::Type {
                    return Err(self.expected_expr_returns(expr, ExprReturns::Type, expr_returns));
                }

                let type_id = match type_expr_ref.type_or_module {
                    TypeOrModule::Type(t) => t,
                    TypeOrModule::Module(_) => {
                        return Err(self.expected_expr_returns(
                            expr,
                            ExprReturns::Type,
                            expr_returns,
                        ));
                    }
                };

                // use pattern matching on our operation of the form
                // ident_expr : type
                // type_id is id of said type

                let pattern_result = self.pattern_matching(ctx, scope, ident_expr, type_id);

                match pattern_result {
                    Ok(value) => {
                        finalized = true;
                        pattern = value;
                    }
                    Err(e) => err = Some(SemanticError::PatternError(e)),
                }
            }
        }

        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_or_module = TypeOrModule::Type(unit_type);
        expr_mut.expr_returns = ExprReturns::Unit;
        expr_mut.finalized = finalized;

        if let Some(err) = err {
            return Err(err);
        }

        Ok(pattern)
    }

    fn analyze_type_def(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        name: ExprID,
        value: ExprID,
    ) {
    }

    fn semantic_analyze_func(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let func_literal = match &self.objs.expr(expr).variant {
            ExprVariant::FunctionLiteral(f) => f.clone(),
            _ => panic!(),
        };

        // create function scope as child of parent then use it later on

        let func_scope = self.objs.scope_push(Scope {
            name: None,
            variant: ScopeVariant::Scope,
            parent_scope: scope,
            refers_to: Some(ScopeRefersTo::Expr(expr)), // connect to function literal
            members: HashMap::new(),
        });

        // record current function scope in context
        ctx.curr_func = Some(func_scope);

        ctx.analyzing_now = AnalyzingNow::FuncParams;
        self.analyze_expr(ctx, func_scope, func_literal.params);

        ctx.analyzing_now = AnalyzingNow::Type;
        self.analyze_expr(ctx, func_scope, func_literal.return_type);

        ctx.analyzing_now = AnalyzingNow::Expr;
        self.analyze_expr(ctx, func_scope, func_literal.body);
    }

    fn semantic_analyze_type_literal(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
    ) {
        let type_literal = match &self.objs.expr(expr).variant {
            ExprVariant::TypeLiteral(t) => t.clone(),
            _ => panic!(),
        };

        let type_scope = self.objs.scope_push(Scope {
            name: None,
            variant: ScopeVariant::Type(type_literal.variant),
            parent_scope: scope,
            refers_to: None,
            members: HashMap::new(),
        });

        let type_id = self.type_push_scope(type_scope);

        let old_analyzing_now = ctx.analyzing_now;

        // analyze body of type and use scope created for type

        // enum parsing is different (comma-separated values)
        ctx.analyzing_now = AnalyzingNow::TypeBody(match type_literal.variant {
            TypeVariant::Enum => IsEnum::Enum,
            _ => IsEnum::Other,
        });
        self.analyze_expr(ctx, type_scope, type_literal.body);
        ctx.analyzing_now = old_analyzing_now;

        // type of named type literal is Unit (cannot assign it to something)
        // type of unnamed type literal is whatever type in question is

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_or_module = TypeOrModule::Type(type_id);
        expr_mut.expr_returns = ExprReturns::Type;
        expr_mut.finalized = true;
    }

    fn test_analyzing_now(
        &self,
        analyzing_now: AnalyzingNow,
        allowed: &[AnalyzingNow],
        expr: ExprID,
    ) -> Result<(), UnexpectedExpr> {
        if allowed.iter().any(|curr| *curr == analyzing_now) {
            Ok(())
        } else {
            Err(UnexpectedExpr { expr })
        }
    }

    // semantic analysis on particular expression
    fn analyze_expr(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        match &self.expr(expr).variant {
            ExprVariant::Unit
            | ExprVariant::IntegerLiteral(_)
            | ExprVariant::FloatLiteral(_)
            | ExprVariant::StringLiteral(_) => {
                if self
                    .test_analyzing_now(ctx.analyzing_now, &[AnalyzingNow::Expr], expr)
                    .is_err()
                {
                    return;
                }

                // find built-in type id and set type of expr

                let type_name = match self.expr(expr).variant {
                    ExprVariant::Unit => UNIT_TYPE,
                    ExprVariant::IntegerLiteral(_) => INTEGER_TYPE,
                    ExprVariant::FloatLiteral(_) => FLOAT_TYPE,
                    ExprVariant::StringLiteral(_) => STRING_TYPE,
                    _ => panic!("only unit, integer, float here"),
                };

                let member = self.get_builtin_type(type_name);

                let type_id = self.objs.member(member).type_or_module.clone();

                let expr = self.expr_mut(expr);

                if type_name != UNIT_TYPE {
                    expr.expr_returns = ExprReturns::Value;
                }

                expr.type_or_module = type_id;
                expr.finalized = true;
            }
            ExprVariant::Identifier(ident) => {
                let name = ctx.tokens.tok_as_str(&ident.name);

                if let Some(member) = self.scope_search(scope, name) {
                    let member = self.objs.member(member);

                    let etype = member.type_or_module.clone();
                    let ident_variant = match member.variant {
                        MemberVariant::Module => IdentifierVariant::Module,
                        MemberVariant::Type => IdentifierVariant::Type,
                        MemberVariant::Instance => IdentifierVariant::Instance,
                    };

                    let expr = self.expr_mut(expr);

                    expr.type_or_module = etype;

                    if let ExprVariant::Identifier(ident) = &mut expr.variant {
                        ident.variant = ident_variant;
                    }

                    expr.is_var = true;
                    expr.finalized = true;
                }
            }
            ExprVariant::FunctionLiteral(_) => {
                if self
                    .test_analyzing_now(ctx.analyzing_now, &[AnalyzingNow::Expr], expr)
                    .is_err()
                {
                    return;
                }

                self.semantic_analyze_func(ctx, scope, expr);
            }
            ExprVariant::TypeLiteral(_) => {
                if self
                    .test_analyzing_now(
                        ctx.analyzing_now,
                        &[AnalyzingNow::Expr, AnalyzingNow::Type],
                        expr,
                    )
                    .is_err()
                {
                    return;
                }

                self.semantic_analyze_type_literal(ctx, scope, expr);
            }
            ExprVariant::Operation(_) => {
                self.analyze_operation(ctx, scope, expr);
            }
            _ => (),
        }
    }

    // public function to do semantic analysis
    pub fn do_semantic_analysis(&mut self, tokens: &Tokens, module_name: &str) {
        if let Some(expr) = self.root_expr {
            // create global scope and add built-ins

            let mut ctx = SemanticContext {
                tokens: tokens,
                analyzing_now: AnalyzingNow::Expr,
                curr_func: None,
            };

            let global_scope = self.objs.scope_push(Scope {
                name: Some(TokenOrString::String("GLOBAL".to_string())),
                variant: ScopeVariant::Module,
                parent_scope: ScopeID::default(),
                refers_to: None,
                members: HashMap::new(),
            });

            for (name, variant) in [
                (ERROR_TYPE, TypeVariant::Error),
                (INTEGER_TYPE, TypeVariant::Integer),
                (FLOAT_TYPE, TypeVariant::Float),
                (UNIT_TYPE, TypeVariant::Unit),
                (BOOLEAN_TYPE, TypeVariant::Enum),
                (STRING_TYPE, TypeVariant::String),
            ] {
                self.scope_add_member_type(
                    &mut ctx,
                    global_scope,
                    TokenOrString::String(name.to_string()),
                    variant,
                );
            }

            // add false, true to Boolean type

            let boolean_type = self.get_builtin_type_id(BOOLEAN_TYPE);

            match self.objs.type_get(boolean_type).clone() {
                Type::Scope(scope) => {
                    for name in ["false", "true"] {
                        let member = self.objs.member_push(Member {
                            name: TokenOrString::String(name.to_string()),
                            visibility: Visibility::Global,
                            variant: MemberVariant::Instance,
                            type_or_module: TypeOrModule::Type(boolean_type),
                        });

                        self.scope_add_member(&mut ctx, scope, member);
                    }
                }
                _ => panic!("boolean type malformed"),
            }

            // create new scope for module

            let module_scope = self.objs.scope_push(Scope {
                name: Some(TokenOrString::String(module_name.to_string())),
                variant: ScopeVariant::Module,
                parent_scope: global_scope,
                refers_to: None,
                members: HashMap::new(),
            });

            // analyze root expr and provided new scope as scope

            self.analyze_expr(&mut ctx, module_scope, expr);
        };
    }
}
