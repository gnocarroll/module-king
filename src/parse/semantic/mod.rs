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
            ExpectedExprReturns, ExprAndType, InvalidExpr, InvalidOperation, MissingOperand,
            PatternError, SemanticError, UnexpectedExpr,
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

        type_id: Option<TypeID>,
    ) -> (PatternID, Option<PatternError>) {
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

        // ensure we always have a type ID by getting ERROR_TYPE id if no type id
        // was passed to this function

        let err_type_id = self.get_builtin_type_id(ERROR_TYPE);

        let type_id = type_id.unwrap_or(err_type_id);

        let mut pattern_err: Option<PatternError> = None;

        if type_id == err_type_id {
            pattern_err = Some(self.pattern_error_push(PatternError::TypeMissing(ident_expr)));
        }

        let type_val = self.objs.type_get(type_id).clone();

        match self.objs.expr(ident_expr).variant {
            ExprVariant::Identifier(ident) => {
                return (
                    self.objs.pattern_push(Pattern {
                        type_id: type_id,
                        variant: PatternVariant::Ident(ident.name),
                    }),
                    pattern_err,
                );
            }
            // Tuple
            ExprVariant::Operation(Operation {
                op: TokenType::Comma,
                operand1: lhs,
                operand2: rhs,
            }) => {
                match type_val {
                    Type::Tuple((lhs_type, rhs_type)) => {
                        let mut lhs_type = Some(lhs_type);
                        let mut rhs_type = rhs_type;

                        if !ident_has_parens {
                            pattern_err = Some(self.pattern_error_push(
                                PatternError::ParenMismatch(ExprAndType {
                                    expr: ident_expr,
                                    type_id,
                                }),
                            ));

                            // due to missing parentheses on lhs (non-type side) cannot match types, so replace
                            // types with None

                            lhs_type = None;
                            rhs_type = None;
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        let rhs_is_unit = self.objs.expr(rhs).is_unit();

                        if rhs_type.is_some() && rhs_is_unit {
                            pattern_err = Some(self.pattern_error_push(
                                PatternError::IdentMissing(rhs_type.expect("impossible")),
                            ));
                        } else if rhs_type.is_none() && !rhs_is_unit {
                            pattern_err =
                                Some(self.pattern_error_push(PatternError::TypeMissing(rhs)));
                        }

                        // first pattern match on lhs of tuple
                        let (lhs_pattern, err) = self.pattern_matching(ctx, scope, lhs, lhs_type);

                        if let Some(err) = err {
                            pattern_err = Some(err);
                        }

                        let rhs_pattern = if !rhs_is_unit {
                            let (pattern, err) = self.pattern_matching(ctx, scope, rhs, rhs_type);

                            if let Some(err) = err {
                                pattern_err = Some(err);
                            }

                            Some(pattern)
                        } else {
                            None
                        };

                        return (
                            self.objs.pattern_push(Pattern {
                                type_id,
                                variant: PatternVariant::Tuple((lhs_pattern, rhs_pattern)),
                            }),
                            pattern_err,
                        );
                    }
                    Type::RestOfTuple((lhs_type, rhs_type)) => {
                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        let mut lhs_type = Some(lhs_type);
                        let mut rhs_type = Some(rhs_type);

                        if ident_has_parens {
                            pattern_err = Some(self.pattern_error_push(
                                PatternError::ParenMismatch(ExprAndType {
                                    expr: ident_expr,
                                    type_id,
                                }),
                            ));

                            // cannot match to the types, unnecessary parens around lhs

                            lhs_type = None;
                            rhs_type = None;
                        } else if self.objs.expr(rhs).is_unit() {
                            pattern_err = Some(PatternError::IdentMissing(
                                rhs_type.expect("should always be Some in this branch"),
                            ));
                        }

                        // first pattern match on lhs of tuple
                        let (lhs_pattern, lhs_err) =
                            self.pattern_matching(ctx, scope, lhs, lhs_type);

                        let (rhs_pattern, rhs_err) =
                            self.pattern_matching(ctx, scope, rhs, rhs_type);

                        match (lhs_err, rhs_err) {
                            (_, Some(err)) | (Some(err), _) => {
                                pattern_err = Some(err);
                            }
                            _ => (),
                        }

                        return (
                            self.objs.pattern_push(Pattern {
                                type_id,
                                variant: PatternVariant::RestOfTuple((lhs_pattern, rhs_pattern)),
                            }),
                            pattern_err,
                        );
                    }
                    _ => (),
                }
            }
            _ => {
                self.analyze_expr(ctx, scope, ident_expr);

                return (
                    self.objs.pattern_push(Pattern {
                        type_id,
                        variant: PatternVariant::MiscExpr(ident_expr),
                    }),
                    pattern_err,
                );
            }
        }

        (
            self.objs.pattern_push(Pattern {
                type_id,
                variant: PatternVariant::IgnoreMultiple,
            }),
            Some(self.pattern_error_push(PatternError::MatchingUnsupported(ident_expr))),
        )
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

    // ret pattern and err if err is present
    fn analyze_instance_creation(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: Option<ExprID>,
        operand2: Option<ExprID>,
    ) -> (PatternID, Option<SemanticError>) {
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

        let pattern: PatternID;
        let mut type_id = self.get_builtin_type_id(ERROR_TYPE);

        if let (Some(ident_expr), type_expr) = (operand1, operand2) {
            let type_expr_struct = type_expr.map(|type_expr| self.objs.expr(type_expr).clone());

            // ensure that analyze "type expr" is actually a type

            let expr_returns = type_expr_struct
                .as_ref()
                .map_or(ExprReturns::Error, |type_expr_struct| {
                    type_expr_struct.expr_returns
                });

            let expr_returns_type = expr_returns == ExprReturns::Type;

            if !expr_returns_type {
                err = Some(self.expected_expr_returns(expr, ExprReturns::Type, expr_returns));
            }

            let maybe_type_id = if let (Some(type_expr_struct), true) =
                (type_expr_struct, expr_returns_type)
            {
                match type_expr_struct.type_or_module {
                    TypeOrModule::Type(t) => Some(t),
                    TypeOrModule::Module(_) => {
                        err =
                            Some(self.expected_expr_returns(expr, ExprReturns::Type, expr_returns));

                        None
                    }
                }
            } else {
                None
            };

            // if TypeID was determined record TypeID of this expr

            if let Some(t) = maybe_type_id {
                type_id = t;
            }

            // use pattern matching on our operation of the form
            // ident_expr : type
            // type_id is id of said type

            let (got_pattern, pattern_err) =
                self.pattern_matching(ctx, scope, ident_expr, maybe_type_id);

            pattern = got_pattern;

            if let Some(e) = pattern_err {
                err = Some(SemanticError::PatternError(e));
            }
        } else {
            // return dummy pattern

            let err_type_id = self.get_builtin_type_id(ERROR_TYPE);

            pattern = self.objs.pattern_push(Pattern {
                type_id: err_type_id,
                variant: PatternVariant::IgnoreMultiple,
            });
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_or_module = TypeOrModule::Type(type_id);
        expr_mut.expr_returns = ExprReturns::Value;
        expr_mut.is_var = true;
        expr_mut.finalized = err.is_none();

        (pattern, err)
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

        let old_curr_func = ctx.curr_func;

        ctx.curr_func = Some(func_scope);

        let old_analyzing_now = ctx.analyzing_now;

        ctx.analyzing_now = AnalyzingNow::FuncParams;
        self.analyze_expr(ctx, func_scope, func_literal.params);

        // at this point assuming things went normally will have params of function recorded
        // and params added as instances in function scope

        ctx.analyzing_now = AnalyzingNow::Type;
        self.analyze_expr(ctx, func_scope, func_literal.return_type);

        let mut ret_type_id = self.get_builtin_type_id(UNIT_TYPE);

        let mut finalized = true;

        let ret_type_expr = self.objs.expr(func_literal.return_type);

        match (
            ret_type_expr.finalized,
            ret_type_expr.expr_returns,
            &ret_type_expr.variant,
        ) {
            (false, ..) => (), // not finalized, don't test for err
            (_, ExprReturns::Type, _) => {
                // Ok, record type
                if let TypeOrModule::Type(t) = ret_type_expr.type_or_module {
                    ret_type_id = t;
                } else {
                    panic!("expr returns type but found module in type or module");
                }
            }
            (_, ExprReturns::Unit, ExprVariant::Unit) => (), // Ok, function returns Unit
            _ => {
                self.semantic_errors
                    .push(SemanticError::InvalidExpr(InvalidExpr {
                        expr,
                        msg: "either provide valid return type or omit return type",
                    }));

                finalized = false;
            }
        }

        ctx.analyzing_now = AnalyzingNow::Expr;
        self.analyze_expr(ctx, func_scope, func_literal.body);

        // if any of the function's child exprs are not finalized, then set finalized to false

        if [
            func_literal.params,
            func_literal.return_type,
            func_literal.body,
        ]
        .iter()
        .any(|expr| !self.objs.expr(*expr).finalized)
        {
            finalized = false;
        }

        // reset these to whatever they were before analyzing sub exprs

        ctx.analyzing_now = old_analyzing_now;
        ctx.curr_func = old_curr_func;

        let func_type = if finalized {
            let type_vec: Vec<TypeID> = func_literal
                .param_info
                .iter()
                .map(|pattern_id| self.objs.pattern(*pattern_id).type_id)
                .collect();

            let input_type = self.type_vec_to_tuple(&type_vec);

            // input type -> ret type

            Some(
                self.objs
                    .type_push(Type::Function((input_type, ret_type_id))),
            )
        } else {
            None
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.finalized = finalized;

        if func_literal.name.is_some() {
            self.set_expr_returns_unit(ctx, expr);
        } else {
            expr_mut.expr_returns = ExprReturns::Value;

            if let Some(t) = func_type {
                expr_mut.type_or_module = TypeOrModule::Type(t);
            }
        }
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

                if let Some(member_id) = self.scope_search(scope, name) {
                    let member = self.objs.member(member_id);

                    let etype = member.type_or_module.clone();

                    let expr_returns = match member.variant {
                        MemberVariant::Module => ExprReturns::Module,
                        MemberVariant::Type => ExprReturns::Type,
                        MemberVariant::Instance => ExprReturns::Value,
                    };

                    let ident_variant = match member.variant {
                        MemberVariant::Module => IdentifierVariant::Module,
                        MemberVariant::Type => IdentifierVariant::Type,
                        MemberVariant::Instance => IdentifierVariant::Instance,
                    };

                    let expr_mut = self.expr_mut(expr);

                    expr_mut.type_or_module = etype;

                    if let ExprVariant::Identifier(ident) = &mut expr_mut.variant {
                        ident.member_id = member_id;
                        ident.variant = ident_variant;
                    }

                    expr_mut.expr_returns = expr_returns;

                    expr_mut.is_var = true;
                    expr_mut.finalized = true;
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
