mod analyze_operation;
mod util;

use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, Identifier, IdentifierVariant, Member, MemberVariant,
        Operation, Pattern, PatternVariant, Scope, ScopeRefersTo, ScopeVariant, TokenOrString,
        Type, TypeVariant, Visibility,
        ast_contents::{ExprID, FunctionID, PatternID, ScopeID, TypeID},
        builtin::Builtin,
        errors::{
            DuplicateName, ExpectedExprReturns, ExpectedType, ExprAndType, InvalidExpr,
            PatternError, SemanticError, UnexpectedExpr,
        },
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
    Expr, // if specification is unnecessary
}

pub struct SemanticContext {
    analyzing_now: AnalyzingNow,

    // ID of current function, if one is being analyzed
    curr_func: Option<FunctionID>,
}

impl AST {
    fn scope_create_members_from_pattern(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        pattern: PatternID,
    ) -> Result<(), DuplicateName> {
        let mut err: Option<DuplicateName> = None;
        let mut pattern_stack = vec![pattern];

        while let Some(pattern) = pattern_stack.pop() {
            let pattern = pattern;

            let pattern_ref = self.objs.pattern(pattern);

            let type_id = pattern_ref.type_id;

            match pattern_ref.variant {
                PatternVariant::Ident(token) => {
                    // instance is added from identifier piece of pattern
                    if let Err(e) = self.scope_add_instance(
                        ctx,
                        scope,
                        TokenOrString::Token(token),
                        Some(type_id),
                    ) {
                        err = Some(e);
                    }
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

        match err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    // process provided pattern for func params
    // - append correct param information to func
    // - add correct instances to function scope
    fn _pattern_process_for_func_params(
        &mut self,
        ctx: &mut SemanticContext,
        pattern: PatternID,
    ) -> Result<(), DuplicateName> {
        let func = if let Some(curr_func) = ctx.curr_func {
            curr_func
        } else {
            return Ok(());
        };

        // add pattern to vec for func

        let func_mut = self.objs.function_mut(func);

        func_mut.params.push(pattern);

        let scope = func_mut.scope;

        self.scope_create_members_from_pattern(ctx, scope, pattern)
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

        // do not analyze operand1 since e.g. if it contains names for new instances
        // they have not been created yet

        if operand1.is_none() {
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
            let maybe_type_id = if let Some(type_expr) = type_expr {
                let expr = self.objs.expr(type_expr);

                match self.objs.type_get(expr.type_id) {
                    Type::Type(t) => Some(*t),
                    _ => {
                        let expr_returns = self.expr_returns(type_expr);

                        err = Some(self.expected_expr_returns(
                            type_expr,
                            ExprReturns::Type,
                            expr_returns,
                        ));

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

        expr_mut.type_id = type_id;
        expr_mut.is_var = true;
        expr_mut.finalized = err.is_none();

        (pattern, err)
    }

    fn analyze_if_elif(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let if_struct = match &self.objs.expr(expr).variant {
            ExprVariant::If(if_struct) | ExprVariant::Elif(if_struct) => if_struct.clone(),
            _ => panic!("should be if"),
        };

        // get if scope or create if necessary

        let if_scope = if if_struct.scope != ScopeID::default() {
            if_struct.scope
        } else {
            let if_scope = self.objs.scope_push(Scope {
                name: None,
                variant: ScopeVariant::Scope,
                parent_scope: scope,
                refers_to: Some(ScopeRefersTo::Expr(expr)),
                ..Default::default()
            });

            match &mut self.objs.expr_mut(expr).variant {
                ExprVariant::If(if_struct) | ExprVariant::Elif(if_struct) => if_struct.scope = if_scope,
                _ => panic!("should be if"),
            };

            if_scope
        };

        // will analyze each sub expr and if any are not finalized then will not finalized this expr
        // only additional check is to check that cond expr is of type Boolean

        let boolean_type_id = self.get_builtin_type_id(BOOLEAN_TYPE);

        self.analyze_expr(ctx, if_scope, if_struct.cond);

        let mut finalized = true;

        let cond_struct = self.objs.expr(if_struct.cond);

        if cond_struct.finalized {
            if !self.type_eq(cond_struct.type_id, boolean_type_id) {
                self.semantic_errors
                    .push(SemanticError::ExpectedType(ExpectedType {
                        expr,
                        expected: boolean_type_id,
                        found: cond_struct.type_id,
                    }));

                finalized = false;
            }
        } else {
            finalized = false;
        }

        self.analyze_expr(ctx, if_scope, if_struct.body);

        if !self.objs.expr(if_struct.body).finalized {
            finalized = false;
        }

        if let Some(else_expr) = if_struct.else_expr {
            self.analyze_expr(ctx, if_scope, else_expr);

            if !self.objs.expr(else_expr).finalized {
                finalized = false;
            }
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = TypeID::unit();
        expr_mut.finalized = finalized;
    }

    fn analyze_while(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let while_struct = match &self.objs.expr(expr).variant {
            ExprVariant::While(while_struct) => while_struct.clone(),
            _ => panic!("should be while"),
        };

        // get while scope if exists or create if necessary

        let while_scope = if while_struct.scope != ScopeID::default() {
            while_struct.scope
        } else {
            let while_scope = self.objs.scope_push(Scope {
                name: None,
                variant: ScopeVariant::Scope,
                parent_scope: scope,
    
                // connect to function obj
                refers_to: Some(ScopeRefersTo::Expr(expr)),
    
                ..Default::default()
            });

            match &mut self.objs.expr_mut(expr).variant {
                ExprVariant::While(while_struct) => while_struct.scope = while_scope,
                _ => panic!("should be while"),
            };

            while_scope
        };

        self.analyze_expr(ctx, while_scope, while_struct.cond);
        self.analyze_expr(ctx, while_scope, while_struct.body);

        let mut finalized = self.objs.expr(while_struct.cond).finalized
            && self.objs.expr(while_struct.body).finalized;

        if finalized {
            // check that condition actually returns Boolean

            let boolean_type_id = self.get_builtin_type_id(BOOLEAN_TYPE);

            let cond_type_id = self.objs.expr(while_struct.cond).type_id;

            if !self.type_eq(boolean_type_id, cond_type_id) {
                finalized = false;

                self.semantic_errors
                    .push(SemanticError::ExpectedType(ExpectedType {
                        expr: while_struct.cond,
                        expected: boolean_type_id,
                        found: cond_type_id,
                    }));
            }
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = TypeID::unit();
        expr_mut.finalized = finalized;
    }

    fn analyze_func(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let func_literal = match &self.objs.expr(expr).variant {
            ExprVariant::FunctionLiteral(f) => f.clone(),
            _ => panic!("should be func literal"),
        };

        let function_id = func_literal.function_id;
        
        let function_struct = self.objs.function(function_id);
        
        let func_name = function_struct.name;

        // IFF FUNCTION SCOPE HAS NOT ALREADY BEEN CREATED
        // create function scope as child of parent then use it later on

        let func_scope = if function_struct.scope != ScopeID::default() {
            function_struct.scope
        } else {
            let func_scope = self.objs.scope_push(Scope {
                name: None,
                variant: ScopeVariant::Scope,
                parent_scope: scope,
    
                // connect to function obj
                refers_to: Some(ScopeRefersTo::Function(function_id)),
    
                ..Default::default()
            });
    
            self.objs.function_mut(function_id).scope = func_scope;

            func_scope
        };
        
        // record current function scope in context

        let old_curr_func = ctx.curr_func;

        ctx.curr_func = Some(function_id);

        let old_analyzing_now = ctx.analyzing_now;

        ctx.analyzing_now = AnalyzingNow::FuncParams;
        self.analyze_expr(ctx, func_scope, func_literal.params);

        // at this point assuming things went normally will have params of function recorded
        // and params added as instances in function scope

        ctx.analyzing_now = AnalyzingNow::Type;
        self.analyze_expr(ctx, func_scope, func_literal.return_type_expr);

        let mut ret_type_id = self.get_builtin_type_id(UNIT_TYPE);

        let mut finalized = true;

        let ret_type_expr = self.objs.expr(func_literal.return_type_expr);

        match (
            ret_type_expr.finalized,
            self.objs.type_get(ret_type_expr.type_id),
            &ret_type_expr.variant,
        ) {
            (false, ..) => (),               // not finalized, don't test for err
            (_, _, ExprVariant::Unit) => (), // Ok, function returns Unit
            (_, Type::Type(t), _) => {
                // Ok, record type
                ret_type_id = *t;
            }
            _ => {
                self.semantic_errors
                    .push(SemanticError::InvalidExpr(InvalidExpr {
                        expr,
                        msg: "either provide valid return type or omit return type",
                    }));

                finalized = false;
            }
        }

        self.objs.function_mut(function_id).return_type = ret_type_id;

        let body_expr = self.objs.function(function_id).body;

        ctx.analyzing_now = AnalyzingNow::Expr;
        self.analyze_expr(ctx, func_scope, body_expr);

        // if any of the function's child exprs are not finalized, then set finalized to false

        if [
            func_literal.params,
            func_literal.return_type_expr,
            body_expr,
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
            let type_vec: Vec<TypeID> = self
                .objs
                .function(function_id)
                .params
                .iter()
                .map(|pattern| self.objs.pattern(*pattern).type_id)
                .collect();

            let input_type = self.type_vec_to_tuple(&type_vec);

            // input type -> ret type

            let func_type = self
                .objs
                .type_push(Type::Function((input_type, ret_type_id)));

            eprintln!("TYPE: {}", self.type_to_string(func_type));

            func_type
        } else {
            self.get_builtin_type_id(ERROR_TYPE)
        };

        self.objs.function_mut(function_id).func_type = func_type;

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.finalized = finalized;

        // named function literal returns Unit, otherwise return function itself from expr

        if func_name.is_some() {
            self.set_expr_returns_unit(ctx, expr);
        } else {
            expr_mut.type_id = func_type;
        }

        // IF FINALIZED AND FUNCTION IS NAMED,
        // add function as member of parent scope

        if let (true, Some(name)) = (finalized, func_name) {
            if self.scope_add_function(scope, name, function_id).is_err() {
                // duplicate name => problem
                self.objs.expr_mut(expr).finalized = false;
            }
        }
    }

    fn analyze_type_literal(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let type_literal = match &self.objs.expr(expr).variant {
            ExprVariant::TypeLiteral(t) => t.clone(),
            _ => panic!(),
        };

        let (type_scope, type_id) = if type_literal.type_id != TypeID::default() {
            let type_scope = match self.objs.type_get(type_literal.type_id) {
                Type::Scope(scope_id) => *scope_id,
                _ => {
                    panic!("type connected to TypeLiteral should be guaranteed to have corresponding scope");
                }
            };

            (type_scope, type_literal.type_id)
        } else {
            let type_scope = self.objs.scope_push(Scope {
                name: None,
                variant: ScopeVariant::Type(type_literal.variant),
                parent_scope: scope,
                refers_to: None,
    
                ..Default::default()
            });
    
            let type_id = self.type_push_scope(type_scope);

            (type_scope, type_id)
        };

        // connect scope back to type it is related to

        self.objs.scope_mut(type_scope).refers_to = Some(ScopeRefersTo::Type(type_id));

        let old_analyzing_now = ctx.analyzing_now;

        // analyze body of type and use scope created for type

        // enum parsing is different (comma-separated values)
        ctx.analyzing_now = AnalyzingNow::TypeBody(match type_literal.variant {
            TypeVariant::Enum => IsEnum::Enum,
            _ => IsEnum::Other,
        });
        self.analyze_expr(ctx, type_scope, type_literal.body);
        ctx.analyzing_now = old_analyzing_now;

        // this is how we indicate that expr is returning type itself rather than a value

        let type_id = self.objs.type_push(Type::Type(type_id));

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = type_id;
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

    fn analyze_ident(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        ident: Identifier,
    ) {
        let name = self.tokens().tok_as_str(&ident.name).to_string();

        // test if name of ident is a built-in function

        for builtin in Builtin::get_builtin_iter() {
            let builtin_name = builtin.get_builtin_name();

            if name != builtin_name {
                continue;
            }

            // name eq => match
            // create builtin type and set fields of expr

            let type_id = self.objs.type_push(Type::Builtin(builtin));

            let expr_mut = self.expr_mut(expr);

            expr_mut.type_id = type_id;
            expr_mut.finalized = true;
        }

        if let AnalyzingNow::TypeBody(_) = ctx.analyzing_now {
            // attempt to add discriminant-only member to enum or variant

            let scope_struct = self.objs.scope(scope);

            match scope_struct.variant {
                ScopeVariant::Type(TypeVariant::Enum | TypeVariant::Variant) => (), // Ok
                _ => {
                    self.semantic_errors
                        .push(SemanticError::UnexpectedExpr(UnexpectedExpr { expr }));
                    return;
                }
            }

            let type_id = match scope_struct.refers_to {
                Some(ScopeRefersTo::Type(type_id)) => type_id,
                _ => {
                    panic!("Current scope should be type if analyzing type body");
                }
            };

            // create member for new addition to enum/variant

            let new_member_id = self.objs.member_push(Member {
                name: TokenOrString::Token(ident.name),

                // global (not tied to specific instance)
                visibility: Visibility::Global,

                // type is enum/variant type itself
                variant: MemberVariant::Instance(type_id),

                ..Default::default()
            });

            // now attempt to insert new member

            let _ = self.scope_try_insert(scope, new_member_id);

            return;
        }

        if let Some(member_id) = self.scope_search(scope, name.as_str()) {
            let member = self.objs.member(member_id);

            let (type_id, ident_variant) = match member.variant {
                MemberVariant::Module(scope_id) => (
                    self.objs.type_push(Type::Module(scope_id)),
                    IdentifierVariant::Module,
                ),
                MemberVariant::Type(type_id) => (
                    self.objs.type_push(Type::Type(type_id)),
                    IdentifierVariant::Type,
                ),
                MemberVariant::Instance(type_id) => (type_id, IdentifierVariant::Instance),
                MemberVariant::Function(function_id) => (
                    self.objs.function(function_id).func_type,
                    IdentifierVariant::Function,
                ),
            };

            let expr_mut = self.expr_mut(expr);

            expr_mut.type_id = type_id;

            if let ExprVariant::Identifier(ident) = &mut expr_mut.variant {
                ident.member_id = member_id;
                ident.variant = ident_variant;
            }

            expr_mut.is_var = true;
            expr_mut.finalized = true;
        }
    }

    // semantic analysis on particular expression
    fn analyze_expr(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
        let expr_struct = self.objs.expr(expr);

        // already finalized => earlier pass successfully analyzed expr, continue

        if expr_struct.finalized {
            return;
        }

        match &expr_struct.variant {
            ExprVariant::Unit
            | ExprVariant::BooleanLiteral(_)
            | ExprVariant::IntegerLiteral(_)
            | ExprVariant::FloatLiteral(_)
            | ExprVariant::CharacterLiteral(_)
            | ExprVariant::StringLiteral(_) => {
                // find built-in type id and set type of expr

                let type_name = match self.expr(expr).variant {
                    ExprVariant::Unit => UNIT_TYPE,
                    ExprVariant::BooleanLiteral(_) => BOOLEAN_TYPE,

                    // character literals will just be Integers like C
                    ExprVariant::IntegerLiteral(_) | ExprVariant::CharacterLiteral(_) => {
                        INTEGER_TYPE
                    }
                    ExprVariant::FloatLiteral(_) => FLOAT_TYPE,
                    ExprVariant::StringLiteral(_) => STRING_TYPE,
                    _ => panic!("bad expr variant, not Unit or a literal"),
                };

                let type_id = self.get_builtin_type_id(type_name);

                let expr = self.expr_mut(expr);

                expr.type_id = type_id;
                expr.finalized = true;
            }
            ExprVariant::Identifier(ident) => {
                self.analyze_ident(ctx, scope, expr, ident.clone());
            }
            ExprVariant::If(_) | ExprVariant::Elif(_) => {
                self.analyze_if_elif(ctx, scope, expr);
            }
            ExprVariant::While(_) => {
                self.analyze_while(ctx, scope, expr);
            }
            ExprVariant::FunctionLiteral(_) => {
                if self
                    .test_analyzing_now(ctx.analyzing_now, &[AnalyzingNow::Expr], expr)
                    .is_err()
                {
                    return;
                }

                self.analyze_func(ctx, scope, expr);
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

                self.analyze_type_literal(ctx, scope, expr);
            }
            ExprVariant::Operation(_) => {
                self.analyze_operation(ctx, scope, expr);
            }
            _ => (),
        }
    }

    fn add_basic_types(&mut self, ctx: &mut SemanticContext) {
        for (name, variant) in [
            (INTEGER_TYPE, TypeVariant::Integer),
            (FLOAT_TYPE, TypeVariant::Float),
            (BOOLEAN_TYPE, TypeVariant::Enum),
        ] {
            if self
                .scope_search_no_recurse(ScopeID::global(), name)
                .is_some()
            {
                continue;
            }

            self.scope_add_member_type(
                ctx,
                ScopeID::global(),
                TokenOrString::String(name.to_string()),
                variant,
            )
            .expect("should be no name conflict for initial insertion of Integer, Float, Boolean");
        }

        // add false, true to Boolean type

        let boolean_type = self.get_builtin_type_id(BOOLEAN_TYPE);

        match self.objs.type_get(boolean_type).clone() {
            Type::Scope(scope) => {
                for name in ["false", "true"] {
                    if self.scope_search_no_recurse(scope, name).is_some() {
                        continue;
                    }

                    let member_id = self.objs.member_push(Member {
                        name: TokenOrString::String(name.to_string()),
                        visibility: Visibility::Global,
                        variant: MemberVariant::Instance(boolean_type),
                        ..Default::default()
                    });

                    self.scope_try_insert(scope, member_id).expect(
                        "initial insertion to Boolean of true, false should not cause conflict",
                    );
                }
            }
            _ => panic!("boolean type malformed"),
        }
    }

    // public function to do semantic analysis
    pub fn do_semantic_analysis(&mut self) {
        let mut ctx = SemanticContext {
            analyzing_now: AnalyzingNow::Expr,
            curr_func: None,
        };

        self.add_basic_types(&mut ctx);

        // provide file module scope as scope
        // use current root expr (found/created from syntax analysis)

        self.analyze_expr(&mut ctx, self.curr_file_module, self.root_expr());

        // if on second pass (repair pass) and could not finalized root expr
        // then that is an error/failure

        if self.doing_repair && !self.objs.expr(self.root_expr()).finalized {
            self.semantic_errors
                .push(SemanticError::RepairFailed(self.curr_file_module));
        }
    }
}
