use std::collections::HashMap;

use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, IdentifierVariant, Member, MemberVariant, Operation, Pattern, PatternVariant, Scope, ScopeRefersTo, ScopeVariant, TokenOrString, Tokens, Type, TypeOrModule, TypeVariant, Visibility, ast_contents::{ExprID, MemberID, PatternID, ScopeID, TypeID}, errors::{
            ExpectedExprReturns, ExprAndType, InvalidOperation, MissingOperand, PatternError, SemanticError, UnexpectedExpr
        }, operator
    },
    scan::TokenType,
};

#[derive(Clone, Copy, PartialEq)]
enum IsEnum {
    Enum,
    Other,
}

#[derive(Clone, Copy, PartialEq)]
enum AnalyzingNow {
    Type,
    TypeBody(IsEnum),
    FuncParams,
    Pattern, // e.g. (x, y) so certain ops not permitted
    Expr,    // if specification is unnecessary
}

struct SemanticContext<'a> {
    tokens: &'a Tokens<'a>,
    analyzing_now: AnalyzingNow,

    // scope id of current function
    curr_func: Option<ScopeID>,
}

use operator::OperatorVariant::*;

impl AST {
    fn get_builtin_type(&mut self, name: &str) -> MemberID {
        if let Some(member) = self.scope_search(ScopeID::default(), name) {
            if self.objs.member(member).variant != MemberVariant::Type {
                panic!("Not a type")
            }

            member
        } else {
            panic!("Builtin type not found: {name}");
        }
    }

    fn pattern_error_push(&mut self, pattern_error: PatternError) {
        self.semantic_errors
            .push(SemanticError::PatternError(pattern_error))
    }

    fn get_builtin_type_id(&mut self, name: &str) -> TypeID {
        let member = self.get_builtin_type(name);

        match self.objs.member(member).type_or_module {
            TypeOrModule::Type(t) => t,
            _ => panic!("builtin type should not be a module"),
        }
    }

    fn missing_operand(&mut self, expr: ExprID, operand: u32) -> SemanticError {
        let ret = SemanticError::MissingOperand(MissingOperand {
            operation: expr,
            operand_missing: operand,
        });

        self.semantic_errors.push(ret.clone());

        ret
    }

    fn invalid_operation(&mut self, expr: ExprID, msg: &'static str) -> SemanticError {
        let ret = SemanticError::InvalidOperation(InvalidOperation {
            operation: expr,
            msg,
        });

        self.semantic_errors.push(ret.clone());

        ret
    }

    // ret: member id of instance
    fn scope_add_instance(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        name: TokenOrString,
        type_id: Option<TypeID>,
    ) -> MemberID {
        let type_id = match type_id {
            Some(id) => id,
            None => self.get_builtin_type_id(ERROR_TYPE),
        };

        let member_id = self.objs.member_push(Member {
            name,
            visibility: Visibility::Private,
            variant: MemberVariant::Instance,
            type_or_module: TypeOrModule::Type(type_id),
        });

        self.scope_add_member(ctx, scope, member_id);

        member_id
    }

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
                            return Err(PatternError::ParenMismatch(ExprAndType {
                                expr: ident_expr,
                                type_id,
                            }));
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        if rhs_type.is_some() && self.objs.expr(rhs).is_unit() {
                            return Err(PatternError::IdentMissing(rhs_type.expect("impossible")));
                        } else if rhs_type.is_none() && !self.expr(rhs).is_unit() {
                            return Err(PatternError::TypeMissing(rhs));
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
                            return Err(PatternError::ParenMismatch(ExprAndType {
                                expr: ident_expr,
                                type_id,
                            }));
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
                        return Err(self.expected_expr_returns(expr, ExprReturns::Type, expr_returns));
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
                        && match self.objs.expr(operation.operand2.expect("RHS should be present"))
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

    fn analyze_type_def(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        name: ExprID,
        value: ExprID,
    ) {
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

    fn analyze_operation(&mut self, ctx: &mut SemanticContext, scope: ScopeID, expr: ExprID) {
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

    fn semantic_analyze_func(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let func_literal = match &self.exprs[expr as usize].variant {
            ExprVariant::FunctionLiteral(f) => f.clone(),
            _ => panic!(),
        };

        // create function scope as child of parent then use it later on

        let func_scope = self.scope_push(Scope {
            name: None,
            variant: ScopeVariant::Scope,
            parent_scope: scope,
            refers_to: Some(expr), // connect to function literal
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

    fn semantic_analyze_type_literal(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let type_literal = match &self.exprs[expr as usize].variant {
            ExprVariant::TypeLiteral(t) => t.clone(),
            _ => panic!(),
        };

        let type_scope = self.scope_push(Scope {
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

        let expr_mut = &mut self.exprs[expr as usize];

        expr_mut.type_or_module = type_id;
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

                let type_id = self.members[member as usize].module_or_type;

                let expr = self.expr_mut(expr);

                expr.type_or_module = type_id;
                expr.finalized = true;
            }
            ExprVariant::Identifier(ident) => {
                let name = ctx.tokens.tok_as_str(&ident.name);

                if let Some(member) = self.scope_search(scope, name) {
                    let member = &self.members[member as usize];

                    let etype = member.module_or_type;
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

    // search provided scope for a given name and received Member if said name
    // can be found, also recurse to parent if needed
    fn scope_search(&mut self, scope: ScopeID, name: &str) -> Option<MemberID> {
        let mut scope = scope;

        loop {
            let scope_ref = self.objs.scope(scope);

            if let Some(member) = scope_ref.members.get(name) {
                return Some(*member);
            } else if scope_ref.parent_scope == scope {
                return None;
            }

            scope = scope_ref.parent_scope;
        }
    }

    fn type_push_scope(&mut self, scope: ScopeID) -> TypeID {
        let type_id = self.objs.type_push(Type::Scope(scope));

        self.objs.scope_mut(scope).refers_to = Some(type_id);

        type_id
    }

    fn scope_add_member(&mut self, ctx: &mut SemanticContext, scope: ScopeID, member: MemberID) {
        let t_or_s = self.objs.member(member).name.clone();

        let member_name = match t_or_s {
            TokenOrString::Token(t) => ctx.tokens.tok_as_str(&t).to_string(),
            TokenOrString::String(s) => s,
        };

        self.objs
            .scope_mut(scope)
            .members
            .insert(member_name.to_string(), member);
    }

    fn type_create(&mut self, scope: ScopeID, name: TokenOrString, variant: TypeVariant) -> TypeID {
        let scope_id = self.objs.scope_push(Scope {
            name: Some(name.clone()),
            variant: ScopeVariant::Type(variant),
            parent_scope: scope,
            refers_to: None,
            members: HashMap::new(),
        });

        self.objs.type_push(Type::Scope(scope_id))
    }

    // provide scope, type id to add type to scope as a member
    // ret: member id
    fn scope_add_member_type_from_id(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        type_id: TypeID,
    ) -> MemberID {
        let name = match self.objs.type_get(type_id) {
            Type::Scope(scope) => self.objs.scope(*scope).name.clone(),
            _ => None,
        };

        let member_id = self.objs.member_push(Member {
            name: name.expect("CURRENTLY CAN ONLY ADD NAMED TYPE AS SCOPE MEMBER"),
            visibility: Visibility::Private,
            variant: MemberVariant::Type,
            type_or_module: TypeOrModule::Type(type_id),
        });

        self.scope_add_member(ctx, scope, member_id);

        member_id
    }

    // return is the id of the Scope which represents the type
    fn scope_add_member_type(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        name: TokenOrString,
        variant: TypeVariant,
    ) -> TypeID {
        let type_id = self.type_create(scope, name.clone(), variant);

        let member_id = self.objs.member_push(Member {
            name: name,
            visibility: Visibility::Private,
            variant: MemberVariant::Type,
            type_or_module: TypeOrModule::Type(type_id),
        });

        self.scope_add_member(ctx, scope, member_id);

        type_id
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

            match self.objs.type_get(boolean_type) {
                Type::Scope(scope) => {
                    for name in ["false", "true"] {
                        let member = self.member_push(Member {
                            name: TokenOrString::String(name.to_string()),
                            visibility: Visibility::Global,
                            variant: MemberVariant::Instance,
                            type_or_module: boolean_type,
                        });

                        self.scope_add_member(&mut ctx, scope, member);
                    }
                }
                _ => panic!("boolean type malformed"),
            }

            // create new scope for module

            let module_scope = self.scope_push(Scope {
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
