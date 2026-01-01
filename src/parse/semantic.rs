use std::{collections::HashMap};

use crate::{
    constants::{ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, FunctionLiteral, Identifier, IdentifierVariant, Member, MemberVariant, Operation, Pattern, PatternVariant, Scope, ScopeVariant, TokenOrString, Tokens, Type, TypeVariant, Visibility, errors::{ExprAndType, InvalidOperation, MissingOperand, PatternError, SemanticError, UnexpectedExpr}
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
    curr_func: Option<u32>,
}

impl AST {
    fn get_builtin_type(&mut self, name: &str) -> u32 {
        if let Some(member) = self.scope_search(0, name) {
            if self.members[member as usize].variant != MemberVariant::Type {
                panic!("Not a type: {member}")
            }

            member
        } else {
            panic!("Builtin type not found: {name}");
        }
    }

    fn pattern_push(&mut self, pattern: Pattern) -> u32 {
        self.patterns.push(pattern);

        self.patterns.len() as u32 - 1
    }

    fn pattern_error_push(&mut self, pattern_error: PatternError) {
        self.semantic_errors.push(SemanticError::PatternError(pattern_error))
    }

    fn get_builtin_type_id(&mut self, name: &str) -> u32 {
        let member = self.get_builtin_type(name);

        self.members[member as usize].module_or_type
    }

    fn missing_operand(&mut self, expr: u32, operand: u32) -> SemanticError {
        let ret  = SemanticError::MissingOperand(MissingOperand {
            operation: expr,
            operand_missing: operand,
        });
        
        self.semantic_errors
            .push(ret.clone());

        ret
    }

    fn invalid_operation(&mut self, expr: u32, msg: &'static str) -> SemanticError {
        let ret = SemanticError::InvalidOperation(InvalidOperation {
            operation: expr,
            msg,
        });

        self.semantic_errors
            .push(ret.clone());

        ret
    }

    // ret: member id of instance
    fn scope_add_instance(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        name: TokenOrString,
        type_id: Option<u32>,
    ) -> u32 {
        let type_id = match type_id {
            Some(id) => id,
            None => self.get_builtin_type_id(ERROR_TYPE),
        };

        let member_id = self.member_push(Member {
            name,
            visibility: Visibility::Private,
            variant: MemberVariant::Instance,
            module_or_type: type_id,
        });

        self.scope_add_member(ctx, scope, member_id);

        member_id
    }

    fn scope_create_members_from_pattern(&mut self, ctx: &mut SemanticContext, scope: u32, pattern: u32) {
        let mut pattern_stack = vec![pattern];

        while let Some(pattern) = pattern_stack.pop() {
            let pattern = pattern;

            let pattern_ref = &self.patterns[pattern as usize];

            let type_id = pattern_ref.type_id;

            match pattern_ref.variant {
                PatternVariant::Ident(token) => {
                    // instance is added from identifier piece of pattern
                    self.scope_add_instance(
                        ctx,
                        scope,
                        TokenOrString::Token(token),
                        Some(type_id),
                    );
                }
                PatternVariant::Tuple((lhs, rhs)) | PatternVariant::Slice((lhs, rhs)) => {
                    if let Some(rhs) = rhs {
                        pattern_stack.push(rhs);
                    }

                    pattern_stack.push(lhs);
                }
                PatternVariant::RestOfTuple((lhs, rhs)) | PatternVariant::RestOfSlice((lhs, rhs)) => {
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
    fn pattern_process_for_func_params(
        &mut self,
        ctx: &mut SemanticContext,
        pattern: u32,
    ) {
        let func_scope = if let Some(curr_func) = ctx.curr_func {
            curr_func
        } else {
            return;
        };

        let func_expr = if let Some(refers_to) = self.scopes[func_scope as usize].refers_to {
            refers_to
        } else {
            return;
        };

        // add pattern to vec for func

        match &mut self.exprs[func_expr as usize].variant {
            ExprVariant::FunctionLiteral(func_literal) => {
                func_literal.param_info.push(pattern)
            }
            _ => ()
        }

        self.scope_create_members_from_pattern(ctx, func_scope, pattern);
    }

    // function to attempt pattern matching between identifier(s) in pattern and type
    // also should add any new identifiers to scope
    // ret should indicate what problems occurred if any
    fn pattern_matching(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        ident_expr: u32,

        // may be absent (so errors will be recorded if necessary)
        type_id: u32,
    ) -> Result<u32, PatternError> {
        let mut ident_expr = ident_expr;

        let mut ident_has_parens = false;

        while match self.exprs[ident_expr as usize].variant {
            ExprVariant::Operation(Operation { op: TokenType::LParen, operand1: Some(expr), operand2: None }) => {
                ident_has_parens = true;
                ident_expr = expr;

                true
            }
            _ => false
        } {}

        let type_val = self.types[type_id as usize].clone();

        match self.exprs[ident_expr as usize].variant {
            ExprVariant::Identifier(ident) => {
                return Ok(self.pattern_push(Pattern { type_id: type_id, variant: PatternVariant::Ident(ident.name) }))
            }
            // Tuple
            ExprVariant::Operation(Operation{ op: TokenType::Comma, operand1: lhs, operand2: rhs}) => {
                match type_val {
                    Type::Tuple((lhs_type, rhs_type)) => {
                        if !ident_has_parens {
                            return Err(PatternError::ParenMismatch(ExprAndType { expr: ident_expr, type_id }));
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        if rhs_type.is_some() && self.exprs[rhs as usize].is_unit() {
                            return Err(PatternError::IdentMissing(rhs_type.expect("impossible")));
                        } else if rhs_type.is_none() && !self.exprs[rhs as usize].is_unit() {
                            return Err(PatternError::TypeMissing(rhs));
                        }

                        // first pattern match on lhs of tuple
                        let lhs_pattern = self.pattern_matching(
                            ctx,
                            scope,
                            lhs,
                            lhs_type,
                        )?;

                        let rhs_pattern = if let Some(rhs_type) = rhs_type {
                            Some(self.pattern_matching(ctx, scope, rhs, rhs_type)?)
                        } else {
                            None
                        };

                        return Ok(self.pattern_push(Pattern {
                            type_id,
                            variant: PatternVariant::Tuple((lhs_pattern, rhs_pattern))
                        }));
                    }
                    Type::RestOfTuple((lhs_type, rhs_type)) => {
                        if ident_has_parens {
                            return Err(PatternError::ParenMismatch(ExprAndType { expr: ident_expr, type_id }));
                        }

                        let lhs = lhs.expect("should have lhs");
                        let rhs = rhs.expect("should have rhs");

                        if self.exprs[rhs as usize].is_unit() {
                            return Err(PatternError::IdentMissing(rhs_type));
                        }

                        // first pattern match on lhs of tuple
                        let lhs_pattern = self.pattern_matching(ctx, scope, lhs, lhs_type)?;

                        let rhs_pattern = self.pattern_matching(ctx, scope, rhs, rhs_type)?;

                        return Ok(self.pattern_push(Pattern {
                            type_id,
                            variant: PatternVariant::RestOfTuple((lhs_pattern, rhs_pattern))
                        }));
                    }
                    _ => (),
                }
            }
            _ => {}
        }

        Ok(0)
    }

    // ret pattern or error
    fn analyze_instance_creation(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        expr: u32,
        operand1: Option<u32>,
        operand2: Option<u32>,
    ) -> Result<u32, SemanticError> {
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
        let mut pattern  = 0;

        if let (Some(ident_expr), Some(type_expr)) = (operand1, operand2) {
            let type_expr_ref = &self.exprs[type_expr as usize];

            if type_expr_ref.finalized && type_expr_ref.expr_returns == ExprReturns::Type {
                let type_id = type_expr_ref.type_or_module;

                // use pattern matching on our operation of the form
                // ident_expr : type
                // type_id is id of said type

                let pattern_result = self.pattern_matching(ctx, scope, ident_expr, type_id);

                match pattern_result {
                    Ok(value) => {
                        finalized = true;
                        pattern = value;
                    }
                    Err(e) => {
                        err = Some(SemanticError::PatternError(e))
                    }
                }
            }
        }

        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = &mut self.exprs[expr as usize];

        expr_mut.type_or_module = unit_type;
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
        scope: u32,
        expr: u32,
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
                ) {

                }
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
        scope: u32,
        expr: u32,
        operation: Operation,
    ) {
        match operation.op {
            TokenType::Begin => {
                // Block
                let child_expr = operation
                    .operand1
                    .expect("Block should always contain Expr");

                match self.exprs[child_expr as usize].variant {
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
                        && match self.exprs
                            [operation.operand2.expect("RHS should be present") as usize]
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
                self.analyze_instance_creation(ctx, scope, expr, operation.operand1, operation.operand2);
            }
            _ => {}
        }
    }

    fn analyze_type_def(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32, name: u32, value: u32) {

    }

    fn analyze_operation_unary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        expr: u32,
        op: TokenType,
        operand: u32,
    ) {
        let operand_type = self.expr(operand).type_or_module;

        match self.expr(operand).expr_returns {
            ExprReturns::Module => {
                self.semantic_errors.push(SemanticError::InvalidOperation(InvalidOperation {
                    operation: expr,
                    msg: "unary operation may not be applied to a module",
                }));
                return;
            }
            ExprReturns::Unit => {
                self.semantic_errors.push(SemanticError::InvalidOperation(InvalidOperation {
                    operation: expr,
                    msg: "unary operation may not be applied to Unit",
                }));
                return;
            }
            ExprReturns::Type => {
                let type_id = match op {
                    TokenType::Star => self.type_push(Type::Ptr(operand_type)),
                    TokenType::Ampersand => self.type_push(Type::Ref(operand_type)),
                    _ => {
                        self.semantic_errors.push(SemanticError::InvalidOperation(InvalidOperation {
                            operation: expr,
                            msg: "this operation is not supported for types",
                        }));
                        return;
                    }
                };

                let expr_mut = &mut self.exprs[expr as usize];

                expr_mut.expr_returns = ExprReturns::Type;
                expr_mut.type_or_module = type_id;
                expr_mut.finalized = true;

                return;
            }
            _ => (),
        }

        match op {
            TokenType::Plus | TokenType::Minus => (),
            _ => {
                self.semantic_errors.push(SemanticError::InvalidOperation(InvalidOperation {
                    operation: expr,
                    msg: "invalid unary operator",
                }));
                return;
            }
        }

    }

    fn analyze_operation_binary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        expr: u32,
        op: TokenType,
        operand1: u32,
        operand2: u32,
    ) {
        match op {
            TokenType::Plus => {

            }
            TokenType::Minus => {

            }
            TokenType::Star => {

            }
            TokenType::FSlash => {

            }
            TokenType::Percent => {

            }
            _ => {
                
            }
        }
    }

    fn analyze_operation(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let operation = match &self.exprs[expr as usize].variant {
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

                if !self.exprs[operand as usize].finalized {
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
            _ => {
                self.semantic_errors.push(SemanticError::InvalidOperation(InvalidOperation {
                    operation: expr,
                    msg: "operation has no operands"
                }))
            }
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
        expr: u32,
    ) -> Result<(), UnexpectedExpr> {
        if allowed.iter().any(|curr| *curr == analyzing_now) {
            Ok(())
        } else {
            Err(UnexpectedExpr { expr })
        }
    }

    // semantic analysis on particular expression
    fn analyze_expr(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
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
    fn scope_search(&mut self, scope: u32, name: &str) -> Option<u32> {
        let mut scope = scope;

        loop {
            let scope_ref = &self.scopes[scope as usize];

            if let Some(member) = scope_ref.members.get(name) {
                return Some(*member);
            } else if scope_ref.parent_scope == scope {
                return None;
            }

            scope = scope_ref.parent_scope;
        }
    }

    fn scope_push(&mut self, scope: Scope) -> u32 {
        self.scopes.push(scope);

        self.scopes.len() as u32 - 1
    }

    fn type_push(&mut self, lang_type: Type) -> u32 {
        self.types.push(lang_type);

        self.types.len() as u32 - 1
    }

    fn type_push_scope(&mut self, scope: u32) -> u32 {
        let type_id = self.type_push(Type::Scope(scope));

        self.scopes[scope as usize].refers_to = Some(type_id);

        type_id
    }

    fn member_push(&mut self, member: Member) -> u32 {
        self.members.push(member);

        return self.members.len() as u32 - 1;
    }

    fn scope_add_member(&mut self, ctx: &mut SemanticContext, scope: u32, member: u32) {
        let member_name = match &self.members[member as usize].name {
            TokenOrString::Token(t) => ctx.tokens.tok_as_str(t),
            TokenOrString::String(s) => s.as_str(),
        };

        self.scopes[scope as usize]
            .members
            .insert(member_name.to_string(), member);
    }

    fn type_create(&mut self, scope: u32, name: TokenOrString, variant: TypeVariant) -> u32 {
        let scope_id = self.scope_push(Scope {
            name: Some(name.clone()),
            variant: ScopeVariant::Type(variant),
            parent_scope: scope,
            refers_to: None,
            members: HashMap::new(),
        });

        self.type_push(Type::Scope(scope_id))
    }

    // provide scope, type id to add type to scope as a member
    // ret: member id
    fn scope_add_member_type_from_id(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        type_id: u32,
    ) -> u32 {
        let name = match self.types[type_id as usize] {
            Type::Scope(scope) => self.scopes[scope as usize].name.clone(),
            _ => None,
        };

        let member_id = self.member_push(Member {
            name: name.expect("CURRENTLY CAN ONLY ADD NAMED TYPE AS SCOPE MEMBER"),
            visibility: Visibility::Private,
            variant: MemberVariant::Type,
            module_or_type: type_id,
        });

        self.scope_add_member(ctx, scope, member_id);

        member_id
    }

    // return is the id of the Scope which represents the type
    fn scope_add_member_type(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        name: TokenOrString,
        variant: TypeVariant,
    ) -> u32 {
        let type_id = self.type_create(scope, name.clone(), variant);

        let member_id = self.member_push(Member {
            name: name,
            visibility: Visibility::Private,
            variant: MemberVariant::Type,
            module_or_type: type_id,
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

            let global_scope = self.scope_push(Scope {
                name: Some(TokenOrString::String("GLOBAL".to_string())),
                variant: ScopeVariant::Module,
                parent_scope: 0,
                refers_to: None,
                members: HashMap::new(),
            });

            for (name, variant) in [
                (ERROR_TYPE, TypeVariant::Error),
                (INTEGER_TYPE, TypeVariant::Integer),
                (FLOAT_TYPE, TypeVariant::Float),
                (UNIT_TYPE, TypeVariant::Unit),
                (STRING_TYPE, TypeVariant::String),
            ] {
                self.scope_add_member_type(
                    &mut ctx,
                    global_scope,
                    TokenOrString::String(name.to_string()),
                    variant,
                );
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
