use std::collections::HashMap;

use crate::{
    constants::{ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, FunctionLiteral, Identifier, IdentifierVariant, Member,
        MemberVariant, Operation, Scope, ScopeVariant, TokenOrString, Tokens, Type, TypeVariant,
        Visibility,
        errors::{InvalidOperation, MissingOperand, SemanticError, UnexpectedExpr},
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

    fn get_builtin_type_id(&mut self, name: &str) -> u32 {
        let member = self.get_builtin_type(name);

        self.members[member as usize].module_or_type
    }

    fn missing_operand(&mut self, expr: u32, operand: u32) {
        self.semantic_errors
            .push(SemanticError::MissingOperand(MissingOperand {
                operation: expr,
                operand_missing: operand,
            }));
    }

    fn invalid_operation(&mut self, expr: u32, msg: &'static str) {
        self.semantic_errors
            .push(SemanticError::InvalidOperation(InvalidOperation {
                operation: expr,
                msg,
            }));
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

    // function to attempt pattern matching between identifier(s) in pattern and type
    // also should add any new identifiers to scope
    // ret should indicate what problems occurred if any
    fn pattern_matching(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        ident_pattern: u32,

        // may be absent (so errors will be recorded if necessary)
        type_pattern: Option<u32>,
    ) -> Result<(), ()> {
        match self.exprs[ident_pattern as usize].variant {
            ExprVariant::Identifier(ident) => {
                self.scope_add_instance(ctx, scope, TokenOrString::Token(ident.name), type_pattern);
            }
            _ => {}
        }

        Ok(())
    }

    fn analyze_instance_creation(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        expr: u32,
        operand1: Option<u32>,
        operand2: Option<u32>,
    ) {
        let old_analyzing_now = ctx.analyzing_now;

        if let Some(pattern) = operand1 {
            ctx.analyzing_now = AnalyzingNow::Pattern;
            self.semantic_analyze_expr(ctx, scope, pattern);
            ctx.analyzing_now = old_analyzing_now;
        } else {
            self.missing_operand(expr, 1);
        }

        if let Some(pattern) = operand2 {
            ctx.analyzing_now = AnalyzingNow::Type;
            self.semantic_analyze_expr(ctx, scope, pattern);
            ctx.analyzing_now = old_analyzing_now;
        } else {
            self.missing_operand(expr, 2);
        }

        let mut finalized = false;

        if let Some(pattern) = operand1 {
            if self
                .pattern_matching(ctx, scope, pattern, operand2)
                .is_ok()
            {
                finalized = true;
            }
        }

        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = &mut self.exprs[expr as usize];

        expr_mut.type_or_module = unit_type;
        expr_mut.expr_returns = ExprReturns::Unit;
        expr_mut.finalized = finalized;
    }

    fn semantic_analyze_operation_func_params(
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
                    self.semantic_analyze_expr(ctx, scope, lhs);
                } else {
                    // should be param on lhs
                    self.missing_operand(expr, 1);
                }

                // it is fine for rhs to be missing e.g. function f(0 : Integer,) ...
                if let Some(rhs) = operation.operand2 {
                    self.semantic_analyze_expr(ctx, scope, rhs);
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

    fn semantic_analyze_operation_type_body(
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

                self.semantic_analyze_expr(ctx, scope, child_expr);
            }
            TokenType::Semicolon | TokenType::Comma => {
                match (operation.op, ctx.analyzing_now) {
                    (TokenType::Semicolon, AnalyzingNow::TypeBody(IsEnum::Enum))
                    | (TokenType::Comma, AnalyzingNow::TypeBody(IsEnum::Other)) => {
                        self.invalid_operation(
                            expr,
                            "only enums should have comma-separated members",
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

                    self.semantic_analyze_expr(
                        ctx,
                        scope,
                        operand.expect("both operands should always be present here"),
                    );
                }
            }
            TokenType::Colon => {}
            _ => {}
        }
    }

    fn semantic_analyze_operation(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let operation = match &self.exprs[expr as usize].variant {
            ExprVariant::Operation(operation) => operation.clone(),
            _ => panic!(),
        };

        match ctx.analyzing_now {
            AnalyzingNow::FuncParams => {
                self.semantic_analyze_operation_func_params(ctx, scope, expr, operation);
            }
            AnalyzingNow::TypeBody(_) => {
                self.semantic_analyze_operation_type_body(ctx, scope, expr, operation);
            }
            _ => {}
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

        ctx.analyzing_now = AnalyzingNow::FuncParams;
        self.semantic_analyze_expr(ctx, func_scope, func_literal.params);

        ctx.analyzing_now = AnalyzingNow::Type;
        self.semantic_analyze_expr(ctx, func_scope, func_literal.return_type);

        ctx.analyzing_now = AnalyzingNow::Expr;
        self.semantic_analyze_expr(ctx, func_scope, func_literal.body);
    }

    fn semantic_analyze_type_literal(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let type_literal = match &self.exprs[expr as usize].variant {
            ExprVariant::TypeLiteral(t) => t.clone(),
            _ => panic!(),
        };

        let type_scope = self.scope_push(Scope {
            name: match type_literal.name {
                Some(t) => Some(TokenOrString::Token(t)),
                None => None,
            },
            variant: ScopeVariant::Type(type_literal.variant),
            parent_scope: scope,
            refers_to: None,
            members: HashMap::new(),
        });

        let type_id = self.type_push_scope(type_scope);

        // if it is a named type add it to scope
        if type_literal.name.is_some() {
            self.scope_add_member_type_from_id(ctx, scope, type_id);
        }

        let old_analyzing_now = ctx.analyzing_now;

        // analyze body of type and use scope created for type

        // enum parsing is different (comma-separated values)
        ctx.analyzing_now = AnalyzingNow::TypeBody(match type_literal.variant {
            TypeVariant::Enum => IsEnum::Enum,
            _ => IsEnum::Other,
        });
        self.semantic_analyze_expr(ctx, type_scope, type_literal.body);
        ctx.analyzing_now = old_analyzing_now;

        // type of named type literal is Unit (cannot assign it to something)
        // type of unnamed type literal is whatever type in question is

        let (expr_type, expr_returns) = match type_literal.name {
            Some(_) => (self.get_builtin_type_id(UNIT_TYPE), ExprReturns::Unit),
            None => (type_id, ExprReturns::Type),
        };

        let expr_mut = &mut self.exprs[expr as usize];

        expr_mut.type_or_module = expr_type;
        expr_mut.expr_returns = expr_returns;
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
    fn semantic_analyze_expr(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
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
                self.semantic_analyze_operation(ctx, scope, expr);
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

            self.semantic_analyze_expr(&mut ctx, module_scope, expr);
        };
    }
}
