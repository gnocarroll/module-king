use std::collections::HashMap;

use crate::{
    constants::{ERROR_TYPE, FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, FunctionLiteral, Identifier, IdentifierVariant, Member,
        MemberVariant, Scope, ScopeVariant, TokenOrString, Tokens, TypeVariant, Visibility, errors::{MissingOperand, SemanticError},
    }, scan::TokenType,
};

#[derive(Clone, Copy, PartialEq)]
enum AnalyzingNow {
    Type,
    TypeBody,
    FuncParams,
    Pattern, // e.g. (x, y) so certain ops not permitted
    Expr, // if specification is unnecessary
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
        self.semantic_errors.push(SemanticError::MissingOperand(MissingOperand {
            operation: expr,
            operand_missing: operand,
        }));
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
        Ok(())
    }

    fn semantic_analyze_operation(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        let operation = match &self.exprs[expr as usize].variant {
            ExprVariant::Operation(operation) => operation.clone(),
            _ => panic!(),
        };

        match ctx.analyzing_now {
            AnalyzingNow::FuncParams => {
                match operation.op {
                    TokenType::Comma => {
                        if let Some(lhs) = operation.operand1 {
                            // scope remains same and still analyzing func params
                            self.semantic_analyze_expr(ctx, scope, lhs);
                        }
                        else {
                            // should be param on lhs
                            self.missing_operand(expr, 1);
                        }

                        // it is fine for rhs to be missing e.g. function f(0 : Integer,) ...
                        if let Some(rhs) = operation.operand2 {
                            self.semantic_analyze_expr(ctx, scope, rhs);
                        }
                    }
                    TokenType::Colon => {
                        let old_analyzing_now = ctx.analyzing_now;

                        if let Some(pattern) = operation.operand1 {
                            ctx.analyzing_now = AnalyzingNow::Pattern;
                            self.semantic_analyze_expr(ctx, scope, pattern);
                            ctx.analyzing_now = old_analyzing_now;
                        }
                        else {
                            self.missing_operand(expr, 1);
                        }

                        if let Some(pattern) = operation.operand2 {
                            ctx.analyzing_now = AnalyzingNow::Type;
                            self.semantic_analyze_expr(ctx, scope, pattern);
                            ctx.analyzing_now = old_analyzing_now;
                        }
                        else {
                            self.missing_operand(expr, 2);
                        }
                        
                        let mut finalized = false;

                        if let (Some(pattern), param_type) = (operation.operand1, operation.operand2) {
                            if self.pattern_matching(ctx, scope, pattern, param_type).is_ok() {
                                finalized = true;
                            }
                        }

                        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

                        let expr_mut = &mut self.exprs[expr as usize];

                        expr_mut.etype = unit_type;
                        expr_mut.expr_returns = ExprReturns::Unit;
                        expr_mut.finalized = finalized;
                    }
                    TokenType::Eq | TokenType::ColonEq => {

                    }
                    _ => {

                    }
                }
            }
            _ => {

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

        self.semantic_analyze_expr(ctx, type_scope, type_literal.body);

        // type of named type literal is Unit (cannot assign it to something)
        // type of unnamed type literal is whatever type in question is

        let (expr_type, expr_returns) = match type_literal.name {
            Some(_) => (self.get_builtin_type_id(UNIT_TYPE), ExprReturns::Unit),
            None => (type_scope, ExprReturns::Type),
        };

        let expr_mut = &mut self.exprs[expr as usize];

        expr_mut.etype = expr_type;
        expr_mut.expr_returns = expr_returns;
        expr_mut.finalized = true;
    }

    // semantic analysis on particular expression
    fn semantic_analyze_expr(&mut self, ctx: &mut SemanticContext, scope: u32, expr: u32) {
        match &self.expr(expr).variant {
            ExprVariant::Unit
            | ExprVariant::IntegerLiteral(_)
            | ExprVariant::FloatLiteral(_)
            | ExprVariant::StringLiteral(_) => {
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

                expr.etype = type_id;
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

                    expr.etype = etype;

                    if let ExprVariant::Identifier(ident) = &mut expr.variant {
                        ident.variant = ident_variant;
                    }

                    expr.finalized = true;
                }
            }
            ExprVariant::FunctionLiteral(_) => {
                self.semantic_analyze_func(ctx, scope, expr);
            }
            ExprVariant::TypeLiteral(_) => {
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

        return self.scopes.len() as u32 - 1;
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

    // return is the id of the Scope which represents the type
    fn scope_add_member_type(
        &mut self,
        ctx: &mut SemanticContext,
        scope: u32,
        name: TokenOrString,
        variant: TypeVariant,
    ) -> u32 {
        if scope >= self.scopes.len() as u32 {
            panic!("Scope DNE in add_member_type");
        }

        let type_id = self.scope_push(Scope {
            name: Some(name.clone()),
            variant: ScopeVariant::Type(variant),
            parent_scope: scope,
            refers_to: None,
            members: HashMap::new(),
        });

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
