use std::collections::HashMap;

use crate::parse::{
    AST, Member, MemberVariant, Scope, ScopeVariant, TokenOrString, Tokens, TypeVariant, Visibility,
};

impl AST {
    // semantic analysis on particular expression
    fn semantic_analyze_expr(&mut self, scope: u32, expr: u32) {
        let expr_ref = self.expr(expr);

        match expr_ref.variant {}
    }

    fn scope_push(&mut self, scope: Scope) -> u32 {
        self.scopes.push(scope);

        return self.scopes.len() as u32 - 1;
    }

    fn member_push(&mut self, member: Member) -> u32 {
        self.members.push(member);

        return self.members.len() as u32 - 1;
    }

    fn scope_add_member(&mut self, tokens: &Tokens, scope: u32, member: u32) {
        let member_name = match &self.members[member as usize].name {
            TokenOrString::Token(t) => tokens.tok_as_str(t),
            TokenOrString::String(s) => s.as_str(),
        };

        self.scopes[scope as usize]
            .members
            .insert(member_name.to_string(), member);
    }

    // return is the id of the Scope which represents the type
    fn scope_add_member_type(
        &mut self,
        tokens: &Tokens,
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
            members: HashMap::new(),
        });

        let member_id = self.member_push(Member {
            name: name,
            visibility: Visibility::Private,
            variant: MemberVariant::Type,
            module_or_type: type_id,
        });

        self.scope_add_member(tokens, scope, member_id);

        type_id
    }

    // public function to do semantic analysis
    pub fn do_semantic_analysis(&mut self, tokens: &Tokens, module_name: &str) {
        if let Some(expr) = self.root_expr {
            // create global scope and add built-ins

            let global_scope = self.scope_push(Scope {
                name: Some(TokenOrString::String("GLOBAL".to_string())),
                variant: ScopeVariant::Module,
                parent_scope: 0,
                members: HashMap::new(),
            });

            for (name, variant) in [
                ("Integer", TypeVariant::Integer),
                ("Float", TypeVariant::Float),
            ] {
                self.scope_add_member_type(
                    tokens,
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
                members: HashMap::new(),
            });

            // analyze root expr and provided new scope as scope

            self.semantic_analyze_expr(module_scope, expr);
        };
    }
}
