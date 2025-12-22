use std::collections::HashMap;

use crate::{
    constants::{FLOAT_TYPE, INTEGER_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprVariant, IdentifierVariant, Member, MemberVariant, Scope, ScopeVariant, TokenOrString, Tokens, TypeVariant, Visibility
    },
};

impl AST {
    // semantic analysis on particular expression
    fn semantic_analyze_expr(&mut self, tokens: &Tokens, scope: u32, expr: u32) {
        let expr_ref = self.expr(expr);

        match expr_ref.variant {
            ExprVariant::Unit
            | ExprVariant::IntegerLiteral(_)
            | ExprVariant::FloatLiteral(_)
            | ExprVariant::StringLiteral(_) => {
                // find built-in type id and set type of expr

                let type_name = match expr_ref.variant {
                    ExprVariant::Unit => UNIT_TYPE,
                    ExprVariant::IntegerLiteral(_) => INTEGER_TYPE,
                    ExprVariant::FloatLiteral(_) => FLOAT_TYPE,
                    ExprVariant::StringLiteral(_) => STRING_TYPE,
                    _ => panic!("only unit, integer, float here"),
                };

                if let Some(member) = self.scope_search(0, type_name) {
                    let type_id = self.members[member as usize].module_or_type;

                    self.exprs[expr as usize].etype = type_id;
                } else {
                    panic!("Builtin type not found: {type_name}");
                }
            }
            ExprVariant::Identifier(ident) => {
                // if it is a member then type should be determined when
                // analyzing access operator
                if ident.variant == IdentifierVariant::Member {
                    return;
                }
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
                (INTEGER_TYPE, TypeVariant::Integer),
                (FLOAT_TYPE, TypeVariant::Float),
                (UNIT_TYPE, TypeVariant::Unit),
                (STRING_TYPE, TypeVariant::String),
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

            self.semantic_analyze_expr(tokens, module_scope, expr);
        };
    }
}
