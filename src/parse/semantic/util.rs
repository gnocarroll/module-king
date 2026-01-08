use std::collections::HashMap;

use crate::{constants::ERROR_TYPE, parse::{
    AST, Member, MemberVariant, Scope, ScopeRefersTo, ScopeVariant, TokenOrString, Type, TypeOrModule, TypeVariant, Visibility, ast_contents::{ExprID, MemberID, ScopeID, TypeID}, errors::{InvalidOperation, MissingOperand, PatternError, SemanticError}, semantic::SemanticContext
}};

impl AST {
    pub fn get_builtin_type(&mut self, name: &str) -> MemberID {
        if let Some(member) = self.scope_search(ScopeID::default(), name) {
            if self.objs.member(member).variant != MemberVariant::Type {
                panic!("Not a type")
            }

            member
        } else {
            panic!("Builtin type not found: {name}");
        }
    }

    pub fn pattern_error_push(&mut self, pattern_error: PatternError) {
        self.semantic_errors
            .push(SemanticError::PatternError(pattern_error))
    }

    pub fn get_builtin_type_id(&mut self, name: &str) -> TypeID {
        let member = self.get_builtin_type(name);

        match self.objs.member(member).type_or_module {
            TypeOrModule::Type(t) => t,
            _ => panic!("builtin type should not be a module"),
        }
    }

    pub fn missing_operand(&mut self, expr: ExprID, operand: u32) -> SemanticError {
        let ret = SemanticError::MissingOperand(MissingOperand {
            operation: expr,
            operand_missing: operand,
        });

        self.semantic_errors.push(ret.clone());

        ret
    }

    pub fn invalid_operation(&mut self, expr: ExprID, msg: &'static str) -> SemanticError {
        let ret = SemanticError::InvalidOperation(InvalidOperation {
            operation: expr,
            msg,
        });

        self.semantic_errors.push(ret.clone());

        ret
    }

    // ret: member id of instance
    pub fn scope_add_instance(
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

    // search provided scope for a given name and received Member if said name
    // can be found, also recurse to parent if needed
    pub fn scope_search(&mut self, scope: ScopeID, name: &str) -> Option<MemberID> {
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

    pub fn type_push_scope(&mut self, scope: ScopeID) -> TypeID {
        let type_id = self.objs.type_push(Type::Scope(scope));

        self.objs.scope_mut(scope).refers_to = Some(ScopeRefersTo::Type(type_id));

        type_id
    }

    pub fn scope_add_member(&mut self, ctx: &mut SemanticContext, scope: ScopeID, member: MemberID) {
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

    pub fn type_create(&mut self, scope: ScopeID, name: TokenOrString, variant: TypeVariant) -> TypeID {
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
    pub fn scope_add_member_type_from_id(
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
    pub fn scope_add_member_type(
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
}
