use crate::{
    constants::{ERROR_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, FunctionLiteral, Identifier, Member, MemberVariant, Scope,
        ScopeRefersTo, ScopeVariant, TokenOrString, Type, TypeOrModule, TypeVariant, Visibility,
        ast_contents::{ExprID, FunctionID, MemberID, ScopeID, TypeID},
        errors::{DuplicateName, InvalidOperation, MissingOperand, PatternError, SemanticError},
        semantic::SemanticContext,
    },
    scan::Token,
    tokens::Tokens,
};

impl AST {
    pub fn get_builtin_type(&self, name: &str) -> MemberID {
        if let Some(member) = self.scope_search(ScopeID::global(), name) {
            if let MemberVariant::Type(_) = self.objs.member(member).variant {
                member
            } else {
                panic!("Not a type")
            }
        } else {
            panic!("Builtin type not found: {name}");
        }
    }

    pub fn pattern_error_push(&mut self, pattern_error: PatternError) -> PatternError {
        self.semantic_errors
            .push(SemanticError::PatternError(pattern_error.clone()));

        pattern_error
    }

    pub fn get_builtin_type_id(&self, name: &str) -> TypeID {
        let member = self.get_builtin_type(name);

        match self.objs.member(member).variant {
            MemberVariant::Type(t) => t,
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
    ) -> Result<MemberID, DuplicateName> {
        let type_id = match type_id {
            Some(id) => id,
            None => self.get_builtin_type_id(ERROR_TYPE),
        };

        let member_id = self.objs.member_push(Member {
            name,
            visibility: Visibility::Private,
            variant: MemberVariant::Instance(type_id),
        });

        self.scope_try_insert(ctx.tokens, scope, member_id)
    }

    pub fn set_expr_returns_unit(&mut self, _ctx: &mut SemanticContext, expr: ExprID) {
        let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = unit_type_id;
    }

    // build tuple type of indefinite length from vector of type IDs
    // if vec is of length 0 then Unit type is returned
    pub fn type_vec_to_tuple(&mut self, type_vec: &Vec<TypeID>) -> TypeID {
        if type_vec.len() == 0 {
            return self.get_builtin_type_id(UNIT_TYPE);
        } else if type_vec.len() == 1 {
            return self.objs.type_push(Type::Tuple((type_vec[0], None)));
        } else if type_vec.len() == 2 {
            return self
                .objs
                .type_push(Type::Tuple((type_vec[0], Some(type_vec[1]))));
        }

        let mut rest_of_tuple = self
            .objs
            .type_push(Type::RestOfTuple((type_vec[1], type_vec[2])));

        let ret = self
            .objs
            .type_push(Type::Tuple((type_vec[0], Some(rest_of_tuple))));

        for idx in 2..(type_vec.len() - 1) {
            let new_rest_of_tuple = self
                .objs
                .type_push(Type::RestOfTuple((type_vec[idx], type_vec[idx + 1])));

            *self.objs.type_mut(rest_of_tuple) =
                Type::RestOfTuple((type_vec[idx - 1], new_rest_of_tuple));

            rest_of_tuple = new_rest_of_tuple;
        }

        ret
    }

    // utility function get ID of current return type
    // (so assumption is that function is being analyzed)
    pub fn get_curr_return_type(&mut self, ctx: &mut SemanticContext) -> TypeID {
        let curr_func = ctx
            .curr_func
            .expect("tried to get current return type without current function");

        self.objs.function(curr_func).return_type
    }

    // search provided scope for a given name and received Member if said name
    // can be found, also recurse to parent if needed
    pub fn scope_search(&self, scope: ScopeID, name: &str) -> Option<MemberID> {
        let mut scope = scope;
        let name = name.to_string();

        loop {
            let scope_ref = self.objs.scope(scope);

            if let Some(member) = scope_ref.members.get(&name) {
                return Some(member);
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

    pub fn type_create(
        &mut self,
        scope: ScopeID,
        name: TokenOrString,
        variant: TypeVariant,
    ) -> TypeID {
        let scope_id = self.objs.scope_push(Scope {
            name: Some(name.clone()),
            variant: ScopeVariant::Type(variant),
            parent_scope: scope,
            refers_to: None,
            ..Default::default()
        });

        self.objs.type_push(Type::Scope(scope_id))
    }

    // provide scope, type id to add type to scope as a member
    // ret: member id
    pub fn _scope_add_member_type_from_id(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        type_id: TypeID,
    ) -> Result<MemberID, DuplicateName> {
        let name = match self.objs.type_get(type_id) {
            Type::Scope(scope) => self.objs.scope(*scope).name.clone(),
            _ => None,
        };

        let member_id = self.objs.member_push(Member {
            name: name.expect("CURRENTLY CAN ONLY ADD NAMED TYPE AS SCOPE MEMBER"),
            visibility: Visibility::Private,
            variant: MemberVariant::Type(type_id),
        });

        self.scope_try_insert(ctx.tokens, scope, member_id)
    }

    // return is the id of the Scope which represents the type
    pub fn scope_add_member_type_from_name_and_id(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        name: TokenOrString,
        type_id: TypeID,
    ) -> Result<MemberID, DuplicateName> {
        let type_id = if let Type::Scope(scope) = self.objs.type_get(type_id) {
            let scope_mut = self.objs.scope_mut(*scope);

            if scope_mut.name.is_none() {
                scope_mut.name = Some(name.clone());

                type_id
            } else {
                // we have name but type_id provided already corresponds to named scope
                // => type being created is alias for another type, so get new TypeID
                self.objs.type_push(Type::Alias(type_id))
            }
        } else {
            type_id
        };

        let member_id = self.objs.member_push(Member {
            name: name,
            visibility: Visibility::Private,
            variant: MemberVariant::Type(type_id),
        });

        self.scope_try_insert(ctx.tokens, scope, member_id)
    }

    // return is the id of the Scope which represents the type
    pub fn scope_add_member_type(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        name: TokenOrString,
        variant: TypeVariant,
    ) -> Result<TypeID, DuplicateName> {
        let type_id = self.type_create(scope, name.clone(), variant);

        let member_id = self.objs.member_push(Member {
            name: name,
            visibility: Visibility::Private,
            variant: MemberVariant::Type(type_id),
        });

        self.scope_try_insert(ctx.tokens, scope, member_id)?;

        Ok(type_id)
    }

    pub fn _member_type_or_module(&mut self, member: MemberID) -> TypeOrModule {
        match self.objs.member(member).variant {
            MemberVariant::Function(function_id) => {
                TypeOrModule::Type(self.objs.function(function_id).func_type)
            }
            MemberVariant::Instance(type_id) => TypeOrModule::Type(type_id),
            MemberVariant::Type(type_id) => TypeOrModule::Type(type_id),
            MemberVariant::Module(scope_id) => TypeOrModule::Module(scope_id),
        }
    }

    pub fn scope_try_insert(
        &mut self,
        tokens: &Tokens,
        scope: ScopeID,
        member_id: MemberID,
    ) -> Result<MemberID, DuplicateName> {
        let name = &self.objs.member(member_id).name;

        let name = tokens.tok_or_string_to_string(name);

        if let Err(e) = self.objs.scope_mut(scope).members.insert(name, member_id) {
            self.semantic_errors
                .push(SemanticError::DuplicateName(e.clone()));

            return Err(e);
        }

        Ok(member_id)
    }

    pub fn scope_add_function(
        &mut self,
        ctx: &Tokens,
        scope: ScopeID,
        name: Token,
        function: FunctionID,
    ) -> Result<MemberID, DuplicateName> {
        // create member struct and get ID

        let member_id = self.objs.member_push(Member {
            name: TokenOrString::Token(name),
            visibility: Visibility::Export,
            variant: MemberVariant::Function(function),
        });

        // insert to provided scope

        self.scope_try_insert(ctx, scope, member_id)?;

        Ok(member_id)
    }

    pub fn expr_returns(&self, expr: ExprID) -> ExprReturns {
        let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);
        let expr = self.objs.expr(expr);

        if expr.type_id == unit_type_id {
            return ExprReturns::Unit;
        }

        match self.objs.type_get(expr.type_id) {
            Type::AnyType | Type::Type(_) => ExprReturns::Type,
            Type::AnyModule | Type::Module(_) => ExprReturns::Module,
            _ => ExprReturns::Value,
        }
    }

    pub fn expr_get_function_id(&self, expr: ExprID) -> Option<FunctionID> {
        match self.objs.expr(expr).variant {
            ExprVariant::FunctionLiteral(FunctionLiteral { function_id, .. }) => Some(function_id),
            ExprVariant::Identifier(Identifier { member_id, .. }) => {
                match self.objs.member(member_id).variant {
                    MemberVariant::Function(function_id) => Some(function_id),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn type_get_scope_id(&self, type_id: TypeID) -> Option<ScopeID> {
        let type_id = self.type_resolve_aliasing(type_id);

        match self.objs.type_get(type_id) {
            Type::Alias(_) => panic!("should have removed aliasing"),
            Type::Scope(scope_id) => Some(*scope_id),
            _ => None,
        }
    }

    pub fn expr_get_scope_id(&self, expr: ExprID) -> Option<ScopeID> {
        let expr_struct = self.objs.expr(expr);

        let maybe_ret = match &expr_struct.variant {
            ExprVariant::Identifier(Identifier { member_id, .. }) => {
                let member = self.objs.member(*member_id);

                match member.variant {
                    MemberVariant::Module(scope_id) => Some(scope_id),
                    MemberVariant::Type(type_id) => self.type_get_scope_id(type_id),
                    MemberVariant::Function(function_id) => {
                        Some(self.objs.function(function_id).scope)
                    }
                    MemberVariant::Instance(_) => None,
                }
            }
            ExprVariant::FunctionLiteral(function_literal) => {
                Some(self.objs.function(function_literal.function_id).scope)
            }
            _ => None,
        };

        if maybe_ret.is_some() {
            return maybe_ret;
        }

        let type_id = self.type_resolve_aliasing(expr_struct.type_id);

        match self.objs.type_get(type_id) {
            Type::Module(scope_id) | Type::ImportTarget(scope_id) => Some(*scope_id),
            Type::Type(type_id) => self.type_get_scope_id(*type_id),
            _ => None,
        }
    }

    pub fn type_resolve_aliasing(&self, type_id: TypeID) -> TypeID {
        let mut ret = type_id;

        loop {
            match self.objs.type_get(type_id) {
                Type::Alias(t) => {
                    ret = *t;
                }
                _ => {
                    break;
                }
            }
        }

        ret
    }

    pub fn type_get_variant(&self, type_id: TypeID) -> Option<TypeVariant> {
        match self.objs.type_get(type_id) {
            Type::Scope(scope_id) => match self.objs.scope(*scope_id).variant {
                ScopeVariant::Type(variant) => Some(variant),
                _ => panic!("should be a type and thus guaranteed to have TypeVariant"),
            },
            _ => None,
        }
    }

    pub fn type_eq(&self, t1: TypeID, t2: TypeID) -> bool {
        let t1 = self.type_resolve_aliasing(t1);
        let t2 = self.type_resolve_aliasing(t2);

        if t1 == t2 {
            return true;
        }

        if self.objs.type_get(t1) == self.objs.type_get(t2) {
            return true;
        }

        match (self.objs.type_get(t1), self.objs.type_get(t2)) {
            (Type::Ptr(t1), Type::Ptr(t2))
            | (Type::Ref(t1), Type::Ref(t2))
            | (Type::Map(t1), Type::Map(t2))
            | (Type::List(t1), Type::List(t2)) => {
                return self.type_eq(*t1, *t2);
            }
            (Type::Function((params1, ret1)), Type::Function((params2, ret2))) => {
                return self.type_eq(*params1, *params2) && self.type_eq(*ret1, *ret2);
            }
            (Type::Module(_), Type::Module(_)) | (Type::Type(_), Type::Type(_)) => return true,
            (Type::Scope(s1), Type::Scope(s2)) => {
                // TODO: in the future could check for unnamed record types e.g. if they have same members in same order

                return *s1 == *s2;
            }
            _ => (),
        }

        let mut t1_iter = t1.to_tuple_iterator(self);
        let mut t2_iter = t2.to_tuple_iterator(self);

        loop {
            let t1 = t1_iter.next();
            let t2 = t2_iter.next();

            match (t1, t2) {
                (None, None) => return true,
                (Some(_), None) => return false,
                (None, Some(_)) => return false,
                (Some(t1), Some(t2)) => {
                    if !self.type_eq(t1, t2) {
                        return false;
                    }
                }
            }
        }
    }
}
