use std::collections::HashMap;

use crate::{
    parse::{
        AST, ExprVariant, MemberVariant, ScopeVariant, Type, TypeOrModule, TypeVariant, Visibility,
        ast_contents::{ExprID, MemberID, ScopeID, TypeID},
    },
    run::{ExecutionContext, error::RuntimeErrorVariant},
};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct RuntimeScopeID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ValueID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RuntimeReference {
    pub scope: RuntimeScopeID,
    pub value_id: ValueID,
}

#[derive(Clone, Debug)]
pub enum ValueVariant {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(MemberID),

    Record(HashMap<String, ValueID>),

    Ref(RuntimeReference),
    ImplicitRef(RuntimeReference),

    Module(ScopeID),
    Function(ExprID),
}

#[derive(Clone)]
pub struct Value {
    pub type_id: Option<TypeID>,
    pub variant: ValueVariant,
}

impl Value {
    pub fn to_runtime_ref(
        self,
        ctx: &mut ExecutionContext,
        scope: RuntimeScopeID,
    ) -> RuntimeReference {
        RuntimeReference {
            scope,
            value_id: ctx.objs.runtime_scope_mut(scope).value_push(self),
        }
    }
}

impl RuntimeReference {
    pub fn to_string(&self, ast: &AST, ctx: &ExecutionContext) -> String {
        let value = ctx.objs.ref_get(*self);

        match &value.variant {
            ValueVariant::Unit => "Unit".to_string(),
            ValueVariant::Integer(val) => val.to_string(),
            ValueVariant::Float(val) => val.to_string(),
            ValueVariant::Boolean(val) => val.to_string(),
            ValueVariant::String(s) => s.clone(),
            ValueVariant::Record(map) => {
                let member_strings: Vec<String> = map
                    .iter()
                    .map(|(name, value_id)| {
                        let value_string = RuntimeReference {
                            scope: self.scope,
                            value_id: *value_id,
                        }
                        .to_string(ast, ctx);

                        format!("{}={}", name, value_string)
                    })
                    .collect();

                format!("({})", member_strings.join(", "))
            }
            ValueVariant::Module(scope_id) => {
                let scope = ast.objs.scope(*scope_id);

                let name_string = match &scope.name {
                    Some(tok_or_string) => ctx.tokens.tok_or_string_to_string(tok_or_string),
                    None => "(anonymous)".to_string(),
                };

                format!("module {}", name_string)
            }
            ValueVariant::Function(func) => {
                let expr = ast.objs.expr(*func);

                let func_name_string = match &expr.variant {
                    ExprVariant::FunctionLiteral(func) => match &func.name {
                        Some(t) => ctx.tokens.tok_as_str(t),
                        None => "(anonymous)",
                    },
                    _ => "ERR_EXPR_IS_NOT_FUNC",
                };

                format!("function {}", func_name_string)
            }
            ValueVariant::Identifier(member_id) => match ctx.objs.instance_get(*member_id) {
                Some(runtime_ref) => runtime_ref.to_string(ast, ctx),
                None => "ERR_IDENT_DNE".to_string(),
            },
            ValueVariant::Ref(runtime_ref) | ValueVariant::ImplicitRef(runtime_ref) => {
                runtime_ref.to_string(ast, ctx)
            }
        }
    }

    pub fn dup_in_scope(
        &self,
        ast: &AST,
        ctx: &mut ExecutionContext,
        target_scope: RuntimeScopeID,
    ) -> Value {
        let value = ctx.objs.ref_get(*self).clone();

        let new_value = match &value.variant {
            ValueVariant::Unit
            | ValueVariant::Float(_)
            | ValueVariant::Boolean(_)
            | ValueVariant::Integer(_)
            | ValueVariant::Module(_)
            | ValueVariant::ImplicitRef(_)
            | ValueVariant::Ref(_)
            | ValueVariant::String(_)
            | ValueVariant::Function(_) => value,
            ValueVariant::Identifier(ident) => {
                return ctx
                    .objs
                    .instance_get(*ident)
                    .expect("should have been alloced")
                    .dup_in_scope(ast, ctx, target_scope);
            }
            ValueVariant::Record(map) => {
                let new_map: HashMap<String, ValueID> = map.iter().map(|(name, value_id)| {
                    let new_value = RuntimeReference {
                        scope: self.scope,
                        value_id: *value_id,
                    }.dup_in_scope(ast, ctx, target_scope);

                    (name.clone(), ctx.objs.runtime_scope_mut(target_scope).value_push(new_value))
                }).collect();

                Value { type_id: value.type_id, variant: ValueVariant::Record(new_map) }
            }
        };

        new_value
    }
}

#[derive(Default)]
pub struct RuntimeScope {
    pub members: HashMap<MemberID, ValueID>,

    pub values: Vec<Value>,

    pub parent: RuntimeScopeID,
}

impl RuntimeScope {
    pub fn value_push(&mut self, value: Value) -> ValueID {
        self.values.push(value);

        ValueID {
            id: self.values.len() as u32 - 1,
        }
    }

    pub fn value(&self, value: ValueID) -> &Value {
        &self.values[value.id as usize]
    }

    pub fn value_mut(&mut self, value: ValueID) -> &mut Value {
        &mut self.values[value.id as usize]
    }

    // ret new value
    pub fn value_overwrite(&mut self, value_id: ValueID, new_value: Value) {
        self.values[value_id.id as usize] = new_value;
    }
}

#[derive(Default)]
pub struct ContextObjects {
    scopes: Vec<RuntimeScope>,

    // find which scope given MemberID is in in O(1) so value can be retrieved
    member_map: HashMap<MemberID, RuntimeScopeID>,
}

fn type_to_value_id(ast: &AST, runtime_scope: &mut RuntimeScope, type_id: TypeID) -> ValueID {
    let variant = match ast.objs.type_get(type_id) {
        Type::Scope(scope) => match &ast.objs.scope(*scope).variant {
            ScopeVariant::Type(variant) => match variant {
                TypeVariant::Error => panic!("error type in type_to_value"),
                TypeVariant::Unit => ValueVariant::Unit,
                TypeVariant::Integer => ValueVariant::Integer(0),
                TypeVariant::Float => ValueVariant::Float(0.0),
                TypeVariant::String => ValueVariant::String("".to_string()),
                TypeVariant::Record => {
                    // filter map function will look at members of Record and for ones that are not global (i.e. shared)
                    // and are instances it will use this function type_to_value to create a corresponding value recursively

                    let values: HashMap<String, ValueID> = ast
                        .objs
                        .scope(*scope)
                        .members
                        .iter()
                        .filter_map(|(name, member_id)| {
                            let member = ast.objs.member(*member_id);

                            match (member.variant, member.visibility) {
                                (MemberVariant::Module, _) => return None,
                                (MemberVariant::Type, _) => return None,
                                (_, Visibility::Global) => return None,
                                _ => (),
                            };

                            let type_id = match member.type_or_module {
                                TypeOrModule::Type(t) => t,
                                TypeOrModule::Module(_) => return None,
                            };

                            let value_id = type_to_value_id(ast, runtime_scope, type_id);

                            Some((name.clone(), value_id))
                        })
                        .collect();

                    ValueVariant::Record(values)
                }
                _ => todo!("not implemented"),
            },
            _ => {
                panic!("scope retrieved using type id is not a type?");
            }
        },
        _ => todo!("have not implemented in interpreter other sorts of types yet"),
    };

    runtime_scope.value_push(Value {
        type_id: Some(type_id),
        variant,
    })
}

impl ContextObjects {
    pub fn runtime_scope_push(&mut self, scope: RuntimeScope) -> RuntimeScopeID {
        self.scopes.push(scope);

        RuntimeScopeID {
            id: self.scopes.len() as u32 - 1,
        }
    }

    pub fn runtime_scope_new(&mut self) -> RuntimeScopeID {
        self.runtime_scope_push(RuntimeScope::default())
    }

    pub fn runtime_scope(&self, scope: RuntimeScopeID) -> &RuntimeScope {
        &self.scopes[scope.id as usize]
    }

    pub fn runtime_scope_mut(&mut self, scope: RuntimeScopeID) -> &mut RuntimeScope {
        &mut self.scopes[scope.id as usize]
    }

    pub fn scope_instance_get(&self, scope: RuntimeScopeID, member_id: MemberID) -> ValueID {
        self.runtime_scope(scope).members[&member_id]
    }

    pub fn ref_get(&self, runtime_ref: RuntimeReference) -> &Value {
        self.runtime_scope(runtime_ref.scope)
            .value(runtime_ref.value_id)
    }

    pub fn ref_get_mut(&mut self, runtime_ref: RuntimeReference) -> &mut Value {
        self.runtime_scope_mut(runtime_ref.scope)
            .value_mut(runtime_ref.value_id)
    }

    // return scope and value for full locat
    pub fn instance_get(&self, member_id: MemberID) -> Option<RuntimeReference> {
        let scope = *self.member_map.get(&member_id)?;

        Some(RuntimeReference {
            scope,
            value_id: self.scope_instance_get(scope, member_id),
        })
    }

    // give new value to member
    pub fn instance_set(&mut self, member_id: MemberID, new_value: Value) {
        let scope_id = *(self.member_map.get(&member_id).expect("member not allocated"));

        let scope = self.runtime_scope_mut(scope_id);

        let value_id = scope.members[&member_id];

        scope.value_overwrite(value_id, new_value);
    }

    pub fn instance_alloc(
        &mut self,
        ast: &AST,
        scope: RuntimeScopeID,
        member_id: MemberID,
    ) -> RuntimeReference {
        let member = ast.objs.member(member_id);

        let type_id = match member.type_or_module {
            TypeOrModule::Type(t) => t,
            TypeOrModule::Module(_) => {
                panic!(
                    "attempted to allocate module in interpreter, doesn't make sense/isn't supported"
                );
            }
        };

        self.member_map.insert(member_id, scope);

        let scope_obj = self.runtime_scope_mut(scope);

        let value_id = type_to_value_id(ast, scope_obj, type_id);

        // record in member_map mapping from MemberID -> runtime id so Value
        // can be retrieved in O(1) time in the future

        scope_obj.members.insert(member_id, value_id);

        RuntimeReference { scope, value_id }
    }
}
