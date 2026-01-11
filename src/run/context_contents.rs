use std::collections::HashMap;

use crate::parse::{
    AST, MemberVariant, ScopeVariant, Type, TypeOrModule, TypeVariant, Visibility,
    ast_contents::{ExprID, MemberID, ScopeID, TypeID},
};

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct RuntimeScopeID {
    id: u32,
}

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct ValueID {
    id: u32,
}

#[derive(Clone)]
pub enum ValueVariant {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(MemberID),
    Record(HashMap<String, Box<Value>>),

    // e.g. left MemberID is for a Point struct and right MemberID is for field "x"
    Access((MemberID, MemberID)),

    Module(ScopeID),
    Function(ExprID),
}

#[derive(Clone)]
pub struct Value {
    pub type_id: Option<TypeID>,
    pub variant: ValueVariant,
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

        ValueID { id: self.values.len() as u32 - 1 }
    }

    pub fn value(&self, value: ValueID) -> &Value {
        &self.values[value.id as usize]
    }

    pub fn value_mut(&mut self, value: ValueID) -> &mut Value {
        &mut self.values[value.id as usize]
    }
}

#[derive(Default)]
pub struct ContextObjects {
    scopes: Vec<RuntimeScope>,

    // find which scope given MemberID is in in O(1) so value can be retrieved
    member_map: HashMap<MemberID, RuntimeScopeID>,
}

fn type_to_value(ast: &AST, type_id: TypeID) -> Value {
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

                    let values: HashMap<String, Box<Value>> = ast
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

                            Some((name.clone(), Box::new(type_to_value(ast, type_id))))
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

    Value {
        type_id: Some(type_id),
        variant,
    }
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

    pub fn instance_get(&self, member_id: MemberID) -> Option<ValueID> {
        Some(self.scope_instance_get(*self.member_map.get(&member_id)?, member_id))
    }

    pub fn instance_alloc(&mut self, ast: &AST, scope: RuntimeScopeID, member_id: MemberID) {
        let member = ast.objs.member(member_id);

        let type_id = match member.type_or_module {
            TypeOrModule::Type(t) => t,
            TypeOrModule::Module(_) => {
                return;
            }
        };

        let value = type_to_value(ast, type_id);

        // record in member_map mapping from MemberID -> runtime id so Value
        // can be retrieved in O(1) time in the future

        self.member_map.insert(member_id, scope);

        let scope_obj = self.runtime_scope_mut(scope);

        scope_obj.members.insert(member_id, value);
    }
}
