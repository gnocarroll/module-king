use std::collections::HashMap;

use crate::{
    parse::{
        AST, MemberVariant, ScopeVariant, Type, TypeVariant, Visibility,
        ast_contents::{FunctionID, MemberID, ScopeID, TypeID},
        builtin::Builtin,
    },
    run::ExecutionContext,
};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct RuntimeScopeID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ValueID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct RetLocationID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct RuntimeReference {
    pub scope: RuntimeScopeID,
    pub value_id: ValueID,
}

impl RuntimeReference {
    pub fn to_tuple_value_iterator<'a>(&self, objs: &'a ContextObjects) -> TupleValueIterator<'a> {
        TupleValueIterator {
            idx: 0,
            scope: objs.runtime_scope(self.scope),
            value_id: self.value_id,
        }
    }
}

// special struct to store reference to a character in a String
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CharReference {
    pub string_rref: RuntimeReference,
    pub idx: usize,
}

impl CharReference {
    pub fn load(&self, ctx: &ExecutionContext) -> u8 {
        match &ctx.objs.ref_get(self.string_rref).variant {
            ValueVariant::String(vec) => {
                vec[self.idx]
            }
            _ => panic!("string ref in CharReference should have ValueVariant::String"),
        }
    }

    pub fn store(&self, ctx: &mut ExecutionContext, c: u8) {
        match &mut ctx.objs.ref_get_mut(self.string_rref).variant {
            ValueVariant::String(vec) => {
                vec[self.idx] = c;
            }
            _ => panic!("string ref in CharReference should have ValueVariant::String"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueVariant {
    Unit,

    // TypeID is contained type
    // e.g. for expr "Integer" it would be ID of Integer type
    Type(TypeID),

    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Identifier(MemberID),

    // tuples are "flattened" unlike linked-list structure used elsewhere in codebase
    Tuple(Vec<ValueID>),

    Record(HashMap<String, ValueID>),

    Ref(RuntimeReference),
    ImplicitRef(RuntimeReference),

    Module(ScopeID),
    Function(FunctionID),

    Builtin(Builtin),

    CharReference(CharReference),

    // Containers (String is also a container but it is above)
    List(Vec<ValueID>),
    Map(HashMap<String, ValueID>),
}

#[derive(Clone)]
pub struct Value {
    pub type_id: Option<TypeID>,
    pub variant: ValueVariant,
}

pub struct TupleValueIterator<'a> {
    idx: usize,
    scope: &'a RuntimeScope,
    value_id: ValueID,
}

impl Iterator for TupleValueIterator<'_> {
    type Item = ValueID;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.scope.value(self.value_id);

        match &value.variant {
            ValueVariant::Tuple(value_id_vec) => {
                if self.idx <= value_id_vec.len() {
                    self.idx += 1;
                }

                value_id_vec.get(self.idx - 1).map(|id| *id)
            }
            _ => {
                if self.idx == 0 {
                    self.idx += 1;

                    Some(self.value_id)
                } else {
                    None
                }
            }
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value {
            type_id: Some(TypeID::unit()),
            variant: ValueVariant::Unit,
        }
    }
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
            ValueVariant::Type(type_id) => {
                format!("type({})", ast.type_to_string(ctx.tokens, *type_id),)
            }
            ValueVariant::Tuple(values) | ValueVariant::List(values) => {
                let variant_str = match value.variant {
                    ValueVariant::Tuple(_) => "tuple",
                    ValueVariant::List(_) => "List",
                    _ => panic!("should be guaranteed to be Tuple or List"),
                };

                // get string for each value

                let element_strings: Vec<String> = values
                    .iter()
                    .map(|value_id| {
                        RuntimeReference {
                            scope: self.scope,
                            value_id: *value_id,
                        }
                        .to_string(ast, ctx)
                    })
                    .collect();

                // combine

                format!("{}({})", variant_str, element_strings.join(", "),)
            }
            ValueVariant::Unit => "Unit".to_string(),
            ValueVariant::Integer(val) => val.to_string(),
            ValueVariant::Float(val) => val.to_string(),
            ValueVariant::Boolean(val) => val.to_string(),
            ValueVariant::String(s) => std::str::from_utf8(s)
                .expect("user was able to create invalid utf8, should be impossible")
                .to_string(),
            ValueVariant::Builtin(builtin) => format!("(builtin {})", builtin.get_builtin_name(),),
            ValueVariant::Record(map) | ValueVariant::Map(map) => {
                let variant_str = match value.variant {
                    ValueVariant::Record(_) => "record",
                    ValueVariant::Map(_) => "Map",
                    _ => panic!("should be guarnateed to be Record or Map"),
                };

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

                format!("{}({})", variant_str, member_strings.join(", "),)
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
                let func = ast.objs.function(*func);

                let func_name_str = match func.name {
                    Some(t) => ctx.tokens.tok_as_str(&t),
                    None => "(anonymous)",
                };

                format!("function {}", func_name_str)
            }
            ValueVariant::Identifier(member_id) => match ctx.objs.instance_get(*member_id) {
                Some(runtime_ref) => runtime_ref.to_string(ast, ctx),
                None => "ERR_IDENT_DNE".to_string(),
            },
            ValueVariant::Ref(runtime_ref) | ValueVariant::ImplicitRef(runtime_ref) => {
                runtime_ref.to_string(ast, ctx)
            }
            ValueVariant::CharReference(CharReference {
                string_rref,
                idx,
            }) => {

            }
        }
    }

    pub fn dup_in_scope_get_id(
        &self,
        ast: &AST,
        ctx: &mut ExecutionContext,
        target_scope: RuntimeScopeID,
    ) -> ValueID {
        let value = self.dup_in_scope(ast, ctx, target_scope);

        ctx.objs.runtime_scope_mut(target_scope).value_push(value)
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
            | ValueVariant::Function(_)
            | ValueVariant::Type(_)
            | ValueVariant::Builtin(_) => value,
            ValueVariant::Identifier(ident) => {
                return ctx
                    .objs
                    .instance_get(*ident)
                    .expect("should have been alloced")
                    .dup_in_scope(ast, ctx, target_scope);
            }
            ValueVariant::Tuple(value_id_vec) | ValueVariant::List(value_id_vec) => {
                let to_variant = {
                    match value.variant {
                        ValueVariant::Tuple(_) => ValueVariant::Tuple,
                        ValueVariant::List(_) => ValueVariant::List,
                        _ => panic!("should be List or Tuple"),
                    }
                };

                let new_vec: Vec<ValueID> = value_id_vec
                    .iter()
                    .map(|value_id| {
                        let new_value = RuntimeReference {
                            scope: self.scope,
                            value_id: *value_id,
                        }
                        .dup_in_scope(ast, ctx, target_scope);

                        ctx.objs
                            .runtime_scope_mut(target_scope)
                            .value_push(new_value)
                    })
                    .collect();

                Value {
                    type_id: value.type_id,
                    variant: to_variant(new_vec),
                }
            }
            ValueVariant::Record(map) | ValueVariant::Map(map) => {
                let to_variant = {
                    match value.variant {
                        ValueVariant::Record(_) => ValueVariant::Record,
                        ValueVariant::Map(_) => ValueVariant::Map,
                        _ => panic!("should be List or Tuple"),
                    }
                };

                let new_map: HashMap<String, ValueID> = map
                    .iter()
                    .map(|(name, value_id)| {
                        let new_value = RuntimeReference {
                            scope: self.scope,
                            value_id: *value_id,
                        }
                        .dup_in_scope(ast, ctx, target_scope);

                        (
                            name.clone(),
                            ctx.objs
                                .runtime_scope_mut(target_scope)
                                .value_push(new_value),
                        )
                    })
                    .collect();

                Value {
                    type_id: value.type_id,
                    variant: to_variant(new_map),
                }
            }
        };

        new_value
    }
}

pub struct RuntimeScope {
    pub members: HashMap<MemberID, ValueID>,

    pub values: Vec<Value>,

    pub parent: RuntimeScopeID,
}

impl Default for RuntimeScope {
    fn default() -> Self {
        // make it so that ValueID 0 can be bogus => panic if access attempted

        RuntimeScope {
            members: HashMap::new(),
            values: vec![Value::default()],
            parent: RuntimeScopeID::default(),
        }
    }
}

impl RuntimeScope {
    pub fn value_push(&mut self, value: Value) -> ValueID {
        self.values.push(value);

        ValueID {
            id: self.values.len() as u32 - 1,
        }
    }

    pub fn value(&self, value: ValueID) -> &Value {
        if value.id == 0 {
            panic!("attempted to access bogus ValueID 0");
        }

        &self.values[value.id as usize]
    }

    pub fn value_mut(&mut self, value: ValueID) -> &mut Value {
        if value.id == 0 {
            panic!("attempted to access bogus ValueID 0");
        }

        &mut self.values[value.id as usize]
    }

    // wipe provided ValueID by replacing value with Unit
    pub fn value_set_unit(&mut self, value: ValueID) {
        if value.id == 0 {
            panic!("attempted to access bogus ValueID 0");
        }

        self.values[value.id as usize] = Value {
            type_id: Some(TypeID::unit()),
            variant: ValueVariant::Unit,
        }
    }

    // ret new value
    pub fn value_overwrite(&mut self, value_id: ValueID, new_value: Value) {
        if value_id.id == 0 {
            panic!("attempted to access bogus ValueID 0");
        }

        self.values[value_id.id as usize] = new_value;
    }
}

pub struct ContextObjects {
    scopes: Vec<RuntimeScope>,

    // find which scope given MemberID is in in O(1) so value can be retrieved
    member_map: HashMap<MemberID, RuntimeScopeID>,

    // when function is returning value will be placed at top of here
    ret_locations: Vec<RuntimeReference>,
}

impl Default for ContextObjects {
    fn default() -> Self {
        // put in one RuntimeScope so that ID 0 can be a bogus ID
        // makes sense since RuntimeScopeID::default() returns 0 and you don't want that used

        ContextObjects {
            scopes: vec![RuntimeScope::default()],
            member_map: HashMap::new(),
            ret_locations: vec![RuntimeReference::default()],
        }
    }
}

fn types_to_tuple_value_id(
    ast: &AST,
    runtime_scope: &mut RuntimeScope,
    type_id: TypeID,
    t1: TypeID,
    maybe_t2: Option<TypeID>,
) -> ValueID {
    let mut value_id_vec = vec![type_to_value_id(ast, runtime_scope, t1)];

    if let Some(t2) = maybe_t2 {
        let mut extended_vec = false;
        let value_id2 = type_to_value_id(ast, runtime_scope, t2);

        match ast.objs.type_get(t2) {
            Type::Unit => (),
            Type::RestOfTuple(_) => match &runtime_scope.value(value_id2).variant {
                ValueVariant::Tuple(value_id_vec2) => {
                    value_id_vec.extend_from_slice(value_id_vec2);

                    extended_vec = true;
                }
                _ => panic!("ValueVariant should be guaranteed to be tuple"),
            },
            _ => {
                value_id_vec.push(value_id2);
            }
        }

        // if we extended vec can wipe other ValueID that had second ValueID vec
        // since it is not needed

        if extended_vec {
            runtime_scope.value_set_unit(value_id2);
        }
    }

    runtime_scope.value_push(Value {
        type_id: Some(type_id),
        variant: ValueVariant::Tuple(value_id_vec),
    })
}

fn type_to_value_id(ast: &AST, runtime_scope: &mut RuntimeScope, type_id: TypeID) -> ValueID {
    let variant = match ast.objs.type_get(type_id) {
        Type::Error => panic!("error type in type_to_value"),
        Type::Unit => ValueVariant::Unit,
        Type::String => ValueVariant::String(b"".to_vec()),
        Type::Scope(scope) => match &ast.objs.scope(*scope).variant {
            ScopeVariant::Type(variant) => match variant {
                TypeVariant::Integer => ValueVariant::Integer(0),
                TypeVariant::Float => ValueVariant::Float(0.0),
                TypeVariant::Record => {
                    // filter map function will look at members of Record and for ones that are not global (i.e. shared)
                    // and are instances it will use this function type_to_value to create a corresponding value recursively

                    let values: HashMap<String, ValueID> = ast
                        .objs
                        .scope(*scope)
                        .members
                        .get_map()
                        .iter()
                        .filter_map(|(name, member_id)| {
                            let member = ast.objs.member(*member_id);

                            let type_id = match (member.variant, member.visibility) {
                                (_, Visibility::Global) => return None,
                                (MemberVariant::Instance(t), _) => t,
                                (_, _) => return None,
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
        Type::Tuple((t1, maybe_t2)) => {
            return types_to_tuple_value_id(ast, runtime_scope, type_id, *t1, *maybe_t2);
        }
        Type::RestOfTuple((t1, t2)) => {
            return types_to_tuple_value_id(ast, runtime_scope, type_id, *t1, Some(*t2));
        }
        _ => todo!("have not implemented in interpreter other sorts of types yet"),
    };

    runtime_scope.value_push(Value {
        type_id: Some(type_id),
        variant,
    })
}

impl ContextObjects {
    pub fn runtime_scope_child(&mut self, parent: RuntimeScopeID) -> RuntimeScopeID {
        if parent.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        }

        self.runtime_scope_push(RuntimeScope {
            parent,
            ..Default::default()
        })
    }

    pub fn runtime_scope_push(&mut self, scope: RuntimeScope) -> RuntimeScopeID {
        self.scopes.push(scope);

        RuntimeScopeID {
            id: self.scopes.len() as u32 - 1,
        }
    }

    // must be top scope
    // ID is provided to ensure that something has not gone wrong
    // if ID does not line up or there is no top scope kill program (panic)
    pub fn runtime_scope_delete(&mut self, scope: RuntimeScopeID) {
        if scope.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        }

        let last_scope_id = self.scopes.len() as u32 - 1;

        if last_scope_id != scope.id {
            panic!(
                "attempted to delete scope ID {} but last was {}",
                scope.id, last_scope_id,
            );
        }

        self.scopes.pop();
    }

    pub fn runtime_scope_new(&mut self) -> RuntimeScopeID {
        self.runtime_scope_push(RuntimeScope::default())
    }

    pub fn runtime_scope(&self, scope: RuntimeScopeID) -> &RuntimeScope {
        if scope.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        }

        &self.scopes[scope.id as usize]
    }

    pub fn runtime_scope_mut(&mut self, scope: RuntimeScopeID) -> &mut RuntimeScope {
        if scope.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        }

        &mut self.scopes[scope.id as usize]
    }

    pub fn scope_instance_get(&self, scope: RuntimeScopeID, member_id: MemberID) -> ValueID {
        if scope.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        } else if member_id == MemberID::default() {
            panic!("provided bogus MemberID 0");
        }

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
        if member_id == MemberID::default() {
            panic!("provided bogus MemberID 0");
        }

        let scope = *self.member_map.get(&member_id)?;

        Some(RuntimeReference {
            scope,
            value_id: self.scope_instance_get(scope, member_id),
        })
    }

    // give new value to member
    pub fn instance_set(&mut self, member_id: MemberID, new_value: Value) {
        if member_id == MemberID::default() {
            panic!("provided bogus MemberID 0");
        }

        let scope_id = *(self
            .member_map
            .get(&member_id)
            .expect("member not allocated"));

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
        if scope.id == 0 {
            panic!("provided bogus RuntimeScopeID 0");
        } else if member_id == MemberID::default() {
            panic!("provided bogus MemberID 0");
        }

        let member = ast.objs.member(member_id);

        let type_id = match member.variant {
            MemberVariant::Instance(t) => t,
            _ => {
                panic!("can only allocate member which is an instance in interpreter");
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

    pub fn ret_location_push(&mut self, scope: RuntimeScopeID) -> RetLocationID {
        let value_id = self.runtime_scope_mut(scope).value_push(Value::default());

        self.ret_locations.push(RuntimeReference {
            scope: scope,
            value_id,
        });

        RetLocationID {
            id: self.ret_locations.len() as u32 - 1,
        }
    }

    pub fn ret_location(&mut self, ret_location: RetLocationID) -> RuntimeReference {
        if ret_location.id == 0 {
            panic!("Bogus RetLocationID 0");
        }

        self.ret_locations[ret_location.id as usize]
    }

    pub fn ret_location_top(&mut self) -> RuntimeReference {
        if self.ret_locations.len() <= 1 {
            panic!("No valid ret locations currently exist")
        }

        let last_ret_locations_idx = self.ret_locations.len() - 1;

        self.ret_locations[last_ret_locations_idx]
    }

    pub fn ret_location_delete(&mut self, ret_location: RetLocationID) {
        if ret_location.id == 0 {
            panic!("Bogus RetLocationID 0");
        }

        let last_ret_locations_id = self.ret_locations.len() as u32 - 1;

        if last_ret_locations_id != ret_location.id {
            panic!(
                "attempted to delete ret location ID {} but last was {}",
                ret_location.id, last_ret_locations_id,
            );
        }

        self.ret_locations.pop();
    }
}
