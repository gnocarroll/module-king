use crate::run::RuntimeScope;

#[derive(Clone, Copy, Default)]
pub struct RuntimeScopeID {
    id: u32,
}

pub struct ContextObjects {
    scopes: Vec<RuntimeScope>
}

impl ContextObjects {
    fn runtime_scope_push(&mut self, scope: RuntimeScope) -> RuntimeScopeID {
        self.scopes.push(scope);

        RuntimeScopeID {
            id: self.scopes.len() as u32 - 1
        }
    }

    fn runtime_scope_new(&mut self) -> RuntimeScopeID {
        self.runtime_scope_push(RuntimeScope::default())
    }

    fn runtime_scope(&self, scope: RuntimeScopeID) -> &RuntimeScope {
        &self.scopes[scope.id as usize]
    }

    fn runtime_scope_mut(&mut self, scope: RuntimeScopeID) -> &mut RuntimeScope {
        &mut self.scopes[scope.id as usize]
    }
}