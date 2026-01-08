use crate::parse::{Expr, Member, Pattern, Scope, Type};

#[derive(Clone, Default)]
pub struct ASTContents {
    // exprs used in initial parsing and semantic analysis stage
    pub exprs: Vec<Expr>,

    // exprs are main thing for parsing so e.g. scopes members are for
    // semantic analysis
    pub scopes: Vec<Scope>,
    pub types: Vec<Type>,
    pub members: Vec<Member>,

    pub patterns: Vec<Pattern>,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct ExprID {
    id: u32,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct ScopeID {
    id: u32,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct TypeID {
    id: u32,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct MemberID {
    id: u32,
}

#[derive(Clone, Copy, Default, PartialEq)]
pub struct PatternID {
    id: u32,
}

impl ASTContents {
    // Expr

    pub fn expr_push(&mut self, expr: Expr) -> ExprID {
        self.exprs.push(expr);

        ExprID { id: self.exprs.len() as u32 - 1 }
    }

    pub fn expr(&self, expr: ExprID) -> &Expr {
        &self.exprs[expr.id as usize]
    }

    pub fn expr_mut(&mut self, expr: ExprID) -> &mut Expr {
        &mut self.exprs[expr.id as usize]
    }

    // Scope

    pub fn scope_push(&mut self, scope: Scope) -> ScopeID {
        self.scopes.push(scope);

        ScopeID { id: self.scopes.len() as u32 - 1 }
    }

    pub fn scope(&self, scope: ScopeID) -> &Scope {
        &self.scopes[scope.id as usize]
    }

    pub fn scope_mut(&mut self, scope: ScopeID) -> &mut Scope {
        &mut self.scopes[scope.id as usize]
    }

    // Type

    pub fn type_push(&mut self, type_struct: Type) -> TypeID {
        self.types.push(type_struct);

        TypeID { id: self.types.len() as u32 - 1 }
    }

    pub fn type_get(&self, type_id: TypeID) -> &Type {
        &self.types[type_id.id as usize]
    }

    pub fn type_mut(&mut self, type_id: TypeID) -> &mut Type {
        &mut self.types[type_id.id as usize]
    }

    // Member

    pub fn member_push(&mut self, member: Member) -> MemberID {
        self.members.push(member);

        MemberID { id: self.members.len() as u32 - 1 }
    }

    pub fn member(&self, member: MemberID) -> &Member {
        &self.members[member.id as usize]
    }

    pub fn member_mut(&mut self, member: MemberID) -> &mut Member {
        &mut self.members[member.id as usize]
    }

    // Pattern

    pub fn pattern_push(&mut self, pattern: Pattern) -> PatternID {
        self.patterns.push(pattern);

        PatternID { id: self.patterns.len() as u32 - 1 }
    }

    pub fn pattern(&self, pattern: PatternID) -> &Pattern {
        &self.patterns[pattern.id as usize]
    }

    pub fn pattern_mut(&mut self, pattern: PatternID) -> &mut Pattern {
        &mut self.patterns[pattern.id as usize]
    }
}