// ID structs in this module enable use of strongly-typed IDs
// replacing previous use of u32 for various AST objects

use std::collections::HashMap;

use crate::{
    constants::{ERROR_TYPE, STRING_TYPE, UNIT_TYPE},
    parse::{
        AST, Expr, Function, Member, MemberVariant, Pattern, PatternIterator, Scope, ScopeVariant, TupleIterator, Type, Visibility
    },
    tokens::TokenOrString,
};

#[derive(Clone)]
pub struct ASTContents {
    // exprs used in initial parsing and semantic analysis stage
    pub exprs: Vec<Expr>,

    // exprs are main thing for parsing so e.g. scopes members are for
    // semantic analysis
    pub scopes: Vec<Scope>,
    pub types: Vec<Type>,
    pub members: Vec<Member>,

    pub patterns: Vec<Pattern>,

    pub functions: Vec<Function>,
}

impl Default for ASTContents {
    fn default() -> Self {
        // since I have default for the ID types I will create a bogus]
        // zeroth entry in each vector (e.g. Expr ID 0, Scope ID 0, etc. are bogus)
        // so can panic if default ID is used for accessing a vector

        let mut ret = ASTContents {
            exprs: vec![Expr::default()],
            scopes: vec![Scope::default()],
            types: vec![Type::default()],
            members: vec![Member::default()],
            patterns: vec![Pattern::default()],
            functions: vec![Function::default()],
        };

        // ensure global scope has ID 1

        ret.scope_push(Scope {
            name: Some(TokenOrString::String("GLOBAL".to_string())),
            variant: ScopeVariant::Module,
            parent_scope: ScopeID::global(),
            refers_to: None,
            ..Default::default()
        });

        // error, unit will have IDs 1, 2 respectively
        // these types are created here since they cannot be created
        // by the user and it is good for them to have consistent IDs

        let error_type_id = ret.type_push(Type::Error);
        let unit_type_id = ret.type_push(Type::Unit);

        // for now String will also just be built-in but later should go in
        // std library

        let string_type_id = ret.type_push(Type::String);

        for (name, type_id) in [
            (ERROR_TYPE, error_type_id),
            (UNIT_TYPE, unit_type_id),
            (STRING_TYPE, string_type_id),
        ] {
            let member_id = ret.member_push(Member {
                name: TokenOrString::String(name.to_string()),
                visibility: Visibility::Export,
                variant: MemberVariant::Type(type_id),
            });

            ret.scope_mut(ScopeID::global())
                .members
                .insert(name.to_string(), member_id);
        }

        ret
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ExprID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct FunctionID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ScopeID {
    id: u32,
}

impl ScopeID {
    // Default for ASTContents should guarantee these IDs are correct

    pub fn global() -> Self {
        ScopeID { id: 1 }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct TypeID {
    id: u32,
}

impl TypeID {
    pub fn to_tuple_iterator<'a>(&self, ast: &'a AST) -> TupleIterator<'a> {
        TupleIterator {
            ast,
            type_id: *self,
            idx: 0,
            done: false,
        }
    }
}

impl TypeID {
    // Default for ASTContents should guarantee these IDs are correct

    pub fn error() -> Self {
        return TypeID { id: 1 };
    }

    pub fn unit() -> Self {
        return TypeID { id: 2 };
    }

    pub fn string() -> Self {
        return TypeID { id: 3 };
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct MemberID {
    id: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct PatternID {
    id: u32,
}

impl PatternID {
    pub fn to_pattern_iterator<'a>(&self, ast: &'a AST) -> PatternIterator<'a> {
        PatternIterator { ast, pattern_stack: vec![*self] }
    }
}

impl ASTContents {
    // Expr

    pub fn expr_push(&mut self, expr: Expr) -> ExprID {
        self.exprs.push(expr);

        ExprID {
            id: self.exprs.len() as u32 - 1,
        }
    }

    pub fn expr(&self, expr: ExprID) -> &Expr {
        if expr.id == 0 {
            panic!("BAD ID (attempted to access bogus ExprID 0)");
        }

        &self.exprs[expr.id as usize]
    }

    pub fn expr_mut(&mut self, expr: ExprID) -> &mut Expr {
        if expr.id == 0 {
            panic!("BAD ID (attempted to access bogus ExprID 0)");
        }

        &mut self.exprs[expr.id as usize]
    }

    // Scope

    pub fn scope_push(&mut self, scope: Scope) -> ScopeID {
        self.scopes.push(scope);

        ScopeID {
            id: self.scopes.len() as u32 - 1,
        }
    }

    pub fn scope(&self, scope: ScopeID) -> &Scope {
        if scope.id == 0 {
            panic!("BAD ID (attempted to access bogus ScopeID 0)");
        }

        &self.scopes[scope.id as usize]
    }

    pub fn scope_mut(&mut self, scope: ScopeID) -> &mut Scope {
        if scope.id == 0 {
            panic!("BAD ID (attempted to access bogus ScopeID 0)");
        }

        &mut self.scopes[scope.id as usize]
    }

    // Type

    pub fn type_push(&mut self, type_struct: Type) -> TypeID {
        self.types.push(type_struct);

        TypeID {
            id: self.types.len() as u32 - 1,
        }
    }

    pub fn type_get(&self, type_id: TypeID) -> &Type {
        if type_id.id == 0 {
            panic!("BAD ID (attempted to access bogus TypeID 0)");
        }

        &self.types[type_id.id as usize]
    }

    pub fn type_mut(&mut self, type_id: TypeID) -> &mut Type {
        if type_id.id == 0 {
            panic!("BAD ID (attempted to access bogus TypeID 0)");
        }

        &mut self.types[type_id.id as usize]
    }

    // Member

    pub fn member_push(&mut self, member: Member) -> MemberID {
        self.members.push(member);

        MemberID {
            id: self.members.len() as u32 - 1,
        }
    }

    pub fn member(&self, member: MemberID) -> &Member {
        if member.id == 0 {
            panic!("BAD ID (attempted to access bogus MemberID 0)");
        }

        &self.members[member.id as usize]
    }

    pub fn member_mut(&mut self, member: MemberID) -> &mut Member {
        if member.id == 0 {
            panic!("BAD ID (attempted to access bogus MemberID 0)");
        }

        &mut self.members[member.id as usize]
    }

    // Pattern

    pub fn pattern_push(&mut self, pattern: Pattern) -> PatternID {
        self.patterns.push(pattern);

        PatternID {
            id: self.patterns.len() as u32 - 1,
        }
    }

    pub fn pattern(&self, pattern: PatternID) -> &Pattern {
        if pattern.id == 0 {
            panic!("BAD ID (attempted to access bogus PatternID 0)");
        }

        &self.patterns[pattern.id as usize]
    }

    pub fn pattern_mut(&mut self, pattern: PatternID) -> &mut Pattern {
        if pattern.id == 0 {
            panic!("BAD ID (attempted to access bogus PatternID 0)");
        }

        &mut self.patterns[pattern.id as usize]
    }

    // Function

    pub fn function_push(&mut self, function: Function) -> FunctionID {
        self.functions.push(function);

        FunctionID {
            id: self.functions.len() as u32 - 1,
        }
    }

    pub fn function(&self, function: FunctionID) -> &Function {
        if function.id == 0 {
            panic!("BAD ID (attempted to access bogus FunctionID 0)");
        }

        &self.functions[function.id as usize]
    }

    pub fn function_mut(&mut self, function: FunctionID) -> &mut Function {
        if function.id == 0 {
            panic!("BAD ID (attempted to access bogus FunctionID 0)");
        }

        &mut self.functions[function.id as usize]
    }
}
