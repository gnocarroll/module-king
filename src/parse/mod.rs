pub mod operator;

use std::collections::HashMap;

use crate::scan::{Token, TokenType};

#[derive(Clone, Copy, PartialEq)]
enum TypeVariant {
    Integer,
    Float,
    Struct, // class keyword can also be used for this
    Enum, // simple C enum
    Variant, // tagged union (can have fields which are just tag)
}

struct TypeLiteral<'a> {
    pub name: &'a str,
    
    pub variant: TypeVariant,

    // body is an Expr
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeVariant {
    Scope,
    Module,
    Type,
}

struct Function<'a> {
    pub name: &'a str,
    pub params: u32,
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Member,
}

struct Identifier<'a> {
    pub name: &'a str,

    pub variant: IdentifierVariant,
}

struct Operation {
    pub op: TokenType,
    pub lhs: u32,
    pub rhs: u32,
}

enum ExprVariant<'a> {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(&'a str),

    FunctionLiteral(Function<'a>),

    TypeLiteral(TypeLiteral<'a>),

    Operation(Operation),
}

struct Expr<'a> {
    pub tokens: &'a [Token<'a>],

    pub etype: u32,

    pub variant: ExprVariant<'a>,
}

#[derive(Clone, Copy, PartialEq)]
enum Visibility {
    Private,
    Export,
    Static,
}

#[derive(Clone, Copy, PartialEq)]

enum MemberVariant {
    Module,
    Type,
    Instance,
}

struct Member<'a> {
    pub name: &'a str,
    pub visibility: Visibility,

    pub variant: MemberVariant,
    
    // this refers to:
    // - the scope which this member owns if it is a module or type
    // - the type of this member if this member is an instance
    pub module_or_type: u32,
}

// a scope may refer to a scope for a code block or
// - module
// - type (e.g. Integer)
struct Scope<'a> {
    pub name: &'a str,
    pub variant: ScopeVariant,

    pub parent_scope: u32,

    pub members: HashMap<&'a str, u32>,
}

struct AST<'a> {
    exprs: Vec<Expr<'a>>,
    scopes: Vec<Scope<'a>>,
    members: Vec<Member<'a>>,
}