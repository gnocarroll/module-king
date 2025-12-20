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
    // may be empty str if type literal does not provide name
    pub name: Option<&'a str>,
    
    pub variant: TypeVariant,

    // body is an Expr
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeVariant {
    Scope, // e.g. scope for a for loop or other block
    Module,
    Type,
}

struct FunctionLiteral<'a> {
    pub name: Option<&'a str>,
    pub params: u32,
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Member, // e.g. for point.x would be "x"
}

struct Identifier<'a> {
    pub name: &'a str,

    pub variant: IdentifierVariant,
}

struct Operation {
    pub op: TokenType,
    pub operand1: Option<u32>,
    pub operand2: Option<u32>,
}

enum ExprVariant<'a> {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(&'a str),

    Identifier(Identifier<'a>),

    Operation(Operation),

    FunctionLiteral(FunctionLiteral<'a>),

    TypeLiteral(TypeLiteral<'a>),
}

struct Expr<'a> {
    pub tokens: &'a [Token<'a>],

    // ID of language type
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
    pub name: Option<&'a str>,
    pub variant: ScopeVariant,

    pub parent_scope: u32,

    pub members: HashMap<&'a str, u32>,
}

struct AST<'a> {
    exprs: Vec<Expr<'a>>,
    scopes: Vec<Scope<'a>>,
    members: Vec<Member<'a>>,
}