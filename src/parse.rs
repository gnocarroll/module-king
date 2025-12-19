use std::collections::HashMap;

use crate::scan::Token;

#[derive(Clone, Copy, PartialEq)]
enum Visibility {
    Private,
    Export,
    Static,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeType {
    Scope,
    Module,
    Type,
}

struct Expr<'a> {
    pub tokens: &'a [Token<'a>],

    pub etype: u32,
}

struct Member<'a> {
    pub name: &'a str,
    pub visibility: Visibility,
}


struct Scope<'a> {
    pub name: &'a str,
    pub stype: ScopeType,

    pub parent_scope: u32,

    pub members: HashMap<&'a str, u32>,
}