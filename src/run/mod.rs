// interpreter module

use std::collections::HashMap;

use crate::parse::{AST, ast_contents::TypeID};

enum ValueVariant {
    Error,
    Unit,
    Integer(i64),
    Float(f64),
    Class(HashMap<String, Box<Value>>),
}

struct Value {
    pub type_id: TypeID,
    pub variant: ValueVariant,
}

pub fn run(ast: &AST) {
    if let Some(expr) = ast.root_expr {

    }
}