// interpreter module

mod context_contents;
mod error;

use std::{collections::HashMap, fmt::Display};

use crate::{parse::{AST, ast_contents::{ExprID, TypeID}}, run::context_contents::RuntimeScopeID};

enum ValueVariant {
    Unit,
    Integer(i64),
    Float(f64),
    String(String),
    Class(HashMap<String, Box<Value>>),
}

struct Value {
    pub type_id: TypeID,
    pub variant: ValueVariant,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.variant {
            ValueVariant::Unit => write!(f, "Unit"),
            ValueVariant::Integer(val) => write!(f, "{val}"),
            ValueVariant::Float(val) => write!(f, "{val}"),
            ValueVariant::String(s) => write!(f, "{s}"),
            ValueVariant::Class(map) => {
                write!(f, "(");

                let mut first_loop = false;

                for (name, val) in map {
                    if first_loop {
                        first_loop = false;
                    } else {
                        write!(f, ", ");
                    }

                    write!(
                        f,
                        "{}={}",
                        name,
                        val,
                    );
                }

                write!(f, ")")
            }
        }
    }
}

#[derive(Default)]
struct RuntimeScope {
    pub members: HashMap<String, Value>,
    
    pub parent: RuntimeScopeID,
}

struct ExecutionContext {
    pub scopes: Vec<RuntimeScope>,
    pub curr_scope: RuntimeScopeID,
}

fn eval(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
) -> Value {

}

pub fn run(ast: &AST) {
    if let Some(expr) = ast.root_expr {

    }
}