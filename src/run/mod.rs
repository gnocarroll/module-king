// interpreter module

mod context_contents;
mod error;
mod eval_operation;

use std::{collections::HashMap, fmt::Display};

use crate::{
    parse::{
        AST, ExprVariant, TypeOrModule,
        ast_contents::{ExprID, ScopeID, TypeID},
    },
    run::{
        context_contents::{ContextObjects, RuntimeScopeID},
        error::{RuntimeError, RuntimeErrorVariant}, eval_operation::eval_operation,
    },
};

#[derive(Clone)]
enum ValueVariant {
    Unit,
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(RuntimeIdentifier),
    Class(HashMap<String, Box<Value>>),
    Module(ScopeID),
    Function(ExprID),
}

#[derive(Clone)]
struct Value {
    pub type_id: Option<TypeID>,
    pub variant: ValueVariant,
}

#[derive(Default)]
struct RuntimeScope {
    pub members: HashMap<String, Value>,

    pub parent: RuntimeScopeID,
}

#[derive(Clone)]
struct RuntimeIdentifier {
    pub scope: RuntimeScopeID,
    pub name: String,
}

struct ExecutionContext {
    pub objs: ContextObjects,
    pub curr_scope: RuntimeScopeID,
}

impl ExecutionContext {
    pub fn value_to_string(&self, value: &Value) -> String {
        match &value.variant {
            ValueVariant::Unit => "Unit".to_string(),
            ValueVariant::Integer(val) => val.to_string(),
            ValueVariant::Float(val) => val.to_string(),
            ValueVariant::String(s) => s.clone(),
            ValueVariant::Class(map) => {
                let member_strings: Vec<String> = map.iter().map(|(name, value)| format!(
                    "{}={}",
                    name,
                    self.value_to_string(&**value)
                )).collect();

                format!("({})", member_strings.join(", "))
            }
            ValueVariant::Module(_) => write!(f, "module"),
            ValueVariant::Function(_) => write!(f, "write",),
            ValueVariant::Identifier(ident) =>
        }
    }
}

impl Default for ExecutionContext {
    fn default() -> Self {
        let mut objs = ContextObjects::default();
        let curr_scope = objs.runtime_scope_new();

        ExecutionContext { objs, curr_scope }
    }
}

fn eval(ast: &AST, ctx: &mut ExecutionContext, expr: ExprID) -> Result<Value, RuntimeError> {
    let type_id = match ast.objs.expr(expr).type_or_module {
        TypeOrModule::Type(t) => t,
        TypeOrModule::Module(scope) => {
            return Ok(Value {
                type_id: None,
                variant: ValueVariant::Module(scope),
            });
        }
    };

    let ret = match ast.objs.expr(expr).variant {
        ExprVariant::IntegerLiteral(i) => ValueVariant::Integer(match i.try_into() {
            Ok(i) => i,
            Err(_) => {
                return Err(RuntimeError {
                    expr,
                    variant: RuntimeErrorVariant::IntegerOverflow,
                });
            }
        }),
        ExprVariant::FloatLiteral(f) => ValueVariant::Float(f),
        ExprVariant::Operation(operation) => {
            return eval_operation(ast, ctx, expr, operation);
        },
        _ => {
            return Err(RuntimeError {
                expr,
                variant: RuntimeErrorVariant::NotImplemented,
            });
        }
    };

    Ok(Value { type_id: Some(type_id), variant: ret })
}

pub fn run(ast: &AST) {
    if let Some(expr) = ast.root_expr {
        let mut ctx = ExecutionContext::default();

        let value = match eval(ast, &mut ctx, expr) {
            Ok(value) => value,
            Err(_) => {
                return;
            }
        };

        println!("{}", value,);
    }
}
