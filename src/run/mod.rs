// interpreter module

mod context_contents;
mod error;
mod eval_operation;

use crate::{
    parse::{AST, ExprVariant, TypeOrModule, ast_contents::ExprID},
    run::{
        context_contents::{ContextObjects, RuntimeScopeID, Value, ValueVariant},
        error::{RuntimeError, RuntimeErrorVariant},
        eval_operation::eval_operation,
    },
    tokens::Tokens,
};

pub struct ExecutionContext<'a> {
    pub tokens: &'a Tokens<'a>,
    pub objs: ContextObjects,
    pub curr_scope: RuntimeScopeID,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(tokens: &'a Tokens) -> Self {
        let mut objs = ContextObjects::default();
        let curr_scope = objs.runtime_scope_new();

        ExecutionContext {
            tokens,
            objs,
            curr_scope,
        }
    }
}

fn eval(ast: &AST, ctx: &mut ExecutionContext, expr: ExprID) -> Result<Value, RuntimeError> {
    // 1. get language type of expr

    let type_id = match ast.objs.expr(expr).type_or_module {
        TypeOrModule::Type(t) => t,
        TypeOrModule::Module(scope) => {
            return Ok(Value {
                type_id: None,
                variant: ValueVariant::Module(scope),
            });
        }
    };

    // 2. depending on expr variant we convert to value variant or maybe
    // immediately find out what the ret value of this function should be

    let variant = match ast.objs.expr(expr).variant {
        ExprVariant::Unit => ValueVariant::Unit,
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
        }
        ExprVariant::Identifier(ident) => ValueVariant::Identifier(ident.member_id),
        _ => {
            return Err(RuntimeError {
                expr,
                variant: RuntimeErrorVariant::NotImplemented,
            });
        }
    };

    Ok(Value {
        type_id: Some(type_id),
        variant,
    })
}

// central public function of this module, used to run interpreter given
// program tokens and AST with semantic information
pub fn run(tokens: &Tokens, ast: &AST) {
    if let Some(expr) = ast.root_expr {
        let mut ctx = ExecutionContext::new(tokens);

        if let Ok(value) = eval(ast, &mut ctx, expr) {
            eprintln!();
            eprintln!("PROG RETURN VALUE BELOW:");
            eprintln!("{}", value.to_string(ast, &ctx));
        }
    }
}
