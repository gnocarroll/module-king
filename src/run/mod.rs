// interpreter module

mod context_contents;
mod error;
mod eval_operation;

use std::{collections::HashMap, fmt::Display};

use crate::{
    parse::{
        AST, ExprVariant, TypeOrModule,
        ast_contents::{ExprID, MemberID, ScopeID, TypeID},
    },
    run::{
        context_contents::{ContextObjects, RuntimeScopeID},
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

impl Value {
    pub fn to_string(&self, ast: &AST, ctx: &ExecutionContext) -> String {
        match &self.variant {
            ValueVariant::Unit => "Unit".to_string(),
            ValueVariant::Integer(val) => val.to_string(),
            ValueVariant::Float(val) => val.to_string(),
            ValueVariant::Boolean(val) => val.to_string(),
            ValueVariant::String(s) => s.clone(),
            ValueVariant::Record(map) => {
                let member_strings: Vec<String> = map
                    .iter()
                    .map(|(name, value)| format!("{}={}", name, value.to_string(ast, ctx)))
                    .collect();

                format!("({})", member_strings.join(", "))
            }
            ValueVariant::Module(scope_id) => {
                let scope = ast.objs.scope(*scope_id);

                let name_string = match &scope.name {
                    Some(tok_or_string) => ctx.tokens.tok_or_string_to_string(tok_or_string),
                    None => "(anonymous)".to_string(),
                };

                format!("module {}", name_string)
            }
            ValueVariant::Function(func) => {
                let expr = ast.objs.expr(*func);

                let func_name_string = match &expr.variant {
                    ExprVariant::FunctionLiteral(func) => match &func.name {
                        Some(t) => ctx.tokens.tok_as_str(t),
                        None => "(anonymous)",
                    },
                    _ => "ERR_EXPR_IS_NOT_FUNC",
                };

                format!("function {}", func_name_string)
            }
            ValueVariant::Identifier(member_id) => match ctx.objs.instance_get(*member_id) {
                Some(value) => value.to_string(ast, ctx),
                None => "ERR_IDENT_DNE".to_string(),
            },
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
