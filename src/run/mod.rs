// interpreter module

mod context_contents;
mod error;
mod eval_operation;
mod util;

use crate::{
    constants::UNIT_TYPE,
    parse::{AST, ExprVariant, Type, ast_contents::ExprID},
    run::{
        context_contents::{ContextObjects, RuntimeReference, RuntimeScopeID, Value, ValueVariant},
        error::{RuntimeException, RuntimeErrorVariant},
        eval_operation::eval_operation,
    },
    tokens::Tokens,
};

pub struct ExecutionContext<'a> {
    pub tokens: &'a Tokens<'a>,
    pub objs: ContextObjects,
    pub curr_scope: RuntimeScopeID,

    // when function is returning value will be placed at top of here
    pub ret_locations: Vec<RuntimeReference>,

    pub return_now: bool,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(tokens: &'a Tokens) -> Self {
        let mut objs = ContextObjects::default();
        let curr_scope = objs.runtime_scope_new();

        ExecutionContext {
            tokens,
            objs,
            curr_scope,
            ret_locations: Vec::new(),
            return_now: false,
        }
    }

    // create scope that is child of current scope and set it to current
    // ret value: new scope's ID
    pub fn switch_to_child_scope(&mut self) -> RuntimeScopeID {
        self.curr_scope = self.objs.runtime_scope_child(self.curr_scope);

        self.curr_scope
    }
}

fn expr_to_unit(ast: &AST, ctx: &mut ExecutionContext, expr: ExprID) -> RuntimeReference {
    let type_id = ast.objs.expr(expr).type_id;

    Value {
        type_id: Some(type_id),
        variant: ValueVariant::Unit,
    }
    .to_runtime_ref(ctx, ctx.curr_scope)
}

fn eval(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
) -> Result<RuntimeReference, RuntimeException> {
    // 1. collect some preliminary info on expr

    let type_id = ast.objs.expr(expr).type_id;

    // if expr is a type itself or a module do not need to do any work

    match ast.objs.type_get(type_id) {
        Type::Type(contained_type_id) => return Ok(Value {
            type_id: Some(type_id),
            variant: ValueVariant::Type(*contained_type_id),
        }.to_runtime_ref(ctx, ctx.curr_scope)),
        Type::Module(scope_id) => return Ok(Value {
            type_id: Some(type_id),
            variant: ValueVariant::Module(*scope_id),
        }.to_runtime_ref(ctx, ctx.curr_scope)),
        _ => (),
    }

    let unit_type_id = ast.get_builtin_type_id(UNIT_TYPE);

    // 2. depending on expr variant we convert to value variant or maybe
    // immediately find out what the ret value of this function should be

    let variant = match &ast.objs.expr(expr).variant {
        ExprVariant::Unit => ValueVariant::Unit,
        ExprVariant::IntegerLiteral(i) => ValueVariant::Integer(match (*i).try_into() {
            Ok(i) => i,
            Err(_) => {
                return Err(RuntimeException {
                    expr,
                    variant: RuntimeErrorVariant::IntegerOverflow,
                });
            }
        }),
        ExprVariant::FloatLiteral(f) => ValueVariant::Float(*f),
        ExprVariant::Operation(operation) => {
            return eval_operation(ast, ctx, expr, *operation);
        }
        ExprVariant::Identifier(ident) => ValueVariant::Identifier(ident.member_id),

        // named function literal returns unit but if not named then return expr id
        ExprVariant::FunctionLiteral(func) => {
            if type_id == unit_type_id {
                ValueVariant::Unit
            } else {
                ValueVariant::Function(func.function_id)
            }
        }

        // no handling for given expr variant
        _ => {
            return Err(RuntimeException {
                expr,
                variant: RuntimeErrorVariant::NotImplemented,
            });
        }
    };

    let ret = Value {
        type_id: Some(type_id),
        variant,
    }
    .to_runtime_ref(ctx, ctx.curr_scope);

    Ok(ret)
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
