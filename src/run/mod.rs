// interpreter module

mod context_contents;
mod error;
mod eval_operation;
mod util;

use crate::{
    constants::UNIT_TYPE,
    parse::{
        AST, Block, ExprVariant, If, MemberVariant, Type, While, ast_contents::{ExprID, FunctionID, ScopeID, TypeID}
    },
    run::{
        context_contents::{ContextObjects, RuntimeRef, RuntimeScopeID, Value, ValueVariant},
        error::{RuntimeErrorVariant, RuntimeException},
        eval_operation::{binary::apply::eval_operation_apply_function, eval_operation},
    },
    tokens::Tokens,
};

pub struct ExecutionContext {
    pub objs: ContextObjects,
    pub curr_scope: RuntimeScopeID,

    pub return_now: bool,
}

impl ExecutionContext {
    pub fn new() -> Self {
        let mut objs = ContextObjects::default();
        let curr_scope = objs.runtime_scope_new();

        ExecutionContext {
            objs,
            curr_scope,
            return_now: false,
        }
    }

    // create scope that is child of current scope and set it to current
    // ret value: new scope's ID
    pub fn switch_to_child_scope(&mut self) -> RuntimeScopeID {
        self.curr_scope = self.objs.runtime_scope_child(self.curr_scope);

        self.curr_scope
    }

    // destroy contents of current scope and return back to parent scope
    pub fn pop_curr_scope(&mut self) -> RuntimeScopeID {
        let old_scope = self.curr_scope;

        // back to parent

        self.curr_scope = self.objs.runtime_scope(old_scope).parent;

        // attempt to destroy old scope

        self.objs.runtime_scope_delete(old_scope);

        self.curr_scope
    }
}

fn expr_to_unit(ast: &AST, ctx: &mut ExecutionContext, expr: ExprID) -> RuntimeRef {
    let type_id = ast.objs.expr(expr).type_id;

    Value {
        type_id: Some(type_id),
        variant: ValueVariant::Unit,
    }
    .to_runtime_ref(ctx, ctx.curr_scope)
}

// eval while loop
fn eval_while(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    while_struct: While,
) -> Result<RuntimeRef, RuntimeException> {
    // loop here corresponds to while loop in program

    loop {
        // switch to while scope

        ctx.switch_to_child_scope();

        // eval cond + test

        let cond_ref = eval(ast, ctx, while_struct.cond)?;

        match ctx.objs.ref_get(cond_ref).variant {
            ValueVariant::Boolean(cond_value) => {
                if !cond_value {
                    ctx.pop_curr_scope();
                    break;
                }
            }
            _ => {
                return Err(RuntimeException {
                    expr,
                    variant: RuntimeErrorVariant::UnexpectedType,
                });
            }
        }

        // eval body + see if should return

        eval(ast, ctx, while_struct.body)?;

        ctx.pop_curr_scope();

        if ctx.return_now {
            break;
        }
    }

    Ok(expr_to_unit(ast, ctx, expr))
}

fn eval_block(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    block: Block,
) -> Result<RuntimeRef, RuntimeException> {
    ctx.switch_to_child_scope();

    // now contained expr is evaluated inside new scope

    eval(ast, ctx, block.body)?;

    ctx.pop_curr_scope();

    return Ok(expr_to_unit(ast, ctx, expr));
}

// eval if/elif loop
fn eval_if_elif(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
    if_struct: If,
) -> Result<RuntimeRef, RuntimeException> {
    let cond_result = eval(ast, ctx, if_struct.cond)?;

    let cond_bool = match ctx.objs.ref_get(cond_result).variant {
        ValueVariant::Boolean(b) => b,
        _ => {
            panic!("IF CONDITION DID NOT RETURN BOOLEAN");
        }
    };

    // cond evaluates to true => execute body
    // otherwise execute else arm if it exists

    if cond_bool {
        eval(ast, ctx, if_struct.body)?;
    } else if let Some(else_expr) = if_struct.else_expr {
        eval(ast, ctx, else_expr)?;
    }

    return Ok(expr_to_unit(ast, ctx, expr));
}

fn eval(
    ast: &AST,
    ctx: &mut ExecutionContext,
    expr: ExprID,
) -> Result<RuntimeRef, RuntimeException> {
    // 1. collect some preliminary info on expr

    let type_id = ast.objs.expr(expr).type_id;

    let type_id = ast.type_resolve_aliasing(type_id);

    // if expr is a type itself or a module do not need to do any work

    match ast.objs.type_get(type_id) {
        Type::Type(contained_type_id) => {
            return Ok(Value {
                type_id: Some(type_id),
                variant: ValueVariant::Type(*contained_type_id),
            }
            .to_runtime_ref(ctx, ctx.curr_scope));
        }
        Type::Module(scope_id) => {
            return Ok(Value {
                type_id: Some(type_id),
                variant: ValueVariant::Module(*scope_id),
            }
            .to_runtime_ref(ctx, ctx.curr_scope));
        }
        Type::Builtin(builtin) => {
            return Ok(Value {
                type_id: Some(type_id),
                variant: ValueVariant::Builtin(*builtin),
            }
            .to_runtime_ref(ctx, ctx.curr_scope));
        }
        _ => (),
    }

    let unit_type_id = ast.get_builtin_type_id(UNIT_TYPE);

    // 2. depending on expr variant we convert to value variant or maybe
    // immediately find out what the ret value of this function should be

    let variant = match &ast.objs.expr(expr).variant {
        ExprVariant::Unit => ValueVariant::Unit,
        ExprVariant::BooleanLiteral(b) => ValueVariant::Boolean(*b),
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

        // CharacterLiteral will be int value (it is Integer type)
        ExprVariant::CharacterLiteral(c) => ValueVariant::Integer(*c as i64),
        ExprVariant::StringLiteral(s) => ValueVariant::String(s.clone()),
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

        ExprVariant::While(while_struct) => {
            return eval_while(ast, ctx, expr, while_struct.clone());
        }

        ExprVariant::Block(block_struct) => {
            return eval_block(ast, ctx, expr, block_struct.clone());
        }

        ExprVariant::If(if_struct) | ExprVariant::Elif(if_struct) => {
            return eval_if_elif(ast, ctx, expr, if_struct.clone());
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

fn eval_main(
    ast: &AST,
    ctx: &mut ExecutionContext,
    function_id: FunctionID,
) -> Result<RuntimeRef, RuntimeException> {
    let value_id = ctx
        .objs
        .runtime_scope_mut(ctx.curr_scope)
        .value_push(Value {
            type_id: Some(TypeID::unit()),
            variant: ValueVariant::Unit,
        });

    let args = RuntimeRef {
        scope: ctx.curr_scope,
        value_id,
    };

    eval_operation_apply_function(ast, ctx, ExprID::default(), function_id, args)
}

// central public function of this module, used to run interpreter given
// program tokens and AST with semantic information
pub fn run(ast: &AST) {
    let main_string = "main".to_string();

    let scope_id = match ast.objs.scope(ScopeID::global()).members.get(&main_string) {
        Some(member_id) => match ast.objs.member(member_id).variant {
            MemberVariant::Module(scope_id) => Some(scope_id),
            _ => None,
        },
        None => None,
    };

    let scope_id = match scope_id {
        Some(scope_id) => scope_id,
        None => {
            eprintln!("NO MAIN MODULE FOUND");
            return;
        }
    };

    let function_id = match ast.objs.scope(scope_id).members.get(&main_string) {
        Some(member_id) => match ast.objs.member(member_id).variant {
            MemberVariant::Function(function_id) => Some(function_id),
            _ => None,
        },
        None => None,
    };

    let function_id = match function_id {
        Some(function_id) => function_id,
        None => {
            eprintln!("MAIN MODULE FOUND BUT NO MAIN FUNCTION");

            eprintln!("MAIN MODULE INFO: {}", ast.scope_to_string(scope_id));

            return;
        }
    };

    let mut ctx = ExecutionContext::new();

    match eval_main(ast, &mut ctx, function_id) {
        Ok(value) => {
            eprintln!();
            eprintln!("PROG RETURN VALUE BELOW:");
            eprintln!("{}", value.to_string(ast, &ctx));
        }
        Err(e) => {
            eprintln!("RUNTIME EXCEPTION: {}", e.to_string(ast));
        }
    }
}
