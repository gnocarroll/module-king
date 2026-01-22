use crate::{
    parse::{
        AST, MemberVariant,
        ast_contents::{FunctionID, PatternID, ScopeID, TypeID}, builtin::Builtin,
    },
    run::{
        ExecutionContext,
        context_contents::{RuntimeReference, ValueVariant},
    },
};

// for each member that can be located from pattern,
// allocate it in current RuntimeScope
pub fn allocate_instances_from_pattern(
    ast: &AST,
    ctx: &mut ExecutionContext,
    pattern: PatternID,
    scope: ScopeID,
) {
    for (name, _) in pattern.to_pattern_iterator(ast) {
        let name = ctx.tokens.tok_as_str(&name).to_string();

        let scope_struct = ast.objs.scope(scope);

        let member_id = scope_struct
            .members
            .get(&name)
            .expect("member should be guaranteed to exist in scope");

        let member_struct = ast.objs.member(member_id);

        match member_struct.variant {
            MemberVariant::Instance(_) => (),
            _ => continue,
        }

        ctx.objs.instance_alloc(ast, ctx.curr_scope, member_id);
    }
}

// if runtime ref is a function then retrieve FunctionID,
// else return None
pub fn runtime_ref_to_function(
    ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeReference,
) -> Option<FunctionID> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Function(function_id) => Some(*function_id),
        ValueVariant::Identifier(member_id) => {
            match ast.objs.member(*member_id).variant {
                MemberVariant::Function(function_id) => Some(function_id),
                _ => None
            }
        }

        _ => None,
    }
}

// if runtime ref is a type then retrieve TypeID,
// else return None
pub fn runtime_ref_to_type(
    ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeReference,
) -> Option<TypeID> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Type(type_id) => Some(*type_id),
        ValueVariant::Identifier(member_id) => {
            match ast.objs.member(*member_id).variant {
                MemberVariant::Type(type_id) => Some(type_id),
                _ => None
            }
        }

        _ => None,
    }
}

pub fn runtime_ref_to_builtin(
    _ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeReference,
) -> Option<Builtin> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Builtin(builtin) => Some(*builtin),
        _ => None,
    }
}
