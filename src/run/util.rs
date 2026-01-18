use crate::{
    parse::{
        AST,
        ast_contents::{PatternID, ScopeID},
    },
    run::ExecutionContext,
};

pub fn alloc_instances_from_pattern(
    ast: &AST,
    ctx: &mut ExecutionContext,
    pattern: PatternID,
    scope: ScopeID,
) {
}
