use crate::{
    parse::{
        AST, MemberVariant,
        ast_contents::{PatternID, ScopeID},
    },
    run::ExecutionContext,
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
