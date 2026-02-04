use std::collections::HashMap;

use crate::{
    parse::{
        AST, HasFileModule, MemberVariant, ScopeVariant, Type, TypeVariant, Visibility,
        ast_contents::{FunctionID, PatternID, ScopeID, TypeID},
        builtin::Builtin,
    },
    run::{
        ExecutionContext,
        context_contents::{RuntimeRef, RuntimeScope, Value, ValueID, ValueVariant},
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
        let name = scope.get_tokens(ast).tok_as_str(&name).to_string();

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
    runtime_ref: RuntimeRef,
) -> Option<FunctionID> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Function(function_id) => Some(*function_id),
        ValueVariant::Identifier(member_id) => match ast.objs.member(*member_id).variant {
            MemberVariant::Function(function_id) => Some(function_id),
            _ => None,
        },

        _ => None,
    }
}

// if runtime ref is a type then retrieve TypeID,
// else return None
pub fn runtime_ref_to_type(
    ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeRef,
) -> Option<TypeID> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Type(type_id) => Some(*type_id),
        ValueVariant::Identifier(member_id) => match ast.objs.member(*member_id).variant {
            MemberVariant::Type(type_id) => Some(type_id),
            _ => None,
        },

        _ => None,
    }
}

pub fn runtime_ref_to_builtin(
    _ast: &AST,
    ctx: &mut ExecutionContext,
    runtime_ref: RuntimeRef,
) -> Option<Builtin> {
    match &ctx.objs.ref_get(runtime_ref).variant {
        ValueVariant::Builtin(builtin) => Some(*builtin),
        _ => None,
    }
}

fn types_to_tuple_value_id(
    ast: &AST,
    runtime_scope: &mut RuntimeScope,
    type_id: TypeID,
    t1: TypeID,
    maybe_t2: Option<TypeID>,
) -> ValueID {
    let mut value_id_vec = vec![type_to_value_id(ast, runtime_scope, t1)];

    if let Some(t2) = maybe_t2 {
        let mut extended_vec = false;
        let value_id2 = type_to_value_id(ast, runtime_scope, t2);

        match ast.objs.type_get(t2) {
            Type::Unit => (),
            Type::RestOfTuple(_) => match &runtime_scope.value(value_id2).variant {
                ValueVariant::Tuple(value_id_vec2) => {
                    value_id_vec.extend_from_slice(value_id_vec2);

                    extended_vec = true;
                }
                _ => panic!("ValueVariant should be guaranteed to be tuple"),
            },
            _ => {
                value_id_vec.push(value_id2);
            }
        }

        // if we extended vec can wipe other ValueID that had second ValueID vec
        // since it is not needed

        if extended_vec {
            runtime_scope.value_set_unit(value_id2);
        }
    }

    runtime_scope.value_push(Value {
        type_id: Some(type_id),
        variant: ValueVariant::Tuple(value_id_vec),
    })
}

pub fn type_to_value_id(ast: &AST, runtime_scope: &mut RuntimeScope, type_id: TypeID) -> ValueID {
    let variant = match ast.objs.type_get(type_id) {
        Type::Error => panic!("error type in type_to_value"),
        Type::Unit => ValueVariant::Unit,
        Type::String => ValueVariant::String(b"".to_vec()),
        Type::Scope(scope) => match &ast.objs.scope(*scope).variant {
            ScopeVariant::Type(variant) => match variant {
                TypeVariant::Integer => ValueVariant::Integer(0),
                TypeVariant::Float => ValueVariant::Float(0.0),
                TypeVariant::Record => {
                    // filter map function will look at members of Record and for ones that are not global (i.e. shared)
                    // and are instances it will use this function type_to_value to create a corresponding value recursively

                    let values: HashMap<String, ValueID> = ast
                        .objs
                        .scope(*scope)
                        .members
                        .get_map()
                        .iter()
                        .filter_map(|(name, member_id)| {
                            let member = ast.objs.member(*member_id);

                            let type_id = match (member.variant, member.visibility) {
                                (_, Visibility::Global) => return None,
                                (MemberVariant::Instance(t), _) => t,
                                (_, _) => return None,
                            };

                            let value_id = type_to_value_id(ast, runtime_scope, type_id);

                            Some((name.clone(), value_id))
                        })
                        .collect();

                    ValueVariant::Record(values)
                }
                _ => todo!("not implemented"),
            },
            _ => {
                panic!("scope retrieved using type id is not a type?");
            }
        },
        Type::Tuple((t1, maybe_t2)) => {
            return types_to_tuple_value_id(ast, runtime_scope, type_id, *t1, *maybe_t2);
        }
        Type::RestOfTuple((t1, t2)) => {
            return types_to_tuple_value_id(ast, runtime_scope, type_id, *t1, Some(*t2));
        }
        Type::Slice((element_type_id, slice_index)) => {
            let element_type_id = *element_type_id;
            let idx_type_id = slice_index.type_id;

            let mut value_id_vec = Vec::new();

            let arr_size = match (slice_index.size, ast.objs.type_get(idx_type_id)) {
                (Some(size), _) => size,
                (None, Type::Scope(scope_id)) => {
                    let scope_struct = ast.objs.scope(*scope_id);

                    match scope_struct.variant {
                        ScopeVariant::Type(TypeVariant::Enum) => (), // Ok
                        _ => {
                            panic!("if size for arr not provided should be using enum as index");
                        }
                    }

                    scope_struct.members.member_count()
                }
                _ => {
                    panic!("creating slice value should have arr size or type with scope");
                }
            };

            // fill array with default values of element type

            for _ in 0..arr_size {
                value_id_vec.push(type_to_value_id(ast, runtime_scope, element_type_id));
            }

            ValueVariant::Array(value_id_vec)
        }
        Type::Ref(_) => {
            ValueVariant::Ref(RuntimeRef::default())
        }
        t @ _ => {
            eprintln!("{:?}", t);

            todo!("have not implemented in interpreter other sorts of types yet")
        }
    };

    runtime_scope.value_push(Value {
        type_id: Some(type_id),
        variant,
    })
}
