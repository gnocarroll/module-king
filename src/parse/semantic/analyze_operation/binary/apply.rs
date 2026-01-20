use crate::{
    constants::ERROR_TYPE,
    parse::{
        AST, ExprVariant, MemberVariant, ScopeVariant, Type, TypeVariant, Visibility,
        ast_contents::{ExprID, FunctionID, ScopeID, TypeID},
        semantic::SemanticContext,
    },
};

// if doing apply operator (e.g. "f()") what are you doing
// (e.g. are you calling a function or casting to other type)
#[derive(Clone, Copy, PartialEq)]
enum ApplyCase {
    Cast(TypeID),
    Function(FunctionID),
}

#[derive(Clone, Copy, PartialEq)]
enum BuiltinGeneric {
    List,
    Map,
}

impl AST {
    // apply a function e.g. f()
    // TokenType is actually LParen
    pub fn analyze_operation_apply(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        // test if user is attempting to create built-in List or Map type

        if let ExprVariant::Identifier(ident) = self.objs.expr(operand1).variant {
            let ident_str = ctx.tokens.tok_as_str(&ident.name);

            // if matches List/Map go to handler for that and return early from here

            if ident_str == "List" || ident_str == "Map" {
                self.analyze_builtin_generic(
                    ctx,
                    scope,
                    expr,
                    operand2,
                    if ident_str == "List" {
                        BuiltinGeneric::List
                    } else {
                        BuiltinGeneric::Map
                    },
                );
                return;
            }
        }

        self.analyze_expr(ctx, scope, operand1);

        let old_analyzing_now = ctx.analyzing_now;

        // ctx.analyzing_now = AnalyzingNow::FuncArgs;
        self.analyze_expr(ctx, scope, operand2);
        ctx.analyzing_now = old_analyzing_now;

        eprintln!(
            "{}",
            self.type_to_string(ctx.tokens, self.objs.expr(operand2).type_id)
        );

        let mut finalized = true;

        let operand1_struct = self.expr(operand1);

        let mut apply_case = ApplyCase::Cast(TypeID::error());

        match (
            operand1_struct.finalized,
            self.objs.type_get(operand1_struct.type_id),
        ) {
            (false, _) => (),

            // type cast
            (true, Type::Type(t)) => {
                apply_case = ApplyCase::Cast(*t);
            }

            // function call, check if type is function
            (true, Type::Function(_)) => {
                let function_id = self
                    .expr_get_function_id(operand1)
                    .expect("expr should be guaranteed to be function");

                apply_case = ApplyCase::Function(function_id);
            }
            _ => {
                self.invalid_operation(expr, "should be type cast or function call");
                finalized = false;
            }
        }

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            finalized = false;
        }

        if finalized {
            match apply_case {
                ApplyCase::Cast(cast_to) => {
                    self.analyze_cast(ctx, scope, expr, cast_to, operand2);
                }
                ApplyCase::Function(function) => {
                    self.analyze_function_call(ctx, scope, expr, function, operand2);
                }
            }
        }
    }

    fn analyze_builtin_generic(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        arg: ExprID,
        which_one: BuiltinGeneric,
    ) {
        self.analyze_expr(ctx, scope, arg);

        let mut type_id = TypeID::error();
        let mut finalized = false;

        let arg_struct = self.objs.expr(arg);

        match (arg_struct.finalized, self.objs.type_get(arg_struct.type_id)) {
            (false, _) => (),
            (true, Type::Type(contained_type_id)) => {
                let generic_type = self.objs.type_push(match which_one {
                    BuiltinGeneric::List => Type::List(*contained_type_id),
                    BuiltinGeneric::Map => Type::Map(*contained_type_id),
                });

                // type itself is what is being returned

                type_id = self.objs.type_push(Type::Type(generic_type));
                
                finalized = true;
            }
            (true, _) => {
                self.invalid_operation(expr, "provide type as argument to List/Map");
            }
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = type_id;
        expr_mut.finalized = finalized;
    }

    // ret value indicates success
    pub fn analyze_cast(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        cast_to: TypeID,
        args: ExprID, // args
    ) {
        let cast_to = self.type_resolve_aliasing(cast_to);

        let arg_type = self.type_resolve_aliasing(self.objs.expr(args).type_id);

        if cast_to == TypeID::error() || arg_type == TypeID::error() {
            return;
        }

        // simple cast means args type must match type being casted to

        let mut simple_cast = true;

        match self.objs.type_get(cast_to) {
            Type::Type(_) | Type::Unit => {
                self.invalid_operation(expr, "cannot cast to this type");
                return;
            }
            Type::Scope(cast_type_scope_id) => {
                match self.objs.scope(*cast_type_scope_id).variant {
                    ScopeVariant::Type(TypeVariant::Integer) => {
                        simple_cast = false;

                        // can be some other integer type being casted

                        match self.type_get_variant(arg_type) {
                            Some(TypeVariant::Integer) | Some(TypeVariant::Float) => (),
                            _ => {
                                self.invalid_operation(expr, "when casting to integer type argument must be integer or float type");
                                return;
                            }
                        }
                    }
                    ScopeVariant::Type(TypeVariant::Float) => {
                        simple_cast = false;

                        // can be some other float type being casted

                        match self.type_get_variant(arg_type) {
                            Some(TypeVariant::Integer) | Some(TypeVariant::Float) => (),
                            _ => {
                                self.invalid_operation(expr, "when casting to float type argument must be integer or float type");
                                return;
                            }
                        }
                    }
                    ScopeVariant::Type(TypeVariant::Record) => {
                        simple_cast = false;

                        let scope_ref = self.objs.scope(*cast_type_scope_id);

                        let mut member_type_ids = Vec::<TypeID>::new();

                        for member_idx in 0..scope_ref.members.member_count() {
                            let member_id = scope_ref.members.nth_member(member_idx);
                            let member = self.objs.member(member_id);

                            let member_type_id = match (member.visibility, member.variant) {
                                (Visibility::Global, _) => continue,
                                (_, MemberVariant::Instance(t)) => t,
                                _ => continue,
                            };

                            member_type_ids.push(member_type_id);
                        }

                        let member_types_tuple = self.type_vec_to_tuple(&member_type_ids);

                        if !self.type_eq(arg_type, member_types_tuple) {
                            self.invalid_operation(expr, "provide args in order members are declared in record and with matching types");
                            return;
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }

        if simple_cast && cast_to != arg_type {
            self.invalid_operation(expr, "type of args does not match type you are casting to");
            return;
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = cast_to;
        expr_mut.finalized = true;
    }

    // ret value indicates success
    pub fn analyze_function_call(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        function: FunctionID,
        args: ExprID, // args
    ) {
        let function = self.objs.function(function);

        let expr_type_id = function.return_type;

        let found_arg_type = self.type_resolve_aliasing(self.objs.expr(args).type_id);

        let actual_arg_type = match self.objs.type_get(function.func_type) {
            Type::Function((actual_arg_type, _)) => *actual_arg_type,
            _ => return,
        };

        if !self.type_eq(actual_arg_type, found_arg_type) {
            self.invalid_operation(expr, "did not provide correct number or type of args");
            return;
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = expr_type_id;
        expr_mut.finalized = true;
    }
}
