use crate::{
    constants::{BOOLEAN_TYPE, INTEGER_TYPE},
    parse::{
        AST, ExprVariant, MemberVariant, ScopeVariant, Type, TypeVariant, Visibility,
        ast_contents::{ExprID, FunctionID, ScopeID, TypeID},
        builtin::Builtin,
        semantic::SemanticContext,
    },
};

// if doing apply operator (e.g. "f()") what are you doing
// (e.g. are you calling a function or casting to other type)
#[derive(Clone, Copy, PartialEq)]
enum ApplyCase {
    Cast(TypeID),
    Function(FunctionID),
    Builtin(Builtin),
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
        self.analyze_expr(ctx, scope, operand1);

        let old_analyzing_now = ctx.analyzing_now;

        // ctx.analyzing_now = AnalyzingNow::FuncArgs;
        self.analyze_expr(ctx, scope, operand2);
        ctx.analyzing_now = old_analyzing_now;

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            return;
        }

        eprintln!(
            "ARGS: {}",
            self.type_to_string(self.objs.expr(operand2).type_id),
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

            // builtin function
            (true, Type::Builtin(builtin)) => {
                apply_case = ApplyCase::Builtin(*builtin);
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
                ApplyCase::Builtin(builtin) => {
                    self.analyze_builtin_function(ctx, scope, expr, builtin, operand2);
                }
            }
        }
    }

    // ret value indicates success
    pub fn analyze_cast(
        &mut self,
        _ctx: &mut SemanticContext,
        _scope: ScopeID,
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
                            Some(TypeVariant::Integer | TypeVariant::Float | TypeVariant::Enum) => {
                                ()
                            }
                            _ => {
                                self.invalid_operation(expr, "when casting to integer type argument must be integer, float, or enum type");
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
            Type::String => {
                // with String there is nothing to check since anything can be casted to a string
                simple_cast = false;
            }
            _ => (),
        }

        if simple_cast && !self.type_eq(cast_to, arg_type) {
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
        _ctx: &mut SemanticContext,
        _scope: ScopeID,
        expr: ExprID,
        function: FunctionID,
        args: ExprID, // args
    ) {
        let function = self.objs.function(function);

        // if function's return type or the type of the func itself is either default (0) or err
        // => problem with func and do not finalized this expr

        if function.return_type == TypeID::default()
            || function.return_type == TypeID::error()
            || function.func_type == TypeID::default()
            || function.func_type == TypeID::error()
        {
            return;
        }

        let expr_type_id = function.return_type;

        let found_arg_type = self.type_resolve_aliasing(self.objs.expr(args).type_id);

        let actual_arg_type = match self.objs.type_get(function.func_type) {
            Type::Function((actual_arg_type, _)) => *actual_arg_type,
            _ => return,
        };

        // if actual arg type or expected arg type are a tuple with a single type in it then pull
        // inner type out
        // this is useful because if args to function are e.g. (Integer,) it will accept just Integer

        let found_arg_type = self.type_unwrap_if_single_tuple(found_arg_type);
        let actual_arg_type = self.type_unwrap_if_single_tuple(actual_arg_type);

        if !self.type_eq(actual_arg_type, found_arg_type) {
            self.invalid_operation(expr, "did not provide correct number or type of args");
            return;
        }

        let expr_mut = self.expr_mut(expr);

        expr_mut.type_id = expr_type_id;
        expr_mut.finalized = true;
    }

    fn analyze_builtin_function(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        builtin: Builtin,
        arg: ExprID,
    ) {
        self.analyze_expr(ctx, scope, arg);

        let arg_struct = self.objs.expr(arg);

        if !arg_struct.finalized {
            return;
        }

        // if arg is a single element tuple pull out inner type
        // also remove any aliasing from type after that

        let arg_type_id = self.type_unwrap_if_single_tuple(arg_struct.type_id);
        let arg_type_id = self.type_resolve_aliasing(arg_type_id);

        // arg is unit i.e. 0 args provided

        let arg_is_unit = matches!(arg_struct.variant, ExprVariant::Unit);

        // perform semantic analysis unique to specific builtin

        match builtin {
            Builtin::Map | Builtin::List => {
                let inner_type_id = match self.objs.type_get(arg_type_id) {
                    Type::Type(type_id) => *type_id,
                    _ => {
                        self.invalid_operation(expr, "arg to Map or List should be a type");
                        return;
                    }
                };

                let generic_type_id = self.objs.type_push(match builtin {
                    Builtin::Map => Type::Map(inner_type_id),
                    Builtin::List => Type::List(inner_type_id),
                    _ => panic!("should be List or Map here"),
                });

                // ret is type itself

                let ret_type_id = self.objs.type_push(Type::Type(generic_type_id));

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = ret_type_id;
                expr_mut.finalized = true;
            }

            Builtin::GenericPush
            | Builtin::GenericGet
            | Builtin::GenericExists
            | Builtin::GenericLen => {
                self.analyze_generic_push_get_exists_len(ctx, scope, expr, builtin, arg);
            }

            Builtin::Malloc => {
                let inner_type_id = match self.objs.type_get(arg_type_id) {
                    Type::Type(type_id) => *type_id,
                    _ => {
                        self.invalid_operation(expr, "provide type argument for memory allocation");
                        return;
                    }
                };

                let type_id = self.type_from_inner(inner_type_id, Type::Ref);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = type_id;
                expr_mut.finalized = true;
            }
            Builtin::Mfree => {
                if !matches!(self.objs.type_get(arg_type_id), Type::Ref(_)) {
                    self.invalid_operation(expr, "provide ref argument for memory freeing");
                    return;
                };

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::unit();
                expr_mut.finalized = true;
            }

            Builtin::GetWD => {
                if !arg_is_unit {
                    self.invalid_operation(expr, "get wd takes no args");
                    return;
                }

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::string();
                expr_mut.finalized = true;
            }
            Builtin::SetWD => {
                if !self.type_eq(arg_type_id, TypeID::string()) {
                    self.invalid_operation(expr, "provide 1 arg which is a String to set wd");
                    return;
                }

                // ret indicates if set wd was successful (true => success)
                let boolean_type_id = self.get_builtin_type_id(BOOLEAN_TYPE);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = boolean_type_id;
                expr_mut.finalized = true;
            }

            Builtin::DirList => {
                if !self.type_eq(arg_type_id, TypeID::string()) {
                    self.invalid_operation(expr, "provide target dir to dir list");
                    return;
                }

                // ret type is List(String)

                let type_id = self.type_from_inner(TypeID::string(), Type::List);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = type_id;
                expr_mut.finalized = true;
            }
            Builtin::FileRead => {
                if !self.type_eq(arg_type_id, TypeID::string()) {
                    self.invalid_operation(expr, "provide filename to read it in");
                    return;
                }

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::string();
                expr_mut.finalized = true;
            }

            Builtin::IsFile | Builtin::IsDir => {
                if !self.type_eq(arg_type_id, TypeID::string()) {
                    self.invalid_operation(
                        expr,
                        "provide 1 arg which is a String to is file / is dir",
                    );
                    return;
                }

                let boolean_type_id = self.get_builtin_type_id(BOOLEAN_TYPE);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = boolean_type_id;
                expr_mut.finalized = true;
            }

            Builtin::Print | Builtin::Println | Builtin::Eprint | Builtin::Eprintln => {
                // args can be anything so just set ret type to Unit

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = TypeID::unit();
                expr_mut.finalized = true;
            }

            Builtin::BuiltinCount => {
                panic!("BuiltinCount enum value should never be passed to this function");
            }
        }
    }

    fn analyze_generic_push_get_exists_len(
        &mut self,
        _ctx: &mut SemanticContext,
        _scope: ScopeID,
        expr: ExprID,
        builtin: Builtin,
        arg: ExprID,
    ) {
        let arg_count_err_msg = "should be two args to push, get, exists or one arg to len";

        // used later since this is inner type of String
        let integer_type_id = self.get_builtin_type_id(INTEGER_TYPE);

        let arg_type_id = self.type_unwrap_if_single_tuple(self.objs.expr(arg).type_id);

        let mut tuple_iter = arg_type_id.to_tuple_iterator(self);

        // attempt to get arg1 type and then analyze to make sure it is Ref to some container type
        // then will be able to find what container type and what is type of container's values

        let arg1_type_id = match tuple_iter.next() {
            Some(type_id) => self.type_resolve_aliasing(type_id),
            None => {
                self.invalid_operation(expr, arg_count_err_msg);
                return;
            }
        };

        // should be Ref to container so get out type it is Ref to if possible

        let arg1_type = self.objs.type_get(arg1_type_id).clone();
        let ref_to_type_id;

        if let Type::Ref(type_id) = arg1_type {
            ref_to_type_id = self.type_resolve_aliasing(type_id);
        } else {
            self.invalid_operation(
                expr,
                "ensure arg1 is Ref to a container (so e.g. not container itself but Ref)",
            );
            return;
        }

        // now will get out type which there is a Ref to
        // and for these functions it should be one of the builtin containers hence var name here

        let arg1_container_type = self.objs.type_get(ref_to_type_id).clone();

        // inner type id is type of values in container e.g. Integer for String
        let inner_type_id;

        match &arg1_container_type {
            Type::List(type_id) | Type::Map(type_id) => {
                inner_type_id = self.type_resolve_aliasing(*type_id);
            } // Ok
            Type::String => {
                // chars in String will just be Integers in this interpreter
                inner_type_id = integer_type_id;
            } // Ok
            _ => {
                self.invalid_operation(
                    expr,
                    "arg1 to push/get/exists/len should be List, Map, String",
                );
                return;
            }
        }

        let arg2_type_id = match tuple_iter.next() {
            Some(type_id) => self.type_resolve_aliasing(type_id),
            None => {
                // if len is being called that is good, it takes one arg
                if builtin == Builtin::GenericLen {
                    // 1 arg that has already been check for correctness
                    // => can set return type (Integer) and finalize expr

                    let expr_mut = self.objs.expr_mut(expr);

                    expr_mut.type_id = integer_type_id;
                    expr_mut.finalized = true;

                    return;
                }

                self.invalid_operation(expr, arg_count_err_msg);
                return;
            }
        };

        // make sure there are only two args

        match tuple_iter.next() {
            Some(_) => {
                self.invalid_operation(expr, arg_count_err_msg);
                return;
            }
            None => (), // Ok
        }

        let arg2_type = self.objs.type_get(arg2_type_id).clone();

        match (builtin, arg1_container_type, arg2_type) {
            // String/List
            (Builtin::GenericPush, Type::String | Type::List(_), _) => {
                if !self.type_eq(inner_type_id, arg2_type_id) {
                    self.invalid_operation(
                        expr,
                        "arg2 should match inner type of List/String (in case of String always Integer)",
                    );
                    return;
                }
            }
            (Builtin::GenericGet | Builtin::GenericExists, Type::String | Type::List(_), _) => {
                // provide integer type to index List

                if !matches!(
                    self.type_get_variant(arg2_type_id),
                    Some(TypeVariant::Integer)
                ) {
                    self.invalid_operation(expr, "use integer type to index String or List");
                    return;
                }
            }

            // Map
            (Builtin::GenericPush, Type::Map(_), Type::Tuple((tuple_t1, Some(tuple_t2)))) => {
                if !self.type_eq(tuple_t1, TypeID::string()) {
                    self.invalid_operation(expr, "first in tuple pushed to Map should be String");
                    return;
                }
                if !self.type_eq(tuple_t2, inner_type_id) {
                    self.invalid_operation(
                        expr,
                        "second in tuple pushed to Map should be inner type of Map",
                    );
                    return;
                }
            }
            (Builtin::GenericGet | Builtin::GenericExists, Type::Map(_), _) => {
                if !self.type_eq(arg2_type_id, TypeID::string()) {
                    self.invalid_operation(expr, "use String to access Map");
                    return;
                }
            }

            // Push arg is not tuple with two elements => wrong
            (Builtin::GenericPush, Type::Map(_), _) => {
                self.invalid_operation(
                    expr,
                    "provide tuple with two elements (String, InnerType) to push to map",
                );
                return;
            }

            _ => {
                self.invalid_operation(expr, "wrong arg types to Generic function");
                return;
            }
        }

        let ret_type_id = match builtin {
            Builtin::GenericPush => TypeID::unit(),
            Builtin::GenericGet => self.type_from_inner(inner_type_id, Type::Ref),
            Builtin::GenericExists => self.get_builtin_type_id(BOOLEAN_TYPE),
            _ => panic!("should always be Push, Get, or Exists"),
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = ret_type_id;
        expr_mut.finalized = true;
    }
}
