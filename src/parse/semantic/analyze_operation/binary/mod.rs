mod apply;

use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprVariant, IdentifierVariant, MemberVariant, Operation, ScopeVariant, Type,
        TypeVariant, Visibility,
        ast_contents::{ExprID, ScopeID, TypeID},
        operator,
        semantic::{AnalyzingNow, SemanticContext},
    },
    scan::TokenType,
    tokens::TokenOrString,
};

use operator::OperatorVariant::*;

impl AST {
    // first return value is supported type variants for args
    // second return value is if operation returns bool (instead of inputted type)
    // for << and >> rhs may be of different integer type but this is handled
    // elsewhere
    fn get_supported_type_variants_binary(op: TokenType) -> Option<(&'static [TypeVariant], bool)> {
        match op {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::FSlash
            | TokenType::Percent
            | TokenType::StarStar => Some((&[TypeVariant::Integer, TypeVariant::Float], false)),

            // bit ops
            TokenType::Pipe
            | TokenType::Carrot
            | TokenType::Ampersand
            | TokenType::LtLt
            | TokenType::GtGt => Some((&[TypeVariant::Integer], false)),

            // comparison
            TokenType::Lt | TokenType::Gt | TokenType::Le | TokenType::Ge => {
                Some((&[TypeVariant::Integer, TypeVariant::Float], true))
            }

            // equality ops also support Booleans
            // comparison
            TokenType::EqEq | TokenType::BangEq => Some((
                &[
                    TypeVariant::Integer,
                    TypeVariant::Float,
                    TypeVariant::Boolean,
                ],
                true,
            )),

            // and/or
            TokenType::And | TokenType::Or => Some((&[TypeVariant::Boolean], true)),

            _ => None,
        }
    }

    fn analyze_operation_eq(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand1);
        self.analyze_expr(ctx, scope, operand2);

        let err_type = self.get_builtin_type_id(ERROR_TYPE);

        let operand1_struct = self.objs.expr(operand1).clone();

        let lhs_type = if !operand1_struct.finalized {
            err_type
        } else if operand1_struct.is_var {
            match operand1_struct.variant {
                ExprVariant::Operation(Operation {
                    op: TokenType::Colon,
                    operand1: Some(_),
                    operand2: Some(type_expr),
                }) => {
                    let type_id = self.objs.expr(type_expr).type_id;

                    match self.objs.type_get(type_id) {
                        Type::Type(t) => *t,
                        _ => err_type,
                    }
                }
                _ => match self.objs.type_get(operand1_struct.type_id) {
                    Type::Type(_) => {
                        self.invalid_operation(
                            expr,
                            "use \"is\" to assign to/create types (type on LHS)",
                        );
                        err_type
                    }
                    Type::Module(_) => {
                        self.invalid_operation(
                            expr,
                            "use \"is\" to assign to/create modules (module on LHS)",
                        );
                        err_type
                    }
                    _ => operand1_struct.type_id,
                },
            }
        } else {
            self.invalid_operation(expr, "lhs of assignment should be an assignable variable");
            err_type
        };

        let operand2_struct = self.objs.expr(operand2).clone();

        let rhs_type = if !operand2_struct.finalized {
            err_type
        } else {
            match self.objs.type_get(operand2_struct.type_id) {
                Type::Type(_) => {
                    self.invalid_operation(
                        expr,
                        "use \"is\" to assign to/create types (type on RHS)",
                    );
                    err_type
                }
                Type::Module(_) => {
                    self.invalid_operation(
                        expr,
                        "use \"is\" to assign to/create modules (module on RHS)",
                    );
                    err_type
                }
                _ => operand2_struct.type_id,
            }
        };

        let mut finalized = false;

        if lhs_type != err_type && rhs_type != err_type && !self.type_eq(lhs_type, rhs_type) {
            self.invalid_operation(
                expr,
                "lhs and rhs type are not the same for this assignment",
            );
        } else {
            finalized = true;
        }

        let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = unit_type_id;
        expr_mut.finalized = finalized;
    }

    fn analyze_operation_is(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        let old_analyzing_now = ctx.analyzing_now;

        ctx.analyzing_now = AnalyzingNow::Type;
        self.analyze_expr(ctx, scope, operand2);
        ctx.analyzing_now = old_analyzing_now;

        let name = match self.expr(operand1).variant {
            ExprVariant::Operation(Operation {
                op: TokenType::Type,
                operand1: Some(type_name),
                operand2: None,
            }) => {
                match self.expr(type_name).variant {
                    ExprVariant::Identifier(ident) => Some(ident.name),
                    _ => {
                        self.invalid_operation(expr, "on left-hand side of \"is\" operator expected to find name of new type");

                        None
                    }
                }
            }
            _ => {
                self.invalid_operation(
                    expr,
                    "on left-hand side of \"is\" operator expected to find new type being declared",
                );

                None
            }
        };

        let rhs = self.objs.expr(operand2);

        // if type creation can be completed then do it here with helper function
        // else record error

        let finalized;

        if let (true, Type::Type(type_id), Some(name)) =
            (rhs.finalized, self.objs.type_get(rhs.type_id), name)
        {
            finalized = self
                .scope_add_member_type_from_name_and_id(
                    ctx,
                    scope,
                    TokenOrString::Token(name),
                    *type_id,
                )
                .is_ok();
        } else {
            self.invalid_operation(expr, "creation of new type could not be completed");
            finalized = false;
        }

        let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = unit_type_id;
        expr_mut.finalized = finalized;
    }

    // member access e.g. point.x
    fn analyze_operation_period(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        // analyze what is being access
        self.analyze_expr(ctx, scope, operand1);

        let operand1_struct = self.objs.expr(operand1).clone();

        if !operand1_struct.finalized {
            return;
        }

        // TODO: search global/shared members of types
        // right now only doing modules and instances

        let search_scope = match self.objs.type_get(operand1_struct.type_id) {
            Type::Module(scope) => *scope,
            _ => {
                let mut type_id = operand1_struct.type_id;

                let scope = loop {
                    match self.objs.type_get(type_id) {
                        Type::Scope(scope) => break *scope,
                        Type::Alias(t) | Type::Ptr(t) | Type::Ref(t) => type_id = *t,
                        _ => {
                            self.invalid_operation(expr, "cannot access members of this type");
                            return;
                        }
                    }
                };

                scope
            }
        };

        let name = match &mut self.objs.expr_mut(operand2).variant {
            ExprVariant::Identifier(ident) => {
                let ident = ident.clone();

                self.tokens().tok_as_str(&ident.name).to_string()
            },
            _ => {
                self.invalid_operation(expr, "rhs of access should be an identifier");
                return;
            }
        };

        let member_id = if let Some(member_id) = self.objs.scope(search_scope).members.get(&name) {
            member_id
        } else {
            self.invalid_operation(expr, "member name on rhs not recognized");
            return;
        };

        // set MemberID and variant of rhs of period

        match &mut self.objs.expr_mut(operand2).variant {
            ExprVariant::Identifier(ident) => {
                ident.member_id = member_id;
                ident.variant = IdentifierVariant::Member;
            }
            _ => panic!("should have already checked that it was ident on rhs"),
        }

        match (
            self.objs.type_get(operand1_struct.type_id),
            self.objs.member(member_id).visibility,
        ) {
            // should only be accessing global fields through type
            (Type::Type(_), Visibility::Export | Visibility::Private) => {
                self.invalid_operation(expr, "only global fields may be accessed through type, not ones specific to an instance");
                return;
            }
            _ => (),
        }

        let expr_type_id = match self.objs.member(member_id).variant {
            MemberVariant::Instance(type_id) => type_id,
            MemberVariant::Type(type_id) => self.objs.type_push(Type::Type(type_id)),
            MemberVariant::Module(scope_id) => self.objs.type_push(Type::Module(scope_id)),

            // TODO: if function is being accessed through instance should return function with first param
            // bound to certain instance
            MemberVariant::Function(function_id) => self.objs.function(function_id).func_type,
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = expr_type_id;

        expr_mut.is_var = true;
        expr_mut.finalized = true;
    }

    // var creation + assignment + type inference
    // operand1 := operand2;
    fn analyze_operation_colon_eq(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand2);

        let maybe_type_id = if self.objs.expr(operand2).finalized {
            let type_id = self.objs.expr(operand2).type_id;

            match self.objs.type_get(type_id) {
                Type::Type(_) | Type::Module(_) => {
                    self.invalid_operation(
                        expr,
                        "\":=\" is only for assigning values, not modules or types",
                    );
                    None
                }
                _ => Some(type_id),
            }
        } else {
            None
        };

        let (pattern, _) = self.pattern_matching(ctx, scope, operand1, maybe_type_id);

        let maybe_err = self.scope_create_members_from_pattern(ctx, scope, pattern);

        // after creation of members analyze lhs (now relevant vars should exist)
        self.analyze_expr(ctx, scope, operand1);

        let finalized = maybe_err.is_ok()
            && self.objs.expr(operand1).finalized
            && self.objs.expr(operand2).finalized;

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = TypeID::unit();
        expr_mut.finalized = finalized;
    }

    // binary import e.g. "from some_module import ..."
    fn analyze_operation_binary_import(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand1);

        let operand1_struct = self.objs.expr(operand1);

        if !operand1_struct.finalized {
            return;
        }

        let import_from_scope_id = match self.objs.type_get(operand1_struct.type_id) {
            Type::ImportTarget(scope_id) => *scope_id,
            _ => {
                self.invalid_operation(
                    expr,
                    "target of import should be valid import target \
                    created with \"from\" e.g. \"from some_module\"",
                );
                return;
            }
        };

        // use scope that is being imported from

        self.analyze_expr(ctx, import_from_scope_id, operand2);

        if !self.objs.expr(operand2).finalized {
            return;
        }

        // search through operand2 for identifiers so we can add them to current scope
        // acceptable to parenthesize imported identifiers and list multiple with comma,
        // other operations not allowed

        let mut finalized = true;
        let mut expr_stack = vec![operand2];

        loop {
            let expr_id = match expr_stack.pop() {
                Some(expr_id) => expr_id,
                None => break,
            };

            match self.objs.expr(expr_id).variant {
                ExprVariant::Operation(Operation {
                    op: TokenType::LParen,
                    operand1: Some(operand1),
                    operand2: None,
                }) => {
                    expr_stack.push(operand1);
                }
                ExprVariant::Operation(Operation {
                    op: TokenType::Comma,
                    operand1: Some(operand1),
                    operand2: Some(operand2),
                }) => {
                    match self.objs.expr(operand2).variant {
                        ExprVariant::Unit => (),
                        _ => {
                            expr_stack.push(operand2);
                        }
                    }

                    expr_stack.push(operand1);
                }
                ExprVariant::Identifier(ident) => {
                    // add mapping from member name -> MemberID to scope

                    if self
                        .scope_try_insert(scope, ident.member_id)
                        .is_err()
                    {
                        // duplicate ident => problem
                        finalized = false;
                    }
                }
                _ => {
                    self.invalid_operation(
                        expr,
                        "provide names separated by commas as things to import",
                    );
                    finalized = false;
                }
            }
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = TypeID::unit();
        expr_mut.finalized = finalized;
    }

    // glue values or type together into tuple
    fn analyze_operation_comma(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand1);
        self.analyze_expr(ctx, scope, operand2);

        let finalized = self.expr(operand1).finalized && self.expr(operand2).finalized;

        let operand1_is_type = matches!(
            self.objs.type_get(self.objs.expr(operand1).type_id),
            Type::Type(_),
        );

        let operand2_struct = self.expr(operand2);

        let operand2_type_id = operand2_struct.type_id;

        let operand2_is_unit = operand2_type_id == self.get_builtin_type_id(UNIT_TYPE);

        if operand2_is_unit && !matches!(operand2_struct.variant, ExprVariant::Unit) {
            self.invalid_operation(
                expr,
                "expr returning unit not allowed on RHS of comma unless it is empty expr",
            );
        }

        let operand2_struct = self.expr(operand2);

        // suppose it is known that operand2 is the "rest of" this tuple rather than a standalone tuple
        // (indicated by fact that it is also using comma operator)
        // then type should be transformed in the following way
        // if no RHS -> type becomes just LHS type
        // if both LHS, RHS -> type becomes RestOfTuple(LHS, RHS) instead of Tuple(LHS, RHS)

        let tuple_type_to_singular_or_rest_of = |ast: &mut AST, type_id: TypeID| -> TypeID {
            match ast.objs.type_get(type_id) {
                Type::Tuple((t, None)) => *t,
                Type::Tuple((t1, Some(t2))) => ast.objs.type_push(Type::RestOfTuple((*t1, *t2))),
                _ => panic!("type should be guaranteed to be Tuple"),
            }
        };

        match operand2_struct.variant {
            ExprVariant::Operation(Operation {
                op: TokenType::Comma,
                ..
            }) => {
                match self.objs.type_get(operand2_type_id) {
                    // if operand2 is returning some Tuple type (rather than tuple expr)
                    // then apply transformation to "inner type" i.e. returned type
                    Type::Type(t) => {
                        let new_inner_type_id = tuple_type_to_singular_or_rest_of(self, *t);

                        let new_type_id = if operand1_is_type
                            || !matches!(
                                self.objs.type_get(new_inner_type_id),
                                Type::RestOfTuple(_)
                            ) {
                            self.objs.type_push(Type::Type(new_inner_type_id))
                        } else {
                            // operand1 is a value AND operand2 is Type(RestOfTuple(_))
                            // => make operand2 into just RestOfTuple(_) since full tuple w/ operand1 is not a type

                            new_inner_type_id
                        };

                        self.objs.expr_mut(operand2).type_id = new_type_id;
                    }

                    // otherwise if it is tuple expr just transform type id itself
                    _ => {
                        let new_type_id = tuple_type_to_singular_or_rest_of(self, operand2_type_id);

                        self.objs.expr_mut(operand2).type_id = new_type_id;
                    }
                }
            }
            _ => (),
        }

        let (operand1_type_id, operand2_type_id) =
            (self.expr(operand1).type_id, self.expr(operand2).type_id);

        // determine type of this expr from operand types

        let type_id = match (
            self.objs.type_get(operand1_type_id),
            self.objs.type_get(operand2_type_id),
        ) {
            (Type::Type(operand1_inner_type_id), Type::Type(operand2_inner_type_id)) => {
                let inner_type_id = self.objs.type_push(Type::Tuple((
                    *operand1_inner_type_id,
                    Some(*operand2_inner_type_id),
                )));

                // LHS, RHS are both return types (not values) => this expr also type (tuple type)

                self.objs.type_push(Type::Type(inner_type_id))
            }

            // Single element tuple type
            (Type::Type(operand1_inner_type_id), Type::Unit) => {
                let inner_type_id = self
                    .objs
                    .type_push(Type::Tuple((*operand1_inner_type_id, None)));

                // LHS, RHS are both return types (not values) => this expr also type (tuple type)

                self.objs.type_push(Type::Type(inner_type_id))
            }

            // one of LHS, RHS is value => return tuple value rather than tuple type
            (_, _) => self.objs.type_push(Type::Tuple((
                operand1_type_id,
                if !operand2_is_unit {
                    Some(operand2_type_id)
                } else {
                    None
                },
            ))),
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = type_id;
        expr_mut.finalized = finalized;
    }

    pub fn analyze_operation_binary(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        op: TokenType,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        if operator::get_bp(op, Infix).is_none() && operator::get_bp(op, PostfixAround).is_none() {
            self.invalid_operation(expr, "not a supported binary operator");
            return;
        }

        // certain operations will be handled up here e.g. colon for var creation
        // this is due to potential need for different sort of analysis

        match op {
            TokenType::Colon => {
                // var creation
                // helper function will look at pairing of pattern, type
                // e.g. (x, y) : (Float, Float)
                let (pattern, err) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    Some(operand1),
                    Some(operand2),
                );

                let no_err = self
                    .scope_create_members_from_pattern(ctx, scope, pattern)
                    .is_ok()
                    && err.is_none();

                // after creation of members analyze lhs (now relevant vars should exist)
                self.analyze_expr(ctx, scope, operand1);

                let finalized = no_err
                    && self.objs.expr(operand1).finalized
                    && self.objs.expr(operand2).finalized;

                let unit_type_id = self.get_builtin_type_id(UNIT_TYPE);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = unit_type_id;
                expr_mut.is_var = true;

                expr_mut.finalized = finalized;

                return;
            }
            TokenType::Import => {
                self.analyze_operation_binary_import(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::ColonEq => {
                self.analyze_operation_colon_eq(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::Eq => {
                self.analyze_operation_eq(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::Is => {
                self.analyze_operation_is(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::Period => {
                self.analyze_operation_period(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::LParen => {
                self.analyze_operation_apply(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::Comma => {
                self.analyze_operation_comma(ctx, scope, expr, operand1, operand2);
                return;
            }
            TokenType::Semicolon => {
                self.analyze_expr(ctx, scope, operand1);
                self.analyze_expr(ctx, scope, operand2);

                let finalized = self.expr(operand1).finalized && self.expr(operand2).finalized;

                let unit_type = self.get_builtin_type_id(UNIT_TYPE);

                let expr_mut = self.objs.expr_mut(expr);

                expr_mut.type_id = unit_type;
                expr_mut.finalized = finalized;

                return;
            }
            _ => (),
        }

        self.analyze_expr(ctx, scope, operand1);
        self.analyze_expr(ctx, scope, operand2);

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            return;
        }

        let boolean_type = self.get_builtin_type_id(BOOLEAN_TYPE);

        let operands = [operand1, operand2];

        let mut found_module = false;
        let mut operand_is_type = [false, false];
        let mut operand_types = [TypeID::default(), TypeID::default()];
        let mut type_variants: [Option<TypeVariant>; 2] = [None, None];

        for (idx, operand) in operands.iter().enumerate() {
            let mut type_id = self.objs.expr(*operand).type_id;

            match self.objs.type_get(type_id) {
                Type::Type(t) => {
                    operand_is_type[idx] = true;
                    type_id = *t;
                }
                Type::Module(_) => {
                    found_module = true;
                    continue;
                }
                _ => (),
            };

            operand_types[idx] = type_id;

            match self.objs.type_get(type_id) {
                Type::Scope(scope) => match self.objs.scope(*scope).variant {
                    ScopeVariant::Type(variant) => {
                        type_variants[idx] = Some(variant);
                    }
                    _ => (),
                },
                _ => (),
            }

            if operand_types[idx] == boolean_type {
                type_variants[idx] = Some(TypeVariant::Boolean);
            }
        }

        if found_module {
            self.invalid_operation(expr, "no binary operations may be applied to a module");
            return;
        }

        if operand_is_type[0] || operand_is_type[1] {
            self.invalid_operation(
                expr,
                "at least one operand is a type, currently only values supported",
            );
            return;
        };

        // below code is if both are values

        let (allowed_type_variants, returns_bool) =
            match AST::get_supported_type_variants_binary(op) {
                Some(info) => info,
                None => {
                    self.invalid_operation(expr, "not a supported binary operator");
                    return;
                }
            };

        // only for << and >> may lhs, rhs types be different
        if op != TokenType::LtLt && op != TokenType::GtGt && operand_types[0] != operand_types[1] {
            self.invalid_operation(expr, "operand types must be the same for this operation");
            return;
        }

        // check if it was possible to retrieve type variants, need to have certain type variants
        // for operations (e.g. for addition one possibility is Integers)

        let type_variants = match type_variants {
            [Some(v1), Some(v2)] => [v1, v2],
            _ => {
                self.invalid_operation(
                    expr,
                    "only certain variants of types permitted for this operation",
                );
                return;
            }
        };

        // check if for ALL operand type variants
        // one of the allowed type variants matches
        if !type_variants.iter().all(|elem| {
            allowed_type_variants
                .iter()
                .any(|allowed| *allowed == *elem)
        }) {
            self.invalid_operation(
                expr,
                "only certain variants of types permitted for this operation",
            );
            return;
        }

        let expr_type_id = if returns_bool {
            boolean_type
        } else {
            operand_types[0]
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.type_id = expr_type_id;
        expr_mut.finalized = true;
    }
}
