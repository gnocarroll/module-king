use std::num::TryFromIntError;

use crate::{
    constants::{BOOLEAN_TYPE, ERROR_TYPE, UNIT_TYPE},
    parse::{
        AST, ExprReturns, ExprVariant, Function, IdentifierVariant, MemberVariant, Operation,
        ScopeVariant, Type, TypeOrModule, TypeVariant, Visibility,
        ast_contents::{ExprID, ScopeID, TypeID},
        operator,
        semantic::{AnalyzingNow, SemanticContext},
    },
    scan::TokenType,
    tokens::TokenOrString,
};

use operator::OperatorVariant::*;

// if doing apply operator (e.g. "f()") what are you doing
// (e.g. are you calling a function or casting to other type)
#[derive(Clone, Copy, PartialEq)]
enum ApplyCase {
    Cast,
    Function,
}

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
            match operand1_struct.type_or_module {
                TypeOrModule::Type(t) => t,
                TypeOrModule::Module(_) => {
                    self.invalid_operation(expr, "use \"is\" to assign to/create modules");
                    err_type
                }
            }
        } else {
            self.invalid_operation(expr, "lhs of assignment should be an assignable variable");
            err_type
        };

        let operand2_struct = self.objs.expr(operand2).clone();

        let rhs_type = if !operand2_struct.finalized {
            err_type
        } else if operand2_struct.expr_returns == ExprReturns::Value {
            match operand2_struct.type_or_module {
                TypeOrModule::Type(t) => t,
                TypeOrModule::Module(_) => {
                    panic!("expr returns value but Module found");
                }
            }
        } else {
            err_type
        };

        let mut finalized = false;

        if lhs_type != err_type && rhs_type != err_type && lhs_type != rhs_type {
            self.invalid_operation(
                expr,
                "lhs and rhs type are not the same for this assignment",
            );
        } else {
            finalized = true;
        }

        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.expr_returns = ExprReturns::Unit;
        expr_mut.type_or_module = TypeOrModule::Type(unit_type);
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

        let mut finalized = false;

        if let (ExprReturns::Type, TypeOrModule::Type(type_id), Some(name)) =
            (rhs.expr_returns, rhs.type_or_module.clone(), name)
        {
            self.scope_add_member_type_from_name_and_id(
                ctx,
                scope,
                TokenOrString::Token(name),
                type_id,
            );

            finalized = true;
        } else {
            self.invalid_operation(expr, "creation of new type could not be completed");
        }

        let unit_type = self.get_builtin_type_id(UNIT_TYPE);

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.expr_returns = ExprReturns::Unit;
        expr_mut.type_or_module = TypeOrModule::Type(unit_type);
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

        let type_scope = match operand1_struct.type_or_module {
            TypeOrModule::Module(scope) => scope,
            TypeOrModule::Type(t) => {
                let mut type_id = t;

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
            ExprVariant::Identifier(ident) => ctx.tokens.tok_as_str(&ident.name).to_string(),
            _ => {
                self.invalid_operation(expr, "rhs of access should be an identifier");
                return;
            }
        };

        let member_id = if let Some(member_id) = self.objs.scope(type_scope).members.get(&name) {
            *member_id
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
            operand1_struct.expr_returns,
            self.objs.member(member_id).visibility,
        ) {
            // should only be accessing global fields through type
            (ExprReturns::Type, Visibility::Export | Visibility::Private) => {
                self.invalid_operation(expr, "only global fields may be accessed through type, not ones specific to an instance");
                return;
            }
            _ => (),
        }

        let (expr_returns, type_or_module) = match self.objs.member(member_id).variant {
            MemberVariant::Instance(type_id) => (ExprReturns::Value, TypeOrModule::Type(type_id)),
            MemberVariant::Type(type_id) => (ExprReturns::Type, TypeOrModule::Type(type_id)),
            MemberVariant::Module(scope_id) => {
                (ExprReturns::Module, TypeOrModule::Module(scope_id))
            }
            MemberVariant::Function(function_id) => (
                ExprReturns::Value,
                TypeOrModule::Type(self.objs.function(function_id).func_type),
            ),
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.expr_returns = expr_returns;
        expr_mut.type_or_module = type_or_module;

        expr_mut.is_var = true;
        expr_mut.finalized = true;
    }

    // apply a function e.g. f()
    // TokenType is actually LParen
    fn analyze_operation_apply(
        &mut self,
        ctx: &mut SemanticContext,
        scope: ScopeID,
        expr: ExprID,
        operand1: ExprID,
        operand2: ExprID,
    ) {
        self.analyze_expr(ctx, scope, operand1);

        let mut finalized = true;
        let mut ret_type = self.get_builtin_type_id(ERROR_TYPE);

        let operand1_struct = self.expr(operand1);

        let mut apply_case = ApplyCase::Function;

        match (
            operand1_struct.finalized,
            operand1_struct.expr_returns,
            &operand1_struct.type_or_module,
        ) {
            (false, _, _) => (),

            // type cast

            (true, ExprReturns::Type, TypeOrModule::Type(t)) => {
                apply_case = ApplyCase::Cast;
                ret_type = *t;
            }

            // function call, check if type is function

            (true, ExprReturns::Value, TypeOrModule::Type(t))
                if matches!(self.objs.type_get(*t), Type::Function(_)) => {

                if let Type::Function((_, ret_t)) = self.objs.type_get(*t) {
                    ret_type = *ret_t;
                } else {
                    panic!("should already have been determined that type is function");
                }
            }

            // invalid

            _ => {
                self.invalid_operation(expr, "should be type cast or function call");
                finalized = false;
            }
        }

        let old_analyzing_now = ctx.analyzing_now;

        ctx.analyzing_now = AnalyzingNow::FuncArgs;
        self.analyze_expr(ctx, scope, operand2);
        ctx.analyzing_now = old_analyzing_now;

        if !self.expr(operand1).finalized || !self.expr(operand2).finalized {
            finalized = false;
        }

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.finalized = finalized;
        expr_mut.expr_returns = ExprReturns::Value;
        expr_mut.type_or_module = TypeOrModule::Type(ret_type)
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
                let (pattern, _) = self.analyze_instance_creation(
                    ctx,
                    scope,
                    expr,
                    Some(operand1),
                    Some(operand2),
                );

                self.scope_create_members_from_pattern(ctx, scope, pattern);

                // after creation of members analyze lhs (now relevant vars should exist)
                self.analyze_expr(ctx, scope, operand1);

                return;
            }
            TokenType::ColonEq => {
                // var creation + assignment + type inference
                // operand1 := operand2;

                self.analyze_expr(ctx, scope, operand2);

                let type_id = if self.objs.expr(operand2).finalized {
                    match self.objs.expr(operand2).type_or_module {
                        TypeOrModule::Type(t) => Some(t),
                        _ => None,
                    }
                } else {
                    None
                };

                let (pattern, _) = self.pattern_matching(ctx, scope, operand1, type_id);

                self.scope_create_members_from_pattern(ctx, scope, pattern);

                // after creation of members analyze lhs (now relevant vars should exist)
                self.analyze_expr(ctx, scope, operand1);

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

                expr_mut.expr_returns = ExprReturns::Unit;
                expr_mut.type_or_module = TypeOrModule::Type(unit_type);
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
        let mut type_variants = [TypeVariant::Error, TypeVariant::Error];

        for (idx, operand) in operands.iter().enumerate() {
            let expr_returns = self.objs.expr(*operand).expr_returns;

            if expr_returns == ExprReturns::Module {
                found_module = true;
                continue;
            } else if expr_returns == ExprReturns::Type {
                operand_is_type[idx] = true;
            }

            let type_id = match self.objs.expr(*operand).type_or_module {
                TypeOrModule::Type(t) => t,
                TypeOrModule::Module(_) => {
                    continue;
                }
            };

            operand_types[idx] = type_id;

            match self.objs.type_get(type_id) {
                Type::Scope(scope) => match self.objs.scope(*scope).variant {
                    ScopeVariant::Type(variant) => {
                        type_variants[idx] = variant;
                    }
                    _ => (),
                },
                _ => (),
            }

            if operand_types[idx] == boolean_type {
                type_variants[idx] = TypeVariant::Boolean;
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

        let expr_type = if returns_bool {
            boolean_type
        } else {
            operand_types[0]
        };

        let expr_mut = self.objs.expr_mut(expr);

        expr_mut.expr_returns = ExprReturns::Value;
        expr_mut.type_or_module = TypeOrModule::Type(expr_type);
        expr_mut.finalized = true;
    }
}
