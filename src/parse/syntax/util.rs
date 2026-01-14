use crate::{
    parse::{
        AST, ExprVariant, ScopeVariant, Type,
        ast_contents::{ExprID, TypeID},
    },
    tokens::Tokens,
};

impl AST {
    pub fn type_to_string(&self, tokens: &Tokens, type_id: TypeID) -> String {
        let type_ref = self.objs.type_get(type_id);

        match type_ref.clone() {
            Type::Alias(t) => self.type_to_string(tokens, t),
            Type::Ptr(t) => format!("*{}", self.type_to_string(tokens, t)),
            Type::Ref(t) => format!("&{}", self.type_to_string(tokens, t)),
            Type::Tuple((t, None)) => format!("({},)", self.type_to_string(tokens, t)),
            Type::Tuple((t1, Some(t2))) => format!(
                "({}, {})",
                self.type_to_string(tokens, t1),
                self.type_to_string(tokens, t2)
            ),
            Type::RestOfTuple((t1, t2)) => format!(
                "{}, {}",
                self.type_to_string(tokens, t1),
                self.type_to_string(tokens, t2)
            ),
            Type::Function((params, ret)) => format!(
                "(function {} => {})",
                self.type_to_string(tokens, params),
                self.type_to_string(tokens, ret),
            ),
            Type::Slice((idx, t)) => format!(
                "[{}]{}",
                self.expr_to_string(tokens, idx),
                self.type_to_string(tokens, t),
            ),
            Type::Scope(scope) => {
                let scope = self.objs.scope(scope);

                let name = match &scope.name {
                    Some(t_or_s) => tokens.tok_or_string_to_string(t_or_s),
                    None => "(anonymous)".to_string(),
                };

                let variant_string = match scope.variant {
                    ScopeVariant::Type(type_variant) => format!("{:?}", type_variant),
                    _ => "BAD_SCOPE_VARIANT".to_string(),
                };

                // TODO: provide more information about members of type

                format!("({} {})", variant_string, name)
            }
        }
    }

    pub fn expr_to_string(&self, tokens: &Tokens, expr: ExprID) -> String {
        let expr_ref = self.expr(expr);

        match &expr_ref.variant {
            ExprVariant::Unit => "Unit".to_string(),
            ExprVariant::IntegerLiteral(i) => i.to_string(),
            ExprVariant::FloatLiteral(f) => f.to_string(),
            ExprVariant::StringLiteral(t) => tokens.tok_as_str(&t).to_string(),
            ExprVariant::Identifier(ident) => tokens.tok_as_str(&ident.name).to_string(),
            ExprVariant::Operation(operation) => {
                let operand_to_string = |operand: Option<ExprID>| match operand {
                    Some(id) => format!(" {}", self.expr_to_string(tokens, id),),
                    None => "".to_string(),
                };

                format!(
                    "({}{}{})",
                    operation.op,
                    operand_to_string(operation.operand1),
                    operand_to_string(operation.operand2),
                )
            }
            ExprVariant::If(if_expr) | ExprVariant::Elif(if_expr) => {
                format!(
                    "(if {} then {}{})",
                    self.expr_to_string(tokens, if_expr.cond),
                    self.expr_to_string(tokens, if_expr.body),
                    match if_expr.else_expr {
                        Some(expr) => format!(" else {}", self.expr_to_string(tokens, expr)),
                        None => "".to_string(),
                    }
                )
            }
            ExprVariant::FunctionLiteral(function_literal) => {
                let func = self.objs.function(function_literal.function_id);

                format!(
                    "(function {}{} => {} = {})",
                    match func.name {
                        Some(tok) => format!("{} ", tokens.tok_as_str(&tok),),
                        None => "".to_string(),
                    },
                    self.expr_to_string(tokens, function_literal.params),
                    self.expr_to_string(tokens, function_literal.return_type_expr),
                    self.expr_to_string(tokens, func.body),
                )
            }
            ExprVariant::TypeLiteral(type_literal) => {
                format!(
                    "(type {:?} {})",
                    type_literal.variant,
                    self.expr_to_string(tokens, type_literal.body),
                )
            }
            _ => "ERR".to_string(),
        }
    }
}
