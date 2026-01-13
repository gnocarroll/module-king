use crate::{
    parse::{AST, ExprVariant, ast_contents::ExprID},
    tokens::Tokens,
};

impl AST {
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
                format!(
                    "(function {}{} => {} = {})",
                    match function_literal.name {
                        Some(tok) => format!("{} ", tokens.tok_as_str(&tok),),
                        None => "".to_string(),
                    },
                    self.expr_to_string(tokens, function_literal.params),
                    self.expr_to_string(tokens, function_literal.return_type),
                    self.expr_to_string(tokens, function_literal.body),
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
