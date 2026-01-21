use crate::{
    parse::{
        AST, ExprVariant, ScopeVariant, Type,
        ast_contents::{ExprID, ScopeID, TypeID},
    },
    scan::{Token, TokenType},
    tokens::Tokens,
};

// util function that will attempt to parse char from Token
// so needs to be Character token and not malformed
// malformed would imply bug in char scanner function
pub fn tok_parse_char(tokens: &Tokens, tok: Token) -> Option<char> {
    if tok.ttype != TokenType::Character {
        return None;
    }

    let tok_str = tokens.tok_as_str(&tok);

    let mut chars = tok_str.chars();

    if chars.next()? != '\'' {
        return None;
    }

    let c;

    match chars.next()? {
        // esc sequence
        '\\' => {
            c = match chars.next()? {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                _ => return None,
            };
        }

        // regular char
        other @ _ => {
            c = other;
        }
    }

    if chars.next()? != '\'' {
        return None;
    }

    Some(c)
}

impl AST {
    pub fn type_to_string(&self, tokens: &Tokens, type_id: TypeID) -> String {
        let type_ref = self.objs.type_get(type_id);

        match type_ref.clone() {
            Type::List(_) | Type::Map(_) => todo!(),
            Type::String => "String".to_string(),
            Type::Error => "error".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::AnyType => "AnyType".to_string(),
            Type::Type(t) => format!("type({})", self.type_to_string(tokens, t),),
            Type::AnyModule => "AnyModule".to_string(),
            Type::Module(_) => "module".to_string(),
            Type::Alias(t) => self.type_to_string(tokens, t),
            Type::Ptr(t) => format!("*{}", self.type_to_string(tokens, t)),
            Type::Ref(t) => format!("&{}", self.type_to_string(tokens, t)),
            Type::Tuple((t, None)) => format!("({},)", self.type_to_string(tokens, t)),
            Type::Tuple((t1, Some(t2))) => format!(
                "tuple({}, {})",
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
            Type::Builtin(builtin) => format!("(builtin {})", builtin.get_builtin_name(),),
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
            Type::ImportTarget(scope_id) => {
                format!("(from {})", self.scope_to_string(tokens, scope_id),)
            }
        }
    }

    pub fn scope_to_string(&self, tokens: &Tokens, scope: ScopeID) -> String {
        let name = match &self.objs.scope(scope).name {
            Some(token_or_string) => tokens.tok_or_string_to_string(token_or_string),
            None => "(anonymous)".to_string(),
        };

        format!("(scope {})", name,)
    }

    pub fn expr_to_string(&self, tokens: &Tokens, expr: ExprID) -> String {
        let expr_ref = self.expr(expr);

        match &expr_ref.variant {
            ExprVariant::Unit => "Unit".to_string(),
            ExprVariant::IntegerLiteral(i) => i.to_string(),
            ExprVariant::FloatLiteral(f) => f.to_string(),
            ExprVariant::CharacterLiteral(c) => c.to_string(),
            ExprVariant::StringLiteral(t) => tokens.tok_as_str(&t).to_string(),
            ExprVariant::Identifier(ident) => tokens.tok_as_str(&ident.name).to_string(),
            ExprVariant::BooleanLiteral(b) => b.to_string(),
            ExprVariant::Underscore => "_".to_string(),
            ExprVariant::DollarNumber(i) => format!(
                "${}",
                u.to_string(),
            ),
            ExprVariant::KWType => "KWType".to_string(),
            ExprVariant::KWModule => "KWModule".to_string(),
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
            ExprVariant::While(while_struct) => {
                format!(
                    "(while {} do {})",
                    self.expr_to_string(tokens, while_struct.cond),
                    self.expr_to_string(tokens, while_struct.body),
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
        }
    }
}
