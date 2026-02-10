use crate::{
    parse::{
        AST, ExprVariant, HasFileModule, MemberVariant, ScopeVariant, Type, ast_contents::{ExprID, FunctionID, ScopeID, TypeID}
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

// similar to above but for String Token -> Vec<u8>
pub fn tok_parse_string(tokens: &Tokens, tok: Token) -> Option<Vec<u8>> {
    if tok.ttype != TokenType::String {
        return None;
    }

    let tok_str = tokens.tok_as_str(&tok);

    let mut chars = tok_str.chars();

    if chars.next()? != '"' {
        return None;
    }

    let mut ret_string = Vec::<u8>::new();

    let mut found_end_quote = false;
    let mut is_escaped = false;

    while let Some(c) = chars.next() {
        let push_c = if is_escaped {
            is_escaped = false;

            match c {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => c,
            }
        } else {
            if c == '"' {
                found_end_quote = true;
                break;
            } else if c == '\\' {
                is_escaped = true;
                continue;
            } else {
                c
            }
        };

        ret_string.push(
            push_c
                .try_into()
                .expect("should have made sure all chars were valid u8 earlier"),
        );
    }

    // should have found terminating quote
    // => should be no further chars

    if !found_end_quote || chars.next().is_some() {
        return None;
    }

    Some(ret_string)
}

impl AST {
    pub fn tokens_peek(&self) -> Token {
        self.tokens().peek()
    }

    pub fn tokens_idx(&self) -> u32 {
        self.tokens().idx()
    }

    pub fn tokens_next(&mut self) -> Token {
        self.tokens_mut().next()
    }

    pub fn type_to_string(&self, type_id: TypeID) -> String {
        let type_ref = self.objs.type_get(type_id);

        match type_ref.clone() {
            Type::List(type_id) => format!("List({})", self.type_to_string(type_id)),
            Type::Map(type_id) => format!("Map({})", self.type_to_string(type_id)),
            Type::String => "String".to_string(),
            Type::Error => "error".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::AnyType => "AnyType".to_string(),
            Type::Type(t) => format!("type({})", self.type_to_string(t),),
            Type::AnyModule => "AnyModule".to_string(),
            Type::Module(_) => "module".to_string(),
            Type::Alias(t) => self.type_to_string(t),
            Type::DeriveFrom(t) => format!("(derives from {})", self.type_to_string(t)),
            Type::Ptr(t) => format!("*{}", self.type_to_string(t)),
            Type::Ref(t) => format!("&{}", self.type_to_string(t)),
            Type::Tuple((t, None)) => format!("({},)", self.type_to_string(t)),
            Type::Tuple((t1, Some(t2))) => format!(
                "tuple({}, {})",
                self.type_to_string(t1),
                self.type_to_string(t2)
            ),
            Type::RestOfTuple((t1, t2)) => {
                format!("{}, {}", self.type_to_string(t1), self.type_to_string(t2))
            }
            Type::Function((params, ret)) => format!(
                "(function {} => {})",
                self.type_to_string(params),
                self.type_to_string(ret),
            ),
            Type::Builtin(builtin) => format!("(builtin {})", builtin.get_builtin_name(),),
            Type::Slice((t, idx)) => {
                format!("{}[{}]", self.type_to_string(t), self.type_to_string(idx.type_id),)
            }
            Type::Scope(scope_id) => {
                let scope = self.objs.scope(scope_id);

                let variant_string = match scope.variant {
                    ScopeVariant::Type(type_variant) => format!("{:?}", type_variant),
                    _ => "BAD_SCOPE_VARIANT".to_string(),
                };

                let name = match &scope.name {
                    Some(t_or_s) => scope_id.get_tokens(self).tok_or_string_to_string(t_or_s),
                    None => "(anonymous)".to_string(),
                };

                // TODO: provide more information about members of type

                format!("({} {})", variant_string, name)
            }
            Type::ImportTarget(scope_id) => {
                format!("(from {})", self.scope_to_string(scope_id))
            }
        }
    }

    pub fn scope_to_string(&self, scope: ScopeID) -> String {
        let scope_struct = self.objs.scope(scope);

        let name = match &scope_struct.name {
            Some(token_or_string) => scope
                .get_tokens(self)
                .tok_or_string_to_string(token_or_string),
            None => "(anonymous)".to_string(),
        };

        let mut member_strings: Vec<String> = Vec::new();

        for member_id in scope_struct.members.member_iter() {
            let name = member_id.get_name(self).expect("all members should be named");

            let desc = match self.objs.member(member_id).variant {
                MemberVariant::Builtin(builtin) => format!(
                    "Builtin {}",
                    builtin.get_builtin_name(),
                ),
                MemberVariant::Function(function_id) => {
                    self.function_to_string(function_id)
                },
                MemberVariant::Instance(type_id) => {
                    format!("w/ type {}", self.type_to_string(type_id))
                }
                MemberVariant::Module(scope_id) => {
                    format!("module {}", self.scope_to_string(scope_id))
                }
                MemberVariant::Type(type_id) => {
                    format!("is type {}", self.type_to_string(type_id))
                }
            };

            member_strings.push(format!("{} {}", name, desc));
        }

        format!("(scope {}\n{})", name, member_strings.join("\n"))
    }

    pub fn function_to_string(&self, function_id: FunctionID) -> String {
        let func = self.objs.function(function_id);

        let function_literal = match &self.objs.expr(func.literal).variant {
            ExprVariant::FunctionLiteral(function_literal) => function_literal,
            _ => {
                panic!("Expr stored in literal field of struct Function should be FunctionLiteral");
            }
        };

        format!(
            "(function {}{} => {})",
            match func.name {
                Some(tok) => format!("{} ", func.literal.get_tokens(self).tok_as_str(&tok),),
                None => "".to_string(),
            },
            self.expr_to_string(function_literal.params),
            self.expr_to_string(function_literal.return_type_expr),
        )
    }

    pub fn expr_to_string(&self, expr: ExprID) -> String {
        let expr_ref = self.expr(expr);

        match &expr_ref.variant {
            ExprVariant::Unit => "Unit".to_string(),
            ExprVariant::IntegerLiteral(i) => i.to_string(),
            ExprVariant::FloatLiteral(f) => f.to_string(),
            ExprVariant::CharacterLiteral(c) => c.to_string(),
            ExprVariant::StringLiteral(v) => std::str::from_utf8(v).unwrap().to_string(),
            ExprVariant::Identifier(ident) => {
                let ident = ident.clone();

                expr.get_tokens(self).tok_as_str(&ident.name).to_string()
            }
            ExprVariant::BooleanLiteral(b) => b.to_string(),
            ExprVariant::Underscore => "_".to_string(),
            ExprVariant::DollarNumber(i) => format!("${}", i.to_string(),),
            ExprVariant::KWType => "KWType".to_string(),
            ExprVariant::KWModule => "KWModule".to_string(),
            ExprVariant::Operation(operation) => {
                let operand_to_string = |operand: Option<ExprID>| match operand {
                    Some(id) => format!(" {}", self.expr_to_string(id),),
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
                    self.expr_to_string(if_expr.cond),
                    self.expr_to_string(if_expr.body),
                    match if_expr.else_expr {
                        Some(expr) => format!(" else {}", self.expr_to_string(expr)),
                        None => "".to_string(),
                    }
                )
            }
            ExprVariant::While(while_struct) => {
                format!(
                    "(while {} do {})",
                    self.expr_to_string(while_struct.cond),
                    self.expr_to_string(while_struct.body),
                )
            }
            ExprVariant::FunctionLiteral(function_literal) => {
                self.function_to_string(function_literal.function_id)
            }
            ExprVariant::TypeLiteral(type_literal) => {
                format!(
                    "(type {:?} {})",
                    type_literal.variant,
                    self.expr_to_string(type_literal.body),
                )
            }
            ExprVariant::Block(block) => {
                format!(
                    "(block {})",
                    self.expr_to_string(block.body),
                )
            }
        }
    }
}
