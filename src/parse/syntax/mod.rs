pub mod expr;
pub mod util;

use operator::OperatorVariant::*;

use crate::{
    parse::{
        AST, Block, Expr, ExprVariant, Function, FunctionLiteral, Identifier, IdentifierVariant,
        Operation, Tokens, TypeLiteral, TypeVariant, While,
        ast_contents::{ExprID, MemberID},
        errors::{InvalidExpr, NameMismatch, ParseError, SemanticError},
        operator,
    },
    scan::{Token, TokenType},
    tokens::ExpectedToken,
};

impl AST {
    fn expect(&mut self, ttype: TokenType) -> Result<Token, ExpectedToken> {
        let ret = self.tokens_mut().expect(ttype);

        if let Err(e) = &ret {
            self.parse_errors.push(ParseError::ExpectedToken(*e))
        }

        ret
    }

    // expect sequence of tokens, returns last one
    fn expect_sequence(&mut self, ttypes: &[TokenType]) -> Result<Token, ExpectedToken> {
        let mut ret: Token = Token::default();

        for ttype in ttypes {
            ret = match self.expect(*ttype) {
                Ok(t) => t,
                Err(e) => return Err(e),
            }
        }

        Ok(ret)
    }

    fn _test_name_match(
        &mut self,
        tokens: &mut Tokens,
        expected: Token,
        found: Token,
    ) -> Result<(), NameMismatch> {
        if tokens.tok_as_str(&expected) == tokens.tok_as_str(&found) {
            Ok(())
        } else {
            let ret = NameMismatch {
                expected: expected,
                found: found,
            };

            self.parse_errors.push(ParseError::NameMismatch(ret));

            Err(ret)
        }
    }

    fn parse_while(&mut self) -> ExprID {
        let start_tok_idx = self.tokens_idx();

        self.tokens_next();

        let cond = self.parse_expr();

        let _ = self.expect(TokenType::Do);

        let body = self.parse_expr();

        if self
            .expect_sequence(&[TokenType::End, TokenType::While])
            .is_err()
        {
            let found = self
                .tokens_mut()
                .sync(&[TokenType::End, TokenType::Semicolon]);

            if found.ttype == TokenType::End {
                self.tokens_next();

                if self.tokens_peek().ttype == TokenType::While {
                    self.tokens_next();
                }
            }
        }

        self.objs.expr_push(Expr {
            tok: start_tok_idx,
            end_tok: self.tokens_idx(),
            variant: ExprVariant::While(While {
                cond,
                body,
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    fn parse_if(&mut self) -> ExprID {
        let set_else_arm = |ast: &mut AST, target: Option<ExprID>, else_expr: ExprID| {
            if let Some(target) = target {
                match &mut ast.objs.expr_mut(target).variant {
                    ExprVariant::If(expr_ref) | ExprVariant::Elif(expr_ref) => {
                        expr_ref.else_expr = Some(else_expr);
                    }
                    _ => panic!("target should always be if or elif"),
                }
            }
        };

        let mut ret = ExprID::default();

        // will need to set else arm of if/elif from previous loop
        let mut prev_expr = None;

        // indicates current loop is elif (as opposed to if)
        let mut is_elif = false;

        loop {
            // consume if/elif token
            self.tokens_next();

            let cond = self.parse_expr();

            let _ = self.expect(TokenType::Then);

            let body = self.parse_expr();

            // else_expr is set later
            let if_expr = self.expr_if(cond, body, None, is_elif);

            // the first if expr is returned from this function
            if !is_elif {
                ret = if_expr;
            }

            // set else arm of previous if/elif to current if expr

            set_else_arm(self, prev_expr, if_expr);

            match self.tokens_peek().ttype {
                TokenType::Elif => (), // => do another loop

                // otherwise loop will be terminating because of else, end, or invalid token
                TokenType::Else => {
                    self.tokens_next();

                    let else_expr = self.parse_expr();

                    set_else_arm(self, Some(if_expr), else_expr);

                    if self
                        .expect_sequence(&[TokenType::End, TokenType::If])
                        .is_err()
                    {
                        self.tokens_mut().sync(&[TokenType::Semicolon]);
                    }

                    break;
                }
                TokenType::End => {
                    self.tokens_next();
                    let _ = self.expect(TokenType::If);
                    break;
                }
                _ => {
                    // this will cause sensible error message about expecting end token
                    let _ = self.expect(TokenType::End);

                    let peek = self
                        .tokens_mut()
                        .sync(&[TokenType::End, TokenType::Semicolon]);

                    match peek.ttype {
                        TokenType::End => {
                            self.tokens_next();

                            if self.tokens_peek().ttype != TokenType::Semicolon {
                                self.tokens_next();
                            }
                        }
                        _ => (),
                    }

                    break;
                }
            }

            prev_expr = Some(if_expr);

            // loops after first are elif
            is_elif = true;
        }

        ret
    }

    // NOTE: this function assumes "function" is upcoming token so that should
    // have been checked at call site
    fn parse_function(&mut self) -> ExprID {
        let tok_idx = self.tokens_idx();

        self.tokens_next();

        let tok = self.tokens_peek();

        let name = match tok.ttype {
            TokenType::Identifier => {
                self.tokens_next();

                Some(tok)
            }
            _ => None,
        };

        // having this in lambda makes it easy to jump ahead if failure occurs
        // it is Temu goto
        let attempt_parse_func = |ast: &mut AST| {
            if ast.expect(TokenType::LParen).is_err() {
                let tok_idx = ast.tokens_idx();

                return (
                    false,
                    ast.expr_unit(tok_idx),
                    ast.expr_unit(tok_idx),
                    ast.expr_unit(tok_idx),
                );
            }

            let params = ast.parse_expr();

            if ast.expect(TokenType::RParen).is_err() {
                let tok_idx = ast.tokens_idx();

                return (
                    false,
                    params,
                    ast.expr_unit(tok_idx),
                    ast.expr_unit(tok_idx),
                );
            }

            let return_type = match ast.tokens_peek().ttype {
                TokenType::Begin | TokenType::Return => ast.expr_unit(ast.tokens_idx()),
                _ => ast.parse_expr(),
            };

            // prevent consumption of semicolon/comma if there is one after function
            // body since this could lead to e.g. a lot of the rest of the
            // file being consumed as part of func body

            let body = ast.parse_expr_bp(match operator::get_bp(TokenType::Comma, Infix) {
                Some((l_bp, _)) => l_bp + 1,
                _ => panic!("comma is not infix op?"),
            });

            // if body of function is block expr check for desired terminating token
            // (keyword "function")

            let success = match ast.expr(body).variant {
                ExprVariant::Operation(Operation {
                    op: TokenType::Begin,
                    ..
                }) => ast.expect(TokenType::Function).is_ok(),
                ExprVariant::Operation(Operation {
                    op: TokenType::Return,
                    ..
                }) => true,
                _ => {
                    ast.semantic_errors
                        .push(SemanticError::InvalidExpr(InvalidExpr {
                            expr: body,
                            msg: "function body should be either block expr or return expr",
                        }));

                    false
                }
            };

            (success, params, return_type, body)
        };

        let (success, params, return_type, body) = attempt_parse_func(self);

        // if problem occurred during function parsing try to
        // recover by skipping ahead to one of these ttypes

        if !success {
            let found = self.tokens_mut().sync(&[
                TokenType::End,
                TokenType::Function,
                TokenType::Semicolon,
                TokenType::Comma,
            ]);

            match found.ttype {
                TokenType::End => {
                    self.tokens_next();

                    match self.tokens_peek().ttype {
                        TokenType::Function | TokenType::Identifier => {
                            self.tokens_next();
                        }
                        _ => (),
                    };
                }
                TokenType::Function => {
                    self.tokens_next();
                }
                _ => (),
            };
        }

        let function_id = self.objs.function_push(Function {
            name,
            body,
            ..Default::default()
        });

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: self.tokens_idx(),
            variant: ExprVariant::FunctionLiteral(FunctionLiteral {
                params: params,
                return_type_expr: return_type,
                function_id,
            }),
            ..Default::default()
        })
    }

    fn parse_number_type_literal(&mut self) -> ExprID {
        let tok_idx = self.tokens_idx();

        let variant = match self.tokens_next().ttype {
            TokenType::KWInteger => TypeVariant::Integer,
            TokenType::KWFloat => TypeVariant::Float,
            _ => panic!("unexpected ttype for beginning of type literal"),
        };

        let body = self.expr_unit(tok_idx + 1);

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: self.tokens_idx(),
            variant: ExprVariant::TypeLiteral(TypeLiteral {
                variant,
                body,
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    fn parse_record_literal(&mut self) -> ExprID {
        let tok_idx = self.tokens_idx();
        let type_variant_tok = self.tokens_next();

        let variant = match type_variant_tok.ttype {
            TokenType::Record => TypeVariant::Record,
            TokenType::Variant => TypeVariant::Variant,
            TokenType::Enum => TypeVariant::Enum,
            _ => panic!("unexpected ttype for beginning of type literal"),
        };

        // don't want to consume semicolon as part of body

        let body = self.parse_expr();

        if self
            .expect_sequence(&[TokenType::End, type_variant_tok.ttype])
            .is_err()
        {
            let tok = self.tokens_mut().sync(&[
                TokenType::Semicolon,
                TokenType::End,
                type_variant_tok.ttype,
            ]);

            match tok.ttype {
                TokenType::End => {
                    self.tokens_next();

                    if self.tokens_peek().ttype == type_variant_tok.ttype {
                        self.tokens_next();
                    }
                }
                val @ _ if val == type_variant_tok.ttype => {
                    self.tokens_next();
                }
                _ => (),
            }
        }

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: self.tokens_idx(),
            variant: ExprVariant::TypeLiteral(TypeLiteral {
                variant,
                body,
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    fn parse_block(&mut self) -> ExprID {
        // assumes TokenType::Begin has been checked for

        let tok_idx = self.tokens_idx();
        self.tokens_next();

        let body = self.parse_expr();

        if self.expect(TokenType::End).is_err() {
            let found = self
                .tokens_mut()
                .sync(&[TokenType::End, TokenType::Semicolon]);

            if found.ttype == TokenType::End {
                self.tokens_next();
            }
        }

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: self.tokens_idx(),
            variant: ExprVariant::Block(Block {
                body,
                ..Default::default()
            }),

            ..Default::default()
        })
    }

    // atom e.g. literal like integer
    fn parse_atom(&mut self) -> ExprID {
        let tok_idx = self.tokens_idx();
        let tok = self.tokens_peek();

        match tok.ttype {
            TokenType::True | TokenType::False => {
                self.tokens_next();

                let value = tok.ttype == TokenType::True;

                self.expr_push(Expr {
                    tok: tok_idx,
                    end_tok: tok_idx + 1,
                    variant: ExprVariant::BooleanLiteral(value),
                    ..Default::default()
                })
            }
            TokenType::Underscore => {
                self.tokens_next();
                self.expr_underscore(tok_idx)
            }
            TokenType::DollarNumber => {
                self.tokens_next();
                self.expr_dollar_number(tok_idx, &tok)
            }
            // single token (e.g. integer) literals or ident
            TokenType::Integer
            | TokenType::Float
            | TokenType::String
            | TokenType::Character
            | TokenType::Identifier => {
                self.tokens_next();

                self.expr_push(Expr {
                    tok: tok_idx,
                    end_tok: tok_idx + 1,
                    variant: match tok.ttype {
                        TokenType::Integer => ExprVariant::IntegerLiteral(
                            self.tokens()
                                .tok_as_str(&tok)
                                .parse::<u64>()
                                .expect("integer scanning or getting token text is broken"),
                        ),
                        TokenType::Float => ExprVariant::FloatLiteral(
                            self.tokens()
                                .tok_as_str(&tok)
                                .parse::<f64>()
                                .expect("float scanning or getting token text is broken"),
                        ),
                        TokenType::String => ExprVariant::StringLiteral(
                            util::tok_parse_string(self.tokens(), tok)
                                .expect("failed to get String from Token"),
                        ),
                        TokenType::Character => ExprVariant::CharacterLiteral(
                            util::tok_parse_char(self.tokens(), tok)
                                .expect("failed to get char from Token"),
                        ),
                        TokenType::Identifier => ExprVariant::Identifier(Identifier {
                            name: tok,

                            // dummy values for these fields for now
                            member_id: MemberID::default(),
                            variant: IdentifierVariant::Unknown,
                        }),
                        _ => panic!("single token literal parsing broken"),
                    },
                    ..Default::default()
                })
            }
            TokenType::Begin => self.parse_block(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Function => self.parse_function(),
            TokenType::KWInteger | TokenType::KWFloat => self.parse_number_type_literal(),
            TokenType::Record | TokenType::Variant | TokenType::Enum => self.parse_record_literal(),
            // if no atom or other (e.g. prefix) expr is found return Unit
            _ => self.expr_unit(tok_idx),
        }
    }

    fn parse_lhs(&mut self) -> ExprID {
        let tok = self.tokens_peek();

        // see if you have prefix op

        match operator::get_bp(tok.ttype, Prefix) {
            Some((_, r_bp)) => {
                self.tokens_next();

                let rhs = self.parse_expr_bp(r_bp);

                return self.expr_prefix(tok, rhs);
            }
            _ => {}
        }

        // around "operator" e.g. parens like this (1)

        for op_variant in [Around, PrefixAround] {
            match operator::get_bp(tok.ttype, op_variant) {
                Some((_, r_bp)) => {
                    self.tokens_next();

                    let lhs = self.parse_expr();

                    let mut found_end = false;

                    match self.expect(match tok.ttype {
                        TokenType::LParen => TokenType::RParen,
                        TokenType::LBrace => TokenType::RBrace,
                        TokenType::Begin => TokenType::End,
                        TokenType::Type => TokenType::Is,
                        _ => panic!("unexpected ttype for Around"),
                    }) {
                        Ok(_) => {
                            found_end = true;
                        }
                        Err(e) => {
                            self.parse_errors.push(ParseError::ExpectedToken(e));
                        }
                    }

                    if op_variant == Around {
                        return self.expr_around(tok, lhs, found_end);
                    } else {
                        let rhs = self.parse_expr_bp(r_bp);

                        return self.expr_prefix_around(tok, lhs, rhs);
                    }
                }
                _ => {}
            }
        }

        self.parse_atom()
    }

    // bp is Binding Power (Pratt parsing) and ret is expr id
    fn parse_expr_bp(&mut self, min_bp: u8) -> ExprID {
        let mut lhs = self.parse_lhs();

        // if lhs is Unit then do not try to parse further operators
        // there is no Infix, Postfix, or PostfixAround operator where it would be
        // valid to omit LHS

        match self.objs.expr(lhs).variant {
            ExprVariant::Unit => return lhs,
            _ => (),
        }

        loop {
            let op = self.tokens_peek();

            // see if one of the operator variants e.g. Infix works with
            // above token and get binding power if so

            let determine_op_variant_and_bp = || {
                for op_variant in [Infix, Postfix, PostfixAround] {
                    if let Some((l_bp, r_bp)) = operator::get_bp(op.ttype, op_variant) {
                        return Some((op_variant, l_bp, r_bp));
                    }
                }

                None
            };

            if let Some((op_variant, l_bp, r_bp)) = determine_op_variant_and_bp() {
                if l_bp < min_bp {
                    // applies to infix and postfix
                    break;
                }

                self.tokens_next();

                // how lhs is replaced depends on op variant

                match op_variant {
                    Infix => {
                        let rhs = self.parse_expr_bp(r_bp);

                        lhs = self.expr_infix(op, lhs, rhs);
                    }
                    Postfix => {
                        lhs = self.expr_postfix(op, lhs);
                    }
                    PostfixAround => {
                        // e.g. function call
                        let rhs = self.parse_expr();

                        let mut found_end = false;

                        match self.expect(match op.ttype {
                            TokenType::LParen => TokenType::RParen,
                            TokenType::LBrace => TokenType::RBrace,
                            TokenType::Begin => TokenType::End,
                            _ => panic!("unexpected ttype for Around"),
                        }) {
                            Ok(_) => {
                                found_end = true;
                            }
                            Err(e) => {
                                self.parse_errors.push(ParseError::ExpectedToken(e));
                            }
                        }

                        lhs = self.expr_postfix_around(op, lhs, rhs, found_end);
                    }
                    _ => panic!("UNEXPECTED OP VARIANT"),
                }
            } else {
                break;
            }
        }

        lhs
    }

    // ret expr id
    fn parse_expr(&mut self) -> ExprID {
        self.parse_expr_bp(0)
    }

    pub fn do_syntax_analysis(&mut self) -> ExprID {
        let ret = self.parse_expr();

        // should be on EOF at this point
        let _ = self.expect(TokenType::Eof);

        ret
    }
}
