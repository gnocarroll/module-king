pub mod expr;
pub mod util;

use operator::OperatorVariant::*;

use crate::{
    parse::{
        AST, Expr, ExprVariant, Function, FunctionLiteral, Identifier, IdentifierVariant, Operation, Tokens, TypeLiteral, TypeVariant, ast_contents::{ExprID, MemberID}, errors::{InvalidExpr, NameMismatch, ParseError, SemanticError}, operator
    },
    scan::{Token, TokenType},
    tokens::ExpectedToken,
};

impl AST {
    fn expect(&mut self, tokens: &mut Tokens, ttype: TokenType) -> Result<Token, ExpectedToken> {
        let ret = tokens.expect(ttype);

        if let Err(e) = &ret {
            self.parse_errors.push(ParseError::ExpectedToken(*e))
        }

        ret
    }

    // expect sequence of tokens, returns last one
    fn expect_sequence(
        &mut self,
        tokens: &mut Tokens,
        ttypes: &[TokenType],
    ) -> Result<Token, ExpectedToken> {
        let mut ret: Token = Token::default();

        for ttype in ttypes {
            ret = match self.expect(tokens, *ttype) {
                Ok(t) => t,
                Err(e) => return Err(e),
            }
        }

        Ok(ret)
    }

    fn test_name_match(
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

    fn parse_if(&mut self, tokens: &mut Tokens) -> ExprID {
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
            tokens.next();

            let cond = self.parse_expr(tokens);

            let _ = tokens.expect(TokenType::Then);

            let body = self.parse_expr(tokens);

            // else_expr is set later
            let if_expr = self.expr_if(cond, body, None, is_elif);

            // the first if expr is returned from this function
            if !is_elif {
                ret = if_expr;
            }

            // set else arm of previous if/elif to current if expr

            set_else_arm(self, prev_expr, if_expr);

            match tokens.peek().ttype {
                TokenType::Elif => (), // => do another loop

                // otherwise loop will be terminating because of else, end, or invalid token
                TokenType::Else => {
                    tokens.next();

                    let else_expr = self.parse_expr(tokens);

                    set_else_arm(self, Some(if_expr), else_expr);

                    if self
                        .expect_sequence(tokens, &[TokenType::End, TokenType::If])
                        .is_err()
                    {
                        tokens.sync(&[TokenType::Semicolon]);
                    }

                    break;
                }
                TokenType::End => {
                    tokens.next();
                    let _ = self.expect(tokens, TokenType::If);
                    break;
                }
                _ => {
                    // this will cause sensible error message about expecting end token
                    let _ = self.expect(tokens, TokenType::End);

                    let peek = tokens.sync(&[TokenType::End, TokenType::Semicolon]);

                    match peek.ttype {
                        TokenType::End => {
                            tokens.next();

                            if tokens.peek().ttype != TokenType::Semicolon {
                                tokens.next();
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
    fn parse_function(&mut self, tokens: &mut Tokens) -> ExprID {
        let tok_idx = tokens.idx();

        tokens.next();

        let tok = tokens.peek();

        let name = match tok.ttype {
            TokenType::Identifier => {
                tokens.next();

                Some(tok)
            }
            _ => None,
        };

        // having this in lambda makes it easy to jump ahead if failure occurs
        // it is Temu goto
        let attempt_parse_func = |ast: &mut AST, tokens: &mut Tokens| {
            if ast.expect(tokens, TokenType::LParen).is_err() {
                return (
                    false,
                    ast.expr_unit(tokens.idx()),
                    ast.expr_unit(tokens.idx()),
                    ast.expr_unit(tokens.idx()),
                );
            }

            let params = ast.parse_expr(tokens);

            if ast.expect(tokens, TokenType::RParen).is_err() {
                return (
                    false,
                    params,
                    ast.expr_unit(tokens.idx()),
                    ast.expr_unit(tokens.idx()),
                );
            }

            let return_type = ast.parse_expr(tokens);

            // prevent consumption of semicolon/comma if there is one after function
            // body since this could lead to e.g. a lot of the rest of the
            // file being consumed as part of func body

            let body = ast.parse_expr_bp(
                tokens,
                match operator::get_bp(TokenType::Comma, Infix) {
                    Some((l_bp, _)) => l_bp + 1,
                    _ => panic!("comma is not infix op?"),
                },
            );

            // if body of function is block expr check for desired terminating token
            // (keyword "function")

            let success = match ast.expr(body).variant {
                ExprVariant::Operation(Operation {
                    op: TokenType::Begin,
                    ..
                }) => ast.expect(tokens, TokenType::Function).is_ok(),
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

        let (success, params, return_type, body) = attempt_parse_func(self, tokens);

        // if problem occurred during function parsing try to
        // recover by skipping ahead to one of these ttypes

        if !success {
            let found = tokens.sync(&[
                TokenType::End,
                TokenType::Function,
                TokenType::Semicolon,
                TokenType::Comma,
            ]);

            match found.ttype {
                TokenType::End => {
                    tokens.next();

                    match tokens.peek().ttype {
                        TokenType::Function | TokenType::Identifier => {
                            tokens.next();
                        }
                        _ => (),
                    };
                }
                TokenType::Function => {
                    tokens.next();
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
            end_tok: tokens.idx(),
            variant: ExprVariant::FunctionLiteral(FunctionLiteral {
                params: params,
                return_type_expr: return_type,
                function_id,
            }),
            ..Default::default()
        })
    }

    fn parse_number_type_literal(&mut self, tokens: &mut Tokens) -> ExprID {
        let tok_idx = tokens.idx();

        let variant = match tokens.next().ttype {
            TokenType::KWInteger => TypeVariant::Integer,
            TokenType::KWFloat => TypeVariant::Float,
            _ => panic!("unexpected ttype for beginning of type literal"),
        };

        let body = self.expr_unit(tok_idx + 1);

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tokens.idx(),
            variant: ExprVariant::TypeLiteral(TypeLiteral { variant, body }),
            ..Default::default()
        })
    }

    fn parse_record_literal(&mut self, tokens: &mut Tokens) -> ExprID {
        let tok_idx = tokens.idx();
        let type_variant_tok = tokens.next();

        let variant = match type_variant_tok.ttype {
            TokenType::Record => TypeVariant::Record,
            TokenType::Variant => TypeVariant::Variant,
            _ => panic!("unexpected ttype for beginning of type literal"),
        };

        // don't want to consume semicolon as part of body

        let body = self.parse_expr(tokens);

        if self
            .expect_sequence(tokens, &[TokenType::End, type_variant_tok.ttype])
            .is_err()
        {
            let tok = tokens.sync(&[TokenType::Semicolon, TokenType::End, type_variant_tok.ttype]);

            match tok.ttype {
                TokenType::End => {
                    tokens.next();

                    if tokens.peek().ttype == type_variant_tok.ttype {
                        tokens.next();
                    }
                }
                val @ _ if val == type_variant_tok.ttype => {
                    tokens.next();
                }
                _ => (),
            }
        }

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tokens.idx(),
            variant: ExprVariant::TypeLiteral(TypeLiteral { variant, body }),
            ..Default::default()
        })
    }

    // atom e.g. literal like integer
    fn parse_atom(&mut self, tokens: &mut Tokens) -> ExprID {
        let tok_idx = tokens.idx();
        let tok = tokens.peek();

        match tok.ttype {
            TokenType::Underscore => {
                tokens.next();
                self.expr_underscore(tok_idx)
            }
            TokenType::DollarNumber => {
                tokens.next();
                self.expr_dollar_number(tokens, tok_idx, &tok)
            }
            // single token (e.g. integer) literals or ident
            TokenType::Integer | TokenType::Float | TokenType::String | TokenType::Identifier => {
                tokens.next();

                self.expr_push(Expr {
                    tok: tok_idx,
                    end_tok: tok_idx + 1,
                    variant: match tok.ttype {
                        TokenType::Integer => ExprVariant::IntegerLiteral(
                            tokens
                                .tok_as_str(&tok)
                                .parse::<u64>()
                                .expect("integer scanning or getting token text is broken"),
                        ),
                        TokenType::Float => ExprVariant::FloatLiteral(
                            tokens
                                .tok_as_str(&tok)
                                .parse::<f64>()
                                .expect("float scanning or getting token text is broken"),
                        ),
                        TokenType::String => ExprVariant::StringLiteral(tok),
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
            TokenType::If => self.parse_if(tokens),
            TokenType::Function => self.parse_function(tokens),
            TokenType::KWInteger | TokenType::KWFloat => self.parse_number_type_literal(tokens),
            TokenType::Record | TokenType::Variant => self.parse_record_literal(tokens),
            // if no atom or other (e.g. prefix) expr is found return Unit
            _ => self.expr_unit(tok_idx),
        }
    }

    fn parse_lhs(&mut self, tokens: &mut Tokens) -> ExprID {
        let tok = tokens.peek();
        let tok_idx = tokens.idx();

        // see if you have prefix op

        match operator::get_bp(tok.ttype, Prefix) {
            Some((_, r_bp)) => {
                tokens.next();

                let rhs = self.parse_expr_bp(tokens, r_bp);

                return self.expr_prefix(tok, rhs);
            }
            _ => {}
        }

        // around "operator" e.g. parens like this (1)

        for op_variant in [Around, PrefixAround] {
            match operator::get_bp(tok.ttype, op_variant) {
                Some((_, r_bp)) => {
                    tokens.next();

                    let lhs = self.parse_expr(tokens);

                    let mut found_end = false;

                    match tokens.expect(match tok.ttype {
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
                        let rhs = self.parse_expr_bp(tokens, r_bp);

                        return self.expr_prefix_around(tok, lhs, rhs);
                    }
                }
                _ => {}
            }
        }

        self.parse_atom(tokens)
    }

    // bp is Binding Power (Pratt parsing) and ret is expr id
    fn parse_expr_bp(&mut self, tokens: &mut Tokens, min_bp: u8) -> ExprID {
        let mut lhs = self.parse_lhs(tokens);

        loop {
            let op = tokens.peek();

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

                tokens.next();

                // how lhs is replaced depends on op variant

                match op_variant {
                    Infix => {
                        let rhs = self.parse_expr_bp(tokens, r_bp);

                        lhs = self.expr_infix(op, lhs, rhs);
                    }
                    Postfix => {
                        lhs = self.expr_postfix(op, lhs);
                    }
                    PostfixAround => {
                        // e.g. function call
                        let rhs = self.parse_expr(tokens);

                        let mut found_end = false;

                        match tokens.expect(match op.ttype {
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
    fn parse_expr(&mut self, tokens: &mut Tokens) -> ExprID {
        self.parse_expr_bp(tokens, 0)
    }

    pub fn do_syntax_analysis(&mut self, tokens: &mut Tokens) {
        self.root_expr = Some(self.parse_expr(tokens));

        // should be on EOF at this point
        let _ = self.expect(tokens, TokenType::Eof);
    }
}
