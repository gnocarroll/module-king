use operator::OperatorVariant::*;

use crate::{
    parse::{
        AST, Expr, ExprVariant, FunctionLiteral, Identifier, IdentifierVariant, If, Operation, Tokens, TypeLiteral, TypeVariant, errors::{ExpectedToken, NameMismatch, ParseError}, operator
    },
    scan::{Token, TokenType},
};

impl AST {
    pub fn expr(&self, expr: u32) -> &Expr {
        &self.exprs[expr as usize]
    }

    pub fn expr_mut(&mut self, expr: u32) -> &mut Expr {
        &mut self.exprs[expr as usize]
    }

    fn expr_unit(&mut self, tok_idx: u32) -> u32 {
        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tok_idx,
            variant: ExprVariant::Unit,
            ..Default::default()
        })
    }

    fn expr_push(&mut self, expr: Expr) -> u32 {
        self.exprs.push(expr);

        (self.exprs.len() - 1) as u32
    }

    fn expr_infix(&mut self, op: Token, lhs: u32, rhs: u32) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(lhs).tok,
            end_tok: self.expr(rhs).end_tok,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            }),
            ..Default::default()
        })
    }

    fn expr_postfix_around(&mut self, op: Token, lhs: u32, rhs: u32, found_end: bool) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(lhs).tok,
            end_tok: self.expr(rhs).end_tok + if found_end { 1 } else { 0 },
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            }),
            ..Default::default()
        })
    }

    fn expr_prefix(&mut self, op: Token, rhs: u32) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    fn expr_postfix(&mut self, op: Token, rhs: u32) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok,
            end_tok: self.expr(rhs).end_tok + 1,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    fn expr_around(&mut self, op: Token, rhs: u32, found_end: bool) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok + if found_end { 1 } else { 0 },
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
            ..Default::default()
        })
    }

    fn expr_if(
        &mut self,
        cond: u32,
        body: u32,
        else_expr: Option<u32>,
        is_elif: bool,
    ) -> u32 {
        let if_struct = If { cond, body, else_expr };

        self.expr_push(Expr {
            tok: self.expr(cond).tok - 1,
            end_tok: self.expr(body).end_tok,
            variant: if is_elif {
                ExprVariant::Elif(if_struct)
            } else {
                ExprVariant::If(if_struct)
            },
            ..Default::default()
        })
    }

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

    fn parse_if(&mut self, tokens: &mut Tokens) -> u32 {
        let set_else_arm = |
            ast: &mut AST,
            target: Option<u32>,
            else_expr: u32,
        | {
            if let Some(target) = target {
                match &mut ast.exprs[target as usize].variant {
                    ExprVariant::If(expr_ref)
                    | ExprVariant::Elif(expr_ref) => {
                        expr_ref.else_expr = Some(else_expr);
                    }
                    _ => panic!("target should always be if or elif"),
                }
            }
        };
        
        let mut ret = 0;

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
            let if_expr = self.expr_if(
                cond,
                body,
                None,
                is_elif,
            );

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

                    if self.expect_sequence(tokens, &[TokenType::End, TokenType::If]).is_err() {
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
    fn parse_function(&mut self, tokens: &mut Tokens) -> u32 {
        let tok_idx = tokens.idx();

        tokens.next();

        let tok = tokens.peek();

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

            // prevent consumption of Eq after return type (if it is present)

            let return_type = ast.parse_expr(tokens);

            // "do" token will precede function body

            if ast.expect(tokens, TokenType::Do).is_err() {
                return (false, params, return_type, ast.expr_unit(tokens.idx()));
            }

            // prevent consumption of semicolon if there is one after function
            // body since this could lead to e.g. a lot of the rest of the
            // file being consumed as part of func body

            let body = ast.parse_expr_bp(
                tokens,
                match operator::get_bp(TokenType::Semicolon, Infix) {
                    Some((l_bp, _)) => l_bp + 1,
                    _ => panic!("semicolon is not infix op?"),
                },
            );

            // if body of function is block expr check for desired terminating token
            // (name in case of named function, otherwise keyword "function")

            let success = if let ExprVariant::Operation(Operation {
                op: TokenType::Begin,
                ..
            }) = ast.expr(body).variant
            {
                ast.expect(tokens, TokenType::Function).is_ok()
            } else {
                true
            };

            (success, params, return_type, body)
        };

        let (success, params, return_type, body) = attempt_parse_func(self, tokens);

        // if problem occurred during function parsing try to
        // recover by skipping ahead to one of these ttypes

        if !success {
            let found = tokens.sync(&[TokenType::End, TokenType::Function, TokenType::Semicolon]);

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

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tokens.idx(), // TODO: correct
            variant: ExprVariant::FunctionLiteral(FunctionLiteral {
                params: params,
                return_type: return_type,
                body: body,
                ..Default::default()
            }),
            ..Default::default()
        })
    }

    fn parse_type_literal(&mut self, tokens: &mut Tokens) -> u32 {
        let tok_idx = tokens.idx();
        let type_variant_tok = tokens.next();

        let variant = match type_variant_tok.ttype {
            TokenType::KWInteger => TypeVariant::Integer,
            TokenType::KWFloat => TypeVariant::Float,
            TokenType::Record => TypeVariant::Record,
            TokenType::Enum => TypeVariant::Enum,
            TokenType::Variant => TypeVariant::Variant,
            _ => panic!("unexpected ttype for beginning of type literal"),
        };

        let name = {
            let tok = tokens.peek();

            match tok.ttype {
                TokenType::Identifier => {
                    tokens.next();
                    Some(tok)
                }
                _ => None,
            }
        };

        let _ = tokens.expect(TokenType::Eq);

        // don't want to consume semicolon as part of body

        let body = self.parse_expr_bp(
            tokens,
            match operator::get_bp(TokenType::Semicolon, Infix) {
                Some((l_bp, _)) => l_bp + 1,
                _ => panic!("semicolon is not infix op?"),
            },
        );

        // if body is a block check for correct terminating token

        if let ExprVariant::Operation(Operation {
            op: TokenType::Begin,
            ..
        }) = self.expr(body).variant
        {
            if let Some(name_tok) = name {
                let result = self.expect(tokens, TokenType::Identifier);

                if let Ok(t) = result {
                    // compare start, end function names
                    let _ = self.test_name_match(tokens, name_tok, t);
                }
            } else {
                let _ = self.expect(tokens, type_variant_tok.ttype).is_ok();
            }
        }

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tokens.idx(),
            variant: ExprVariant::TypeLiteral(TypeLiteral {
                name,
                variant,
                body,
            }),
            ..Default::default()
        })
    }

    // atom e.g. literal like integer
    fn parse_atom(&mut self, tokens: &mut Tokens) -> u32 {
        let tok_idx = tokens.idx();
        let tok = tokens.peek();

        match tok.ttype {
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
                            variant: IdentifierVariant::Unknown,
                        }),
                        _ => panic!("single token literal parsing broken"),
                    },
                    ..Default::default()
                })
            }
            TokenType::If => self.parse_if(tokens),
            TokenType::Function => self.parse_function(tokens),
            // One of these tokens indicates type literal
            TokenType::KWInteger
            | TokenType::KWFloat
            | TokenType::Record
            | TokenType::Enum
            | TokenType::Variant => self.parse_type_literal(tokens),
            // if no atom or other (e.g. prefix) expr is found return Unit
            _ => self.expr_unit(tok_idx),
        }
    }

    fn parse_lhs(&mut self, tokens: &mut Tokens) -> u32 {
        let tok = tokens.peek();

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

        match operator::get_bp(tok.ttype, Around) {
            Some(_) => {
                tokens.next();

                let rhs = self.parse_expr(tokens);

                let mut found_end = false;

                match tokens.expect(match tok.ttype {
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

                return self.expr_around(tok, rhs, found_end);
            }
            _ => {}
        }

        self.parse_atom(tokens)
    }

    // bp is Binding Power (Pratt parsing) and ret is expr id
    fn parse_expr_bp(&mut self, tokens: &mut Tokens, min_bp: u8) -> u32 {
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
    fn parse_expr(&mut self, tokens: &mut Tokens) -> u32 {
        self.parse_expr_bp(tokens, 0)
    }

    pub fn expr_to_string(&self, tokens: &Tokens, expr: u32) -> String {
        if expr >= self.exprs.len() as u32 {
            return "".to_string();
        }

        let expr_ref = self.expr(expr);

        match &expr_ref.variant {
            ExprVariant::Unit => "Unit".to_string(),
            ExprVariant::IntegerLiteral(i) => i.to_string(),
            ExprVariant::FloatLiteral(f) => f.to_string(),
            ExprVariant::StringLiteral(t) => tokens.tok_as_str(&t).to_string(),
            ExprVariant::Identifier(ident) => tokens.tok_as_str(&ident.name).to_string(),
            ExprVariant::Operation(operation) => {
                let operand_to_string = |operand: Option<u32>| match operand {
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
                    "(defun {}{} => {} = {})",
                    match function_literal.name {
                        Some(name) => format!("{} ", tokens.tok_as_str(&name),),
                        None => "".to_string(),
                    },
                    self.expr_to_string(tokens, function_literal.params),
                    self.expr_to_string(tokens, function_literal.return_type),
                    self.expr_to_string(tokens, function_literal.body),
                )
            }
            ExprVariant::TypeLiteral(type_literal) => {
                format!(
                    "(deftype {:?} {}{})",
                    type_literal.variant,
                    match type_literal.name {
                        Some(name) => format!("{} ", tokens.tok_as_str(&name),),
                        None => "".to_string(),
                    },
                    self.expr_to_string(tokens, type_literal.body),
                )
            }
            _ => "ERR".to_string(),
        }
    }

    pub fn do_syntax_analysis(&mut self, tokens: &mut Tokens) {
        self.root_expr = Some(self.parse_expr(tokens));
    }
}
