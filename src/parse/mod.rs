mod errors;
pub mod operator;

use std::{collections::HashMap};

use crate::{parse::errors::NameMismatch, scan::{Token, TokenType}};
use errors::{ParseError, SemanticError, ExpectedToken};

#[derive(Clone, Copy, PartialEq)]
enum TypeVariant {
    Integer,
    Float,
    Struct, // class keyword can also be used for this
    Enum, // simple C enum
    Variant, // tagged union (can have fields which are just tag)
}

struct TypeLiteral {
    // may be empty str if type literal does not provide name
    pub name: Option<Token>,
    
    pub variant: TypeVariant,

    // body is an Expr
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeVariant {
    Scope, // e.g. scope for a for loop or other block
    Module,
    Type,
}

struct FunctionLiteral {
    pub name: Option<Token>,
    pub params: u32,
    pub body: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Member, // e.g. for point.x would be "x"
}

struct Identifier {
    pub name: Token,

    pub variant: IdentifierVariant,
}

struct Operation {
    pub op: TokenType,
    pub operand1: Option<u32>,
    pub operand2: Option<u32>,
}

enum ExprVariant {
    Unit,
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(Token),

    Identifier(Identifier),

    Operation(Operation),

    FunctionLiteral(FunctionLiteral),

    TypeLiteral(TypeLiteral),
}

struct Expr {
    // token indices range for expression (end_tok is not inclusive)
    pub tok: u32,
    pub end_tok: u32,

    // ID of language type
    pub etype: u32,

    pub variant: ExprVariant,
}

impl Expr {
    pub fn is_unit(&self) -> bool {
        match self.variant {
            ExprVariant::Unit => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Visibility {
    Private,
    Export,
    Global,
}

#[derive(Clone, Copy, PartialEq)]

enum MemberVariant {
    Module,
    Type,
    Instance,
}

struct Member {
    pub name: Token,
    pub visibility: Visibility,

    pub variant: MemberVariant,
    
    // this refers to:
    // - the scope which this member owns if it is a module or type
    // - the type of this member if this member is an instance
    pub module_or_type: u32,
}

// a scope may refer to a scope for a code block or
// - module
// - type (e.g. Integer)
struct Scope {
    pub name: Option<Token>,
    pub variant: ScopeVariant,

    pub parent_scope: u32,

    pub members: HashMap<Token, u32>,
}

struct Tokens<'a> {
    file_str: &'a str,
    tokens: &'a Vec<Token>,

    // token not file offset
    offset: usize,
}

impl<'a> Tokens<'a> {
    fn new(file_str: &'a str, tokens: &'a Vec<Token>) -> Self {
        Tokens{
            file_str,
            tokens: tokens,
            offset: 0,
        }
    }

    // Token -> &str
    pub fn tok_as_str(&self, token: &Token) -> &str {
        token.as_str(self.file_str)
    }

    pub fn idx(&self) -> u32 {
        self.offset as u32
    }

    // 0-indexed (e.g. 0 is the same as just calling peek())
    fn _peek_nth(&self, idx: isize) -> Token {
        let mut idx = idx + (self.offset as isize);

        if idx < 0 {
            idx = 0;
        }

        match self.tokens.get(idx as usize) {
            Some(t) => *t,
            None => match self.tokens.last() {
                Some(t) => *t,
                None => Token::default(),
            }
        }
    }
    
    pub fn peek_nth(&self, idx: usize) -> Token {
        self._peek_nth(idx as isize)
    }

    pub fn peek(&self) -> Token {
        self.peek_nth(0)
    }

    pub fn next(&mut self) -> Token {
        if self.offset < self.tokens.len() - 1 {
            self.offset += 1;
        }

        self._peek_nth(-1)
    }

    pub fn expect(&mut self, ttype: TokenType) -> Result<Token, ExpectedToken> {
        let maybe_ret = self.peek();

        if maybe_ret.ttype != ttype {
            return Err(ExpectedToken{
                expected: ttype,
                found: maybe_ret,
            })
        }

        self.next();

        Ok(maybe_ret)
    }

    // advance until you find one of the provided token types (or EOF)
    // but DO NOT consume it, so ret will be peek()
    pub fn sync(&mut self, ttypes: &[TokenType]) -> Token {
        loop {
            let tok = self.peek();

            if
                tok.ttype == TokenType::Eof ||
                ttypes.iter().any(|ttype| *ttype == tok.ttype)
            {
                return tok;
            }

            self.next();
        }
    }
}

// NOTE: AST is used for syntax and semantic analysis since I think
// it simplifies things
#[derive(Default)]
pub struct AST {
    // exprs used in initial parsing and semantic analysis stage
    exprs: Vec<Expr>,

    // scopes, members not used until semantic analysis stage
    scopes: Vec<Scope>,
    members: Vec<Member>,

    parse_errors: Vec<ParseError>,
    semantic_errors: Vec<SemanticError>,
}

use operator::OperatorVariant::*;

impl AST {
    fn expr(&self, expr: u32) -> &Expr {
        &self.exprs[expr as usize]
    }

    fn expr_unit(&mut self, tok_idx: u32) -> u32 {
        self.expr_push(Expr{
            tok: tok_idx,
            end_tok: tok_idx,
            etype: 0,
            variant: ExprVariant::Unit,
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
            etype: 0,
            variant: ExprVariant::Operation(Operation{
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            })
        })
    }

    fn expr_postfix_around(
        &mut self,
        op: Token,
        lhs: u32,
        rhs: u32,
        found_end: bool,
    ) -> u32 {
        self.expr_push(Expr {
            tok: self.expr(lhs).tok,
            end_tok: self.expr(rhs).end_tok + if found_end {
                1
            } else { 0 },
            etype: 0,
            variant: ExprVariant::Operation(Operation{
                op: op.ttype,
                operand1: Some(lhs),
                operand2: Some(rhs),
            })
        })
    }

    fn expr_prefix(&mut self, op: Token, rhs: u32) -> u32 {     
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok,
            etype: 0,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
        })
    }

    fn expr_postfix(&mut self, op: Token, rhs: u32) -> u32 {     
        self.expr_push(Expr {
            tok: self.expr(rhs).tok,
            end_tok: self.expr(rhs).end_tok + 1,
            etype: 0,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
        })
    }

    fn expr_around(
        &mut self,
        op: Token,
        rhs: u32,
        found_end: bool,
    ) -> u32 {     
        self.expr_push(Expr {
            tok: self.expr(rhs).tok - 1,
            end_tok: self.expr(rhs).end_tok + if found_end {
                1
            } else {
                0
            },
            etype: 0,
            variant: ExprVariant::Operation(Operation {
                op: op.ttype,
                operand1: Some(rhs),
                operand2: None,
            }),
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
    fn expect_sequence(&mut self, tokens: &mut Tokens, ttypes: &[TokenType]) -> Result<Token, ExpectedToken> {
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
        }
        else {
            let ret = NameMismatch{
                expected: expected,
                found: found,
            };

            self.parse_errors.push(ParseError::NameMismatch(ret));

            Err(ret)
        }
    }

    // NOTE: this function assumes "function" is upcoming token so that should
    // have been checked at call site
    fn parse_function(&mut self, tokens: &mut Tokens) -> u32 {
        let tok_idx = tokens.idx();        

        tokens.next();

        let tok = tokens.peek();

        let name = match tok.ttype {
            TokenType::Identifier => {
                tokens.next();
                Some(tok)
            },
            _ => None,
        };

        // having this in lambda makes it easy to jump ahead if failure occurs
        // it is Temu goto
        let attempt_parse_func = |
            ast: &mut AST,
            tokens: &mut Tokens,
            name: Option<Token>,
        | {
            if ast.expect(tokens, TokenType::LParen).is_err() {
                return (
                    false,
                    ast.expr_unit(tokens.idx()),
                    ast.expr_unit(tokens.idx()),
                );
            }

            let params = ast.parse_expr(tokens);

            if ast.expect(tokens, TokenType::RParen).is_err() {
                return (false, params, ast.expr_unit(tokens.idx()));
            }

            // optional "=>" before return type

            if tokens.peek().ttype == TokenType::DArrow {
                tokens.next();
            }

            let ret_type = ast.parse_expr(tokens);


            let body = ast.parse_expr(tokens);

            if ast.expect(tokens, TokenType::End).is_err() {
                return (false, params, body);
            }

            let success = if let Some(name_tok) = name {
                let result = ast.expect(
                    tokens,
                    TokenType::Identifier,
                );

                if let Ok(t) = result {
                    // compare start, end function names
                    let _ = ast.test_name_match(tokens, name_tok, t);
                }

                result.is_ok()
            }
            else {
                ast.expect(tokens, TokenType::Function).is_ok()
            };

            (success, params, body)
        };

        let (success, params, body) = attempt_parse_func(
            self,
            tokens,
            name,
        );

        // if problem occurred during function parsing try to
        // recover by skipping ahead to one of these ttypes

        if !success {
            let found = tokens.sync(&[
                TokenType::End,
                TokenType::Function,
                TokenType::Semicolon,
            ]);

            match found.ttype {
                TokenType::End => {
                    tokens.next();

                    match tokens.peek().ttype {
                        TokenType::Function | TokenType::Identifier => {
                            tokens.next();
                        },
                        _ => (),
                    };
                },
                TokenType::Function => {
                    tokens.next();
                },
                _ => (),
            };
        }

        self.expr_push(Expr {
            tok: tok_idx,
            end_tok: tokens.idx(), // TODO: correct
            etype: 0,
            variant: ExprVariant::FunctionLiteral(FunctionLiteral {
                name: name,
                params: params,
                body: body,
            })
        })
    }

    // atom e.g. literal like integer
    fn parse_atom(&mut self, tokens: &mut Tokens) -> u32 {
        let tok_idx = tokens.idx();
        let tok = tokens.peek();

        match tok.ttype {
            // single token (e.g. integer) literals or ident
            TokenType::Integer | TokenType::Float |
            TokenType::String | TokenType::Identifier => {
                tokens.next();

                self.expr_push(Expr {
                    tok: tok_idx,
                    end_tok: tok_idx + 1,
                    etype: 0,
                    variant: match tok.ttype {
                        TokenType::Integer => ExprVariant::IntegerLiteral(
                            tokens.tok_as_str(&tok).parse::<u64>().expect(
                                "integer scanning or getting token text is broken",
                            ),
                        ),
                        TokenType::Float => ExprVariant::FloatLiteral(
                            tokens.tok_as_str(&tok).parse::<f64>().expect(
                                "float scanning or getting token text is broken",
                            ),
                        ),
                        TokenType::String => ExprVariant::StringLiteral(tok),
                        TokenType::Identifier => ExprVariant::Identifier(
                            Identifier {
                                name: tok,
                                variant: IdentifierVariant::Unknown,
                            }
                        ),
                        _ => panic!("single token literal parsing broken"),
                    }
                })
            },
            TokenType::Function => self.parse_function(tokens),
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

                return self.expr_prefix(
                    tok,
                    rhs,
                );
            },
            _ => {},
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
                    _ => panic!("unexpected ttype for Around"),
                }) {
                    Ok(_) => {
                        found_end = true;
                    },
                    Err(e) => {
                        self.parse_errors.push(
                            ParseError::ExpectedToken(e),
                        );
                    },
                }

                return self.expr_around(tok, rhs, found_end);
            },
            _ => {},
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
                    if let Some((l_bp, r_bp)) = operator::get_bp(
                        op.ttype,
                        op_variant,
                    ) {
                        return Some((op_variant, l_bp, r_bp));
                    }
                }

                None
            };

            if let Some((op_variant, l_bp, r_bp)) = determine_op_variant_and_bp() {
                if l_bp < min_bp { // applies to infix and postfix
                    break;
                }
                
                tokens.next();

                // how lhs is replaced depends on op variant

                match op_variant {
                    Infix => {
                        let rhs = self.parse_expr_bp(tokens, r_bp);

                        lhs = self.expr_infix(
                            op,
                            lhs,
                            rhs,
                        );
                    },
                    Postfix => {
                        lhs = self.expr_postfix(op, lhs);
                    },
                    PostfixAround => { // e.g. function call
                        let rhs = self.parse_expr(tokens);

                        let mut found_end = false;

                        match tokens.expect(match op.ttype {
                            TokenType::LParen => TokenType::RParen,
                            TokenType::LBrace => TokenType::RBrace,
                            _ => panic!("unexpected ttype for Around"),
                        }) {
                            Ok(_) => {
                                found_end = true;
                            },
                            Err(e) => {
                                self.parse_errors.push(
                                    ParseError::ExpectedToken(e),
                                );
                            },
                        }

                        lhs = self.expr_postfix_around(
                            op,
                            lhs,
                            rhs,
                            found_end,
                        );
                    },
                    _ => panic!("UNEXPECTED OP VARIANT"),
                }
            }
            else {
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
            ExprVariant::Unit => "()".to_string(),
            ExprVariant::IntegerLiteral(i) => i.to_string(),
            ExprVariant::FloatLiteral(f) => f.to_string(),
            ExprVariant::StringLiteral(t) => {
                tokens.tok_as_str(&t).to_string()
            },
            ExprVariant::Identifier(ident) => {
                tokens.tok_as_str(&ident.name).to_string()
            },
            ExprVariant::Operation(operation) => {
                let operand_to_string = |operand: Option<u32>| {
                    match operand {
                        Some(id) => format!(
                            " {}",
                            self.expr_to_string(tokens, id),
                        ),
                        None => "".to_string()
                    }
                };
                
                format!(
                    "({}{}{})",
                    operation.op,
                    operand_to_string(operation.operand1),
                    operand_to_string(operation.operand2),
                )
            },
            ExprVariant::FunctionLiteral(function_literal) => {
                format!(
                    "(defun {}({}) {})",
                    match function_literal.name {
                        Some(name) => format!(
                            "{} ",
                            tokens.tok_as_str(&name),
                        ),
                        None => "".to_string(),
                    },
                    self.expr_to_string(tokens, function_literal.params),
                    self.expr_to_string(tokens, function_literal.body),
                )
            },
            _ => "ERR".to_string(),
        }
    }
}

// public function to perform syntactic + semantic analysis
pub fn parse_file(file_str: &str, tokens: &Vec<Token>) -> AST {
    let mut ast = AST::default();
    let mut tokens = Tokens::new(file_str, tokens);

    // call to parse_expr does syntactic analysis

    let root = ast.parse_expr(&mut tokens);

    println!("{}", ast.expr_to_string(&tokens, root));

    // TODO: semantic analysis

    // ...

    ast
}