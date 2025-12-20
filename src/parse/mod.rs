mod errors;
pub mod operator;

use std::{collections::HashMap};

use crate::scan::{Token, TokenType};
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

    pub fn expect(&'a mut self, ttype: TokenType) -> Result<Token, ExpectedToken> {
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

    fn parse_lhs(&mut self, tokens: &mut Tokens, min_bp: u8) -> u32 {
        let tok = tokens.peek();

        match tok.ttype {
            TokenType::Integer | TokenType::Float | TokenType::String => {
                let idx = tokens.idx();

                tokens.next();

                self.expr_push(Expr {
                    tok: idx,
                    end_tok: idx + 1,
                    etype: 0,
                    variant: match tok.ttype {
                        TokenType::Integer => ExprVariant::IntegerLiteral(
                            str::parse::<u64>(tokens.tok_as_str(&tok)).expect(
                                "integer scanning or getting token text is broken",
                            )
                        ),
                        TokenType::Float => ExprVariant::FloatLiteral(
                            str::parse::<f64>(tokens.tok_as_str(&tok)).expect(
                                "float scanning or getting token text is broken",
                            )
                        ),
                        TokenType::String => ExprVariant::StringLiteral(tok),
                        _ => panic!("literal parsing broken"),
                    }
                })
            },
            _ => self.expr_unit(),
        }
    }

    // bp is Binding Power (Pratt parsing) and ret is expr id
    fn parse_expr_bp(&mut self, tokens: &mut Tokens, min_bp: u8) -> u32 {
        let mut lhs = self.parse_lhs(tokens, min_bp);

        if self.expr(lhs).is_unit() {
            return lhs;
        }

        loop {
            let op = tokens.peek();

            let (l_bp, r_bp) = match operator::get_bp(op.ttype, Infix) {
                Some(bp) => bp,
                None => break,
            };

            if l_bp < min_bp {
                break;
            }

            tokens.next();

            let rhs = self.parse_expr_bp(tokens, r_bp);

            lhs = self.expr_infix(
                op,
                lhs,
                rhs,
            );
        }

        lhs
    }

    // ret expr id
    fn parse_expr(&mut self, tokens: &mut Tokens) -> u32 {
        self.parse_expr_bp(tokens, 0)
    }
}

// public function to perform syntactic + semantic analysis
pub fn parse_file(file_str: &str, tokens: &Vec<Token>) -> AST {
    let mut ast = AST::default();
    let mut tokens = Tokens::new(file_str, tokens);

    // call to parse_expr does syntactic analysis

    let _root = ast.parse_expr(&mut tokens);

    // TODO: semantic analysis

    // ...

    ast
}