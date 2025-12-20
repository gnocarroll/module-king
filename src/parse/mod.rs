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

struct TypeLiteral<'a> {
    // may be empty str if type literal does not provide name
    pub name: Option<&'a str>,
    
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

struct FunctionLiteral<'a> {
    pub name: Option<&'a str>,
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

struct Identifier<'a> {
    pub name: &'a str,

    pub variant: IdentifierVariant,
}

struct Operation {
    pub op: TokenType,
    pub operand1: Option<u32>,
    pub operand2: Option<u32>,
}

enum ExprVariant<'a> {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(&'a str),

    Identifier(Identifier<'a>),

    Operation(Operation),

    FunctionLiteral(FunctionLiteral<'a>),

    TypeLiteral(TypeLiteral<'a>),
}

struct Expr<'a> {
    pub tokens: &'a [Token],

    // ID of language type
    pub etype: u32,

    pub variant: ExprVariant<'a>,
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

struct Member<'a> {
    pub name: &'a str,
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
struct Scope<'a> {
    pub name: Option<&'a str>,
    pub variant: ScopeVariant,

    pub parent_scope: u32,

    pub members: HashMap<&'a str, u32>,
}

struct Tokens<'a> {
    tokens: &'a Vec<Token>,
    offset: usize,
}

impl<'a> Tokens<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        Tokens{
            tokens: tokens,
            offset: 0,
        }
    }

    // 0-indexed (e.g. 0 is the same as just calling peek())
    fn _peek_nth(&self, idx: isize) -> Token {
        let mut idx = idx + (self.offset as isize);

        if idx < 0 {
            idx = 0;
        }

        // cost of cloning is ACCEPTABLE fr

        match self.tokens.get(idx as usize) {
            Some(t) => t.clone(),
            None => match self.tokens.last() {
                Some(t) => t.clone(),
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
pub struct AST<'a> {
    // exprs used in initial parsing and semantic analysis stage
    exprs: Vec<Expr<'a>>,

    // scopes, members not used until semantic analysis stage
    scopes: Vec<Scope<'a>>,
    members: Vec<Member<'a>>,

    parse_errors: Vec<ParseError>,
    semantic_errors: Vec<SemanticError>,
}

// public function to perform syntactic + semantic analysis
pub fn parse_file<'a>(tokens: &Vec<Token>) -> AST<'a> {
    let ast = AST::default();
    let tokens = Tokens::new(tokens);


    ast
}

impl AST<'_> {
    
}