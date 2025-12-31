mod errors;
pub mod operator;
mod semantic;
mod syntax;

use std::collections::HashMap;

use crate::{
    parse::errors::{ExpectedToken, ParseError, SemanticError},
    scan::{Token, TokenType},
};

#[derive(Clone, Copy, Debug, PartialEq)]
enum TypeVariant {
    Error, // e.g. type could not be determined, other problem
    Unit,
    String,
    Integer,
    Float,
    Record,  // class keyword can also be used for this
    Enum,    // simple C enum
    Variant, // tagged union (can have fields which are just tag)
}

#[derive(Clone)]
struct TypeLiteral {
    // may be empty str if type literal does not provide name
    pub variant: TypeVariant,

    // body is an Expr
    pub body: u32,
}

#[derive(Clone)]
enum Type {
    // link to scope containing type information e.g. members
    Scope(u32),

    // u32 is type id
    Ref(u32),
    Ptr(u32),

    // lhs is type, rhs is (optional) expr
    // e.g. [Integer; 5] -> Integer, 5
    Slice((u32, u32)),

    // for tuple with > 2 elements second u32 will link to a RestOfTuple
    Tuple((u32, Option<u32>)),

    // use this inside above Tuple to indicate later pieces of it like
    // a linked list
    RestOfTuple((u32, u32)),

    // args, ret type (args can be tuple)
    Function((u32, u32)),
}

#[derive(Clone, Copy)]
enum ScopeVariant {
    Scope, // e.g. scope for a for loop or other block
    Module,
    Type(TypeVariant),
}

// destructuring from given type using a certain pattern
// e.g. to pattern (x, y) from type (u32, u32)
#[derive(Clone)]
struct Destructure {
    // pattern i.d. (see Pattern struct)
    dest_pattern: u32,

    // type id
    src_type: u32,
}

#[derive(Clone, Default)]
struct FunctionLiteral {
    pub name: Option<Token>,
    pub params: u32,

    // return type is an expr id rather than Type struct
    pub return_type: u32,
    pub body: u32,

    // for semantic analysis stage more expressions will be added
    // for ironed-out params
    pub params_start: u32,
    pub params_stop: u32,
}

#[derive(Clone, Copy, PartialEq)]
enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Member, // e.g. for point.x would be "x"
}

#[derive(Clone, Copy)]
struct Identifier {
    pub name: Token,

    pub variant: IdentifierVariant,
}

#[derive(Clone, Copy)]
struct Operation {
    pub op: TokenType,
    pub operand1: Option<u32>,
    pub operand2: Option<u32>,
}

#[derive(Clone)]
struct If {
    pub cond: u32,
    pub body: u32,

    // expr to go to if cond is false
    pub else_expr: Option<u32>,
}

#[derive(Clone)]
enum ExprVariant {
    Unit,
    Underscore,
    KWType, // the keyword "type" e.g. type(x)
    DollarNumber(u64),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(Token),

    Identifier(Identifier),

    Operation(Operation),

    // without separate categories then these would be recorded the same
    // if ... else if
    // if ... elif
    // this lang has separate elif like Python rather than e.g. C where it is
    // really just all if and else case
    If(If),
    Elif(If),

    FunctionLiteral(FunctionLiteral),

    TypeLiteral(TypeLiteral),

    Destructure(Destructure),
}

#[derive(Clone, Copy, PartialEq)]
enum ExprReturns {
    Unit,
    Value,
    Type,
    Module,
}

#[derive(Clone)]
struct Expr {
    // token indices range for expression (end_tok is not inclusive)
    pub tok: u32,
    pub end_tok: u32,

    // ID of language type (or module if expr is module)
    pub type_or_module: u32,

    pub variant: ExprVariant,

    pub expr_returns: ExprReturns,

    // start off false and then set to true if/when semantic analysis is
    // successfully completed for Expr
    pub finalized: bool,
}

impl Default for Expr {
    fn default() -> Self {
        Expr {
            tok: 0,
            end_tok: 0,
            type_or_module: 0,
            variant: ExprVariant::Unit,
            expr_returns: ExprReturns::Unit,
            finalized: false,
        }
    }
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

#[derive(Clone)]
enum TokenOrString {
    Token(Token),
    String(String),
}

struct Member {
    pub name: TokenOrString,
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
    // name may be present in the code but could also be from elsewhere
    // e.g. name of file is name of corresponding module
    pub name: Option<TokenOrString>,
    pub variant: ScopeVariant,

    pub parent_scope: u32,

    // refers to what this scope belongs to e.g. function
    // (if present)
    pub refers_to: Option<u32>,

    pub members: HashMap<String, u32>,
}

#[derive(Clone)]
enum Pattern {
    Missing,               // piece of pattern that should be there but is not
    IgnoreOne,             // _
    IgnoreMultiple,        // ..
    Binding((Token, u32)), // e.g. rest @ ..

    // lhs, (optional) rhs
    Tuple((u32, Option<u32>)),
    RestOfTuple((u32, u32)),

    Ident(Token),

    // expr id, should be statically computable I guess
    Value(u32),

    // for struct simply leave out ignored fields from this
    // data structure
    Struct(
        (
            Option<Token>, // type name (optional)
            u32, // at least one addition pattern inside struct pattern
            Option<u32>, // next pattern
        ),
    ),

    RestOfStruct((
        u32,
        u32, // next ptr
    )),

    // same as tuple really but uses []
    Slice((u32, Option<u32>)),
    RestOfSlice((u32, u32)),
}

struct Tokens<'a> {
    file_str: &'a str,
    tokens: &'a Vec<Token>,

    // token not file offset
    offset: usize,
}

impl<'a> Tokens<'a> {
    fn new(file_str: &'a str, tokens: &'a Vec<Token>) -> Self {
        Tokens {
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
            },
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
            return Err(ExpectedToken {
                expected: ttype,
                found: maybe_ret,
            });
        }

        self.next();

        Ok(maybe_ret)
    }

    // advance until you find one of the provided token types (or EOF)
    // but DO NOT consume it, so ret will be peek()
    pub fn sync(&mut self, ttypes: &[TokenType]) -> Token {
        loop {
            let tok = self.peek();

            if tok.ttype == TokenType::Eof || ttypes.iter().any(|ttype| *ttype == tok.ttype) {
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

    // exprs are main thing for parsing so e.g. scopes members are for
    // semantic analysis
    scopes: Vec<Scope>,
    types: Vec<Type>,
    members: Vec<Member>,

    patterns: Vec<Pattern>,

    parse_errors: Vec<ParseError>,
    semantic_errors: Vec<SemanticError>,

    root_expr: Option<u32>,
}

// public function to perform syntactic + semantic analysis
pub fn parse_file(file_name: &str, file_str: &str, tokens: &Vec<Token>) -> AST {
    let mut ast = AST::default();
    let mut tokens = Tokens::new(file_str, tokens);

    // call to parse_expr does syntactic analysis

    ast.do_syntax_analysis(&mut tokens);

    if let Some(expr) = ast.root_expr {
        println!("{}", ast.expr_to_string(&tokens, expr));
    }

    // reenable later to test
    // ast.do_semantic_analysis(&tokens, file_name);

    ast
}
