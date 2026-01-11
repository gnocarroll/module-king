pub mod ast_contents;
mod errors;
pub mod operator;
mod semantic;
mod syntax;

use std::{collections::HashMap, fmt::Formatter};

use crate::{
    parse::{
        ast_contents::{ASTContents, ExprID, MemberID, PatternID, ScopeID, TypeID},
        errors::{ParseError, SemanticError},
    },
    scan::{Token, TokenType},
    tokens::{ExpectedToken, TokenOrString, Tokens},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypeVariant {
    Error, // e.g. type could not be determined, other problem
    Unit,
    String,
    Integer,
    Float,
    Record,  // class keyword can also be used for this
    Enum,    // simple C enum
    Variant, // tagged union (can have fields which are just tag)

    // useful for certain code to have this here, but Boolean
    // will actually just be a built-in Enum
    Boolean,
}

#[derive(Clone)]
pub struct TypeLiteral {
    // may be empty str if type literal does not provide name
    pub variant: TypeVariant,

    // body is an Expr
    pub body: ExprID,
}

#[derive(Clone)]
pub enum Type {
    // link to scope containing type information e.g. members
    Scope(ScopeID),

    // u32 is type id
    Ref(TypeID),
    Ptr(TypeID),

    // alias is like C/C++ typedef where compiler will not recognized it as
    // actually separate type
    Alias(TypeID),

    // lhs is type, rhs is (optional) expr
    // e.g. [Integer; 5] -> Integer, 5
    Slice((TypeID, TypeID)),

    // for tuple with > 2 elements second u32 will link to a RestOfTuple
    Tuple((TypeID, Option<TypeID>)),

    // use this inside above Tuple to indicate later pieces of it like
    // a linked list
    RestOfTuple((TypeID, TypeID)),

    // args, ret type (args can be tuple)
    Function((TypeID, TypeID)),
}

#[derive(Clone, Copy)]
pub enum ScopeVariant {
    Scope, // e.g. scope for a for loop or other block
    Module,
    Type(TypeVariant),
}

#[derive(Clone, Default)]
pub struct FunctionLiteral {
    pub name: Option<Token>,
    pub params: ExprID,

    // return type is an expr id rather than Type struct
    pub return_type: ExprID,
    pub body: ExprID,

    // for semantic analysis record pattern id for each param
    pub param_info: Vec<PatternID>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Member, // e.g. for point.x would be "x"
}

#[derive(Clone, Copy)]
pub struct Identifier {
    pub name: Token,

    // during semantic analysis connect to actual corresponding Member
    pub member_id: MemberID,

    pub variant: IdentifierVariant,
}

#[derive(Clone, Copy)]
pub struct Operation {
    pub op: TokenType,
    pub operand1: Option<ExprID>,
    pub operand2: Option<ExprID>,
}

#[derive(Clone)]
pub struct If {
    pub cond: ExprID,
    pub body: ExprID,

    // expr to go to if cond is false
    pub else_expr: Option<ExprID>,
}

#[derive(Clone)]
pub enum ExprVariant {
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
}

#[derive(Clone, Copy, PartialEq)]
enum ExprReturns {
    Error,
    Unit,
    Value,
    Type,
    Module,
}

#[derive(Clone)]
pub struct Expr {
    // token indices range for expression (end_tok is not inclusive)
    pub tok: u32,
    pub end_tok: u32,

    // ID of language type (or module if expr is module)
    pub type_or_module: TypeOrModule,

    pub variant: ExprVariant,

    pub expr_returns: ExprReturns,

    // is a variable which implies you can take certain action e.g. take address
    pub is_var: bool,

    // start off false and then set to true if/when semantic analysis is
    // successfully completed for Expr
    pub finalized: bool,
}

impl Default for Expr {
    fn default() -> Self {
        Expr {
            tok: 0,
            end_tok: 0,
            type_or_module: TypeOrModule::default(),
            variant: ExprVariant::Unit,
            expr_returns: ExprReturns::Unit,
            is_var: false,
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
pub enum Visibility {
    Private,
    Export,
    Global,
}

#[derive(Clone, Copy, PartialEq)]
pub enum MemberVariant {
    Module,
    Type,
    Instance,
}

#[derive(Clone)]
pub enum TypeOrModule {
    Type(TypeID),
    Module(ScopeID),
}

impl Default for TypeOrModule {
    fn default() -> Self {
        TypeOrModule::Type(TypeID::default())
    }
}

#[derive(Clone)]
pub struct Member {
    pub name: TokenOrString,
    pub visibility: Visibility,

    pub variant: MemberVariant,

    // instance or type => type
    // if this member is a module => module
    pub type_or_module: TypeOrModule,
}

#[derive(Clone)]
pub enum ScopeRefersTo {
    Type(TypeID),
    Expr(ExprID),
}

// a scope may refer to a scope for a code block or
// - module
// - type (e.g. Integer)
#[derive(Clone)]
pub struct Scope {
    // name may be present in the code but could also be from elsewhere
    // e.g. name of file is name of corresponding module
    pub name: Option<TokenOrString>,
    pub variant: ScopeVariant,

    pub parent_scope: ScopeID,

    // refers to what this scope belongs to e.g. function
    // (if present)
    pub refers_to: Option<ScopeRefersTo>,

    pub members: HashMap<String, MemberID>,
}

#[derive(Clone)]
pub struct Pattern {
    type_id: TypeID,
    variant: PatternVariant,
}

#[derive(Clone)]
pub enum PatternVariant {
    IgnoreOne,                   // _
    IgnoreMultiple,              // ..
    Binding((Token, PatternID)), // e.g. rest @ ..

    // lhs, (optional) rhs
    Tuple((PatternID, Option<PatternID>)),
    RestOfTuple((PatternID, PatternID)),

    Ident(Token),

    // expr id, should be statically computable I guess
    Value(ExprID),

    // for struct simply leave out ignored fields from this
    // data structure
    Struct(
        (
            Option<Token>,     // type name (optional)
            PatternID,         // at least one addition pattern inside struct pattern
            Option<PatternID>, // next pattern
        ),
    ),

    RestOfStruct(
        (
            PatternID,
            PatternID, // next ptr
        ),
    ),

    // same as tuple really but uses []
    Slice((PatternID, Option<PatternID>)),
    RestOfSlice((PatternID, PatternID)),
}

// NOTE: AST is used for syntax and semantic analysis since I think
// it simplifies things
#[derive(Default)]
pub struct AST {
    pub objs: ASTContents,

    pub parse_errors: Vec<ParseError>,
    pub semantic_errors: Vec<SemanticError>,

    pub root_expr: Option<ExprID>,
}

impl AST {
    pub fn has_errors(&self) -> bool {
        self.parse_errors.len() > 0 || self.semantic_errors.len() > 0
    }

    pub fn display_parse_errors(&self, tokens: &Tokens) {
        for err in &self.parse_errors {
            match err {
                ParseError::ExpectedToken(ExpectedToken { expected, found }) => {
                    eprintln!(
                        "Ln {}, Col {}: expected {}, found {} instead",
                        found.line, found.column, expected, found.ttype,
                    );
                }
                _ => (),
            };
        }
    }

    pub fn display_semantic_errors(&self, tokens: &Tokens) {
        for err in &self.semantic_errors {
            match err {
                SemanticError::InvalidOperation(invalid_op) => {
                    eprintln!("invalid op");
                }
                _ => {
                    eprintln!("Displaying not implemented for this kind of semantic error.")
                }
            }
        }
    }
}

// public function to perform syntactic + semantic analysis
pub fn parse_file(file_name: &str, tokens: &mut Tokens) -> AST {
    let mut ast = AST::default();

    // call to parse_expr does syntactic analysis

    ast.do_syntax_analysis(tokens);

    if let Some(expr) = ast.root_expr {
        println!("{}", ast.expr_to_string(&tokens, expr));
    }

    if ast.has_errors() {
        eprintln!("One or more syntax errors occurred, program will not be compiled.");

        ast.display_parse_errors(&tokens);

        return ast;
    }

    // reenable later to test
    ast.do_semantic_analysis(&tokens, file_name);

    if ast.has_errors() {
        eprintln!("One or more semantic errors occurred, program will not be compiled.");

        ast.display_semantic_errors(&tokens);

        return ast;
    }

    ast
}
