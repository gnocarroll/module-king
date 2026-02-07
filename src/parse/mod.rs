pub mod ast_contents;
pub mod builtin;
mod errors;
pub mod operator;
pub mod scope_members;
mod semantic;
mod syntax;

use std::collections::HashMap;

use crate::{
    parse::{
        ast_contents::{ASTContents, ExprID, FunctionID, MemberID, PatternID, ScopeID, TypeID},
        builtin::Builtin,
        errors::{DuplicateName, ParseError, PatternError, SemanticError},
        scope_members::ScopeMembers,
    },
    scan::{Token, TokenType},
    tokens::{ExpectedToken, TokenOrString, Tokens},
};

// will implement this for some of the IDs to get their file module scope and name
pub trait HasFileModule {
    fn file_module_scope(&self, ast: &AST) -> ScopeID;
    fn get_name(&self, ast: &AST) -> Option<String>;

    fn get_tokens<'a>(&self, ast: &'a AST) -> &'a Tokens {
        let scope_id = self.file_module_scope(ast);

        match &ast.objs.scope(scope_id).variant {
            ScopeVariant::FileModule(info) => &info.tokens,
            _ => {
                panic!("could not get Tokens for HasFileModule");
            }
        }
    }
}

// this is only for the basic types that a user can build on top of
// e.g. a user can create a new Integer type
// it is not for types which will remain truly built-in e.g. Unit
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypeVariant {
    Integer,
    Float,
    Record,  // class keyword can also be used for this
    Enum,    // simple C enum
    Variant, // tagged union (can have fields which are just tag)

    // useful for certain code to have this here, but Boolean
    // will actually just be a built-in Enum
    Boolean,
}

impl Default for TypeVariant {
    fn default() -> Self {
        Self::Integer
    }
}

#[derive(Clone, Default)]
pub struct TypeLiteral {
    // may be empty str if type literal does not provide name
    pub variant: TypeVariant,

    // body is an Expr
    pub body: ExprID,

    // during semantic analysis will connect to corresponding Type
    pub type_id: TypeID,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SliceIndex {
    pub type_id: TypeID,

    pub size: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Error,
    Unit,

    String,

    AnyType,
    AnyModule,

    // is a type as opposed to a value
    // (a value could be e.g. the number 5 w/ type Integer)
    Type(TypeID),

    // is a module
    Module(ScopeID),

    // result of unary from e.g. "from Boolean"
    ImportTarget(ScopeID),

    // link to scope containing type information e.g. members
    // this covers types which are integers, floats, records, etc.
    Scope(ScopeID),

    // u32 is type id
    Ref(TypeID),
    Ptr(TypeID),

    // alias is like C/C++ typedef where compiler will not recognized it as
    // actually separate type
    Alias(TypeID),

    // Type[Expr]
    Slice((TypeID, SliceIndex)),

    // for tuple with > 2 elements second u32 will link to a RestOfTuple
    Tuple((TypeID, Option<TypeID>)),

    // use this inside above Tuple to indicate later pieces of it like
    // a linked list
    RestOfTuple((TypeID, TypeID)),

    // args, ret type (args can be tuple)
    Function((TypeID, TypeID)),

    // dedicated special type to record which builtin function
    Builtin(Builtin),

    // in this initial Rust interpreter will not have full generics
    // however will support lists of a given Type
    // and maps from String -> some Type

    // List of Type
    List(TypeID),

    // Map String -> Type
    Map(TypeID),
}

pub struct TupleIterator<'a> {
    ast: &'a AST,
    type_id: TypeID,
    idx: usize,
    done: bool,
}

// iterate through types in tuple type
impl Iterator for TupleIterator<'_> {
    type Item = TypeID;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        loop {
            match self.ast.objs.type_get(self.type_id) {
                Type::Tuple((t1, maybe_t2)) => {
                    if self.idx == 0 {
                        self.idx += 1;

                        return Some(*t1);
                    } else {
                        self.idx = 0;

                        match maybe_t2 {
                            Some(t2) => {
                                self.type_id = *t2;
                            }
                            None => {
                                self.done = true;

                                return None;
                            }
                        }
                    }
                }
                Type::RestOfTuple((t1, t2)) => {
                    if self.idx == 0 {
                        self.idx += 1;

                        return Some(*t1);
                    } else {
                        self.idx = 0;

                        self.type_id = *t2;
                    }
                }
                _ => {
                    self.done = true;

                    return Some(self.type_id);
                }
            }
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Unit
    }
}

#[derive(Clone)]
pub struct FileModuleInfo {
    pub tokens: Tokens,
    pub root_expr: ExprID,
}

#[derive(Clone)]
pub enum ScopeVariant {
    Scope, // e.g. scope for a for loop or other block
    Module,
    FileModule(FileModuleInfo),
    Type(TypeVariant),
}

#[derive(Clone, Default)]
pub struct FunctionLiteral {
    // this expr is not needed by function struct itself
    // not relevant once semantic analysis is complete
    pub params: ExprID,

    // return type is an expr id rather than Type struct
    // once type is determined it will be stored in
    // Function struct
    pub return_type_expr: ExprID,

    pub function_id: FunctionID,
}

#[derive(Clone, Default)]
pub struct Function {
    pub name: Option<Token>,

    // connect back to literal
    pub literal: ExprID,

    pub return_type: TypeID,

    pub scope: ScopeID,
    pub body: ExprID,

    // overall type of function itself
    pub func_type: TypeID,

    // order in Vec should be order they are listed in program
    pub params: Vec<PatternID>,

    // record exprs corresponding to params to prevent duplicate param insertion
    // (on semantic pass after first)
    pub expr_to_param_idx: HashMap<ExprID, usize>,

    pub file_module: ScopeID,
}

#[derive(Clone, Copy, PartialEq)]
pub enum IdentifierVariant {
    Unknown,
    Module,
    Type,
    Instance,
    Function,
    Member, // e.g. for point.x would be "x"
}

impl Default for IdentifierVariant {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Clone, Copy, Default)]
pub struct Identifier {
    pub name: Token,

    // during semantic analysis connect to actual corresponding Member
    pub member_id: MemberID,

    pub variant: IdentifierVariant,

    // can put expr which corresponds to initial value of ident here
    // e.g. ident := initial_value;
    pub initial_value: Option<ExprID>,
}

#[derive(Clone, Copy)]
pub struct Operation {
    pub op: TokenType,
    pub operand1: Option<ExprID>,
    pub operand2: Option<ExprID>,
}

#[derive(Clone, Default)]
pub struct If {
    pub cond: ExprID,
    pub body: ExprID,

    // expr to go to if cond is false
    pub else_expr: Option<ExprID>,

    pub scope: ScopeID,
}

#[derive(Clone, Default)]
pub struct While {
    pub cond: ExprID,
    pub body: ExprID,

    pub scope: ScopeID,
}

#[derive(Clone, Default)]
pub struct Block {
    pub body: ExprID,

    pub scope: ScopeID,
}

#[derive(Clone)]
pub enum ExprVariant {
    Unit,
    Underscore,
    KWType,   // the keyword "type" e.g. type(x)
    KWModule, // the keyword "module"
    DollarNumber(u64),

    BooleanLiteral(bool),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    CharacterLiteral(char),
    StringLiteral(Vec<u8>),

    Identifier(Identifier),

    Operation(Operation),

    Block(Block),

    // without separate categories then these would be recorded the same
    // if ... else if
    // if ... elif
    // this lang has separate elif like Python rather than e.g. C where it is
    // really just all if and else case
    If(If),
    Elif(If),

    While(While),

    FunctionLiteral(FunctionLiteral),

    TypeLiteral(TypeLiteral),
}

impl Default for ExprVariant {
    fn default() -> Self {
        ExprVariant::Unit
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExprReturns {
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

    // TypeID
    // will also indicate if the expression is a type itself
    // or a module
    pub type_id: TypeID,

    pub variant: ExprVariant,

    // is a variable which implies you can take certain action e.g. take address
    pub is_var: bool,

    // start off false and then set to true if/when semantic analysis is
    // successfully completed for Expr
    pub finalized: bool,

    pub file_module: ScopeID,
}

impl Default for Expr {
    fn default() -> Self {
        Expr {
            tok: 0,
            end_tok: 0,
            type_id: TypeID::default(),
            variant: ExprVariant::Unit,
            is_var: false,
            finalized: false,
            file_module: ScopeID::default(),
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
    Module(ScopeID),
    Type(TypeID),
    Instance(TypeID),
    Function(FunctionID),
    Builtin(Builtin),
}

#[derive(Clone)]
pub struct Member {
    pub name: TokenOrString,
    pub visibility: Visibility,

    pub variant: MemberVariant,

    pub file_module: ScopeID,
}

impl Default for Member {
    fn default() -> Self {
        Member {
            name: TokenOrString::String("".to_string()),
            visibility: Visibility::Private,
            variant: MemberVariant::Instance(TypeID::error()),
            file_module: ScopeID::default(),
        }
    }
}

#[derive(Clone)]
pub enum ScopeRefersTo {
    Type(TypeID),
    Expr(ExprID),
    Function(FunctionID),
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

    pub members: ScopeMembers,

    pub file_module: ScopeID,
}

impl Default for Scope {
    fn default() -> Self {
        Scope {
            name: None,
            variant: ScopeVariant::Scope,
            parent_scope: ScopeID::default(),
            refers_to: None,
            members: ScopeMembers::default(),
            file_module: ScopeID::default(),
        }
    }
}

#[derive(Clone)]
pub struct Pattern {
    type_id: TypeID,
    variant: PatternVariant,
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern {
            type_id: TypeID::error(),
            variant: PatternVariant::IgnoreOne,
        }
    }
}

#[derive(Clone)]
pub enum PatternVariant {
    IgnoreOne,                   // _
    IgnoreMultiple,              // ..
    Binding((Token, PatternID)), // e.g. rest @ ..

    // lhs, (optional) rhs
    Tuple((PatternID, Option<PatternID>)),
    RestOfTuple((PatternID, PatternID)),

    // this will link to expr which should be of the variant Identifier
    // I will prefer to link it to expr rather than just Token to have more info
    // connected here
    Ident(ExprID),

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

    // some other kind of expression is found
    MiscExpr(ExprID),
}

pub struct PatternIterator<'a> {
    ast: &'a AST,
    pattern_stack: Vec<PatternID>,
}

impl Iterator for PatternIterator<'_> {
    type Item = (Token, TypeID);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pattern_id = self.pattern_stack.pop()?;

            let pattern = self.ast.objs.pattern(pattern_id);

            let type_id = pattern.type_id;

            match pattern.variant {
                PatternVariant::IgnoreOne | PatternVariant::IgnoreMultiple => (),
                PatternVariant::Ident(expr) => match &self.ast.objs.expr(expr).variant {
                    ExprVariant::Identifier(ident) => return Some((ident.name, type_id)),
                    _ => {
                        panic!("Ident pattern variant should only have expr w/ variant Identifier")
                    }
                },
                PatternVariant::Tuple((lhs, None)) | PatternVariant::Slice((lhs, None)) => {
                    self.pattern_stack.push(lhs)
                }
                PatternVariant::Tuple((lhs, Some(rhs)))
                | PatternVariant::RestOfTuple((lhs, rhs))
                | PatternVariant::Slice((lhs, Some(rhs)))
                | PatternVariant::RestOfSlice((lhs, rhs)) => {
                    self.pattern_stack.push(rhs);
                    self.pattern_stack.push(lhs);
                }
                _ => (),
            }
        }
    }
}

// NOTE: AST is used for syntax and semantic analysis since I think
// it simplifies things
#[derive(Default)]
pub struct AST {
    pub objs: ASTContents,

    pub parse_errors: Vec<ParseError>,
    pub semantic_errors: Vec<SemanticError>,

    pub curr_file_module: ScopeID,

    // true => doing second pass and everything should be able to be resolved correctly
    // (i.e. finalized)
    pub doing_repair: bool,
}

impl AST {
    pub fn has_errors(&self) -> bool {
        self.parse_errors.len() > 0 || self.semantic_errors.len() > 0
    }

    pub fn display_all_errors(&self) {
        self.display_parse_errors();
        self.display_semantic_errors();
    }

    pub fn display_parse_errors(&self) {
        for err in &self.parse_errors {
            eprintln!("ParseError: {:?}", err);

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

    pub fn display_semantic_errors(&self) {
        for err in &self.semantic_errors {
            eprintln!("{:?}", err);

            match err {
                SemanticError::InvalidOperation(invalid_op) => {
                    eprintln!("EXPR TEXT: {}", self.expr_to_string(invalid_op.operation));
                    eprintln!("MSG: {}", invalid_op.msg);
                }
                SemanticError::PatternError(pattern_err) => match pattern_err {
                    PatternError::TypeMissing(expr) => {
                        eprintln!("TYPE MISSING: {}", self.expr_to_string(*expr));
                    }
                    _ => (),
                },
                SemanticError::InvalidExpr(invalid_expr) => {
                    eprintln!("EXPR TEXT: {}", self.expr_to_string(invalid_expr.expr));
                }
                SemanticError::UnexpectedExpr(unexpected) => {
                    eprintln!("EXPR TEXT: {}", self.expr_to_string(unexpected.expr));
                }
                SemanticError::ExpectedExprReturns(expected_expr_returns) => {
                    eprintln!(
                        "EXPR TEXT: {}\nEXPECTED: {:?}\nFOUND:{:?}",
                        self.expr_to_string(expected_expr_returns.expr),
                        expected_expr_returns.expected,
                        expected_expr_returns.found,
                    );
                }
                _ => {
                    eprintln!("Displaying not implemented for this kind of semantic error.")
                }
            }
        }
    }

    // set curr file module for AST itself and ASTContents struct

    pub fn set_curr_file_module(&mut self, curr_file_module: ScopeID) {
        self.curr_file_module = curr_file_module;

        self.objs.set_curr_file_module(curr_file_module);
    }

    fn add_file_module(
        &mut self,
        tokens: Tokens,
        modulepath: Vec<String>,
    ) -> Result<ScopeID, DuplicateName> {
        let mut scope_id = ScopeID::global();

        // helper function to create new child module

        let create_child_module =
            |ast: &mut AST, insert_to: ScopeID, name: String| -> Result<MemberID, DuplicateName> {
                let new_module_scope_id = ast.objs.scope_push(Scope {
                    name: Some(TokenOrString::String(name.clone())),
                    variant: ScopeVariant::Module,
                    parent_scope: insert_to,
                    ..Default::default()
                });

                // if last then this is the file module itself so set scope properly

                let member_id = ast.objs.member_push(Member {
                    name: TokenOrString::String(name),
                    visibility: Visibility::Export,
                    variant: MemberVariant::Module(new_module_scope_id),
                    ..Default::default()
                });

                ast.scope_try_insert(insert_to, member_id)
            };

        for module_name in &modulepath {
            // if module already exists get it, else use helper function to create it

            scope_id = if let Some(member_id) = self.objs.scope(scope_id).members.get(module_name) {
                match self.objs.member(member_id).variant {
                    MemberVariant::Module(scope_id) => scope_id,
                    _ => {
                        create_child_module(self, scope_id, module_name.clone())?;

                        panic!("err should be guaranteed for prev function call");
                    }
                }
            } else {
                let member_id = create_child_module(self, scope_id, module_name.clone())?;

                match self.objs.member(member_id).variant {
                    MemberVariant::Module(scope_id) => scope_id,
                    _ => {
                        panic!("should have just created module member");
                    }
                }
            };
        }

        // last scope we get is the file module scope
        // so it is special and we should set its "variant" field
        // and its file module is itself

        let scope_mut = self.objs.scope_mut(scope_id);

        scope_mut.variant = ScopeVariant::FileModule(FileModuleInfo {
            tokens,
            root_expr: ExprID::default(),
        });
        scope_mut.file_module = scope_id;

        // AST will keep record of all file modules pushed this way
        self.objs.file_module_push(scope_id);

        Ok(scope_id)
    }

    // get tokens for current file
    pub fn tokens(&self) -> &Tokens {
        let scope_id = self.curr_file_module;

        match &self.objs.scope(scope_id).variant {
            ScopeVariant::FileModule(info) => &info.tokens,
            _ => {
                panic!("no current Tokens");
            }
        }
    }

    // get tokens for current file
    pub fn tokens_mut(&mut self) -> &mut Tokens {
        let scope_id = self.curr_file_module;

        match &mut self.objs.scope_mut(scope_id).variant {
            ScopeVariant::FileModule(info) => &mut info.tokens,
            _ => {
                panic!("no current Tokens");
            }
        }
    }

    pub fn root_expr(&self) -> ExprID {
        let scope_id = self.curr_file_module;

        match &self.objs.scope(scope_id).variant {
            ScopeVariant::FileModule(info) => info.root_expr,
            _ => {
                panic!("no current Tokens");
            }
        }
    }

    // suppose file imports function from other file which was parsed later
    // this way those imports and the code can be fixed up
    pub fn repair(&mut self) {
        let file_module_scopes: Vec<ScopeID> = self.objs.file_module_iter().collect();

        for scope_id in file_module_scopes {
            self.set_curr_file_module(scope_id);

            self.doing_repair = true;

            self.do_semantic_analysis();

            self.doing_repair = false;
        }
    }
}

// module path is what module this file corresponds to e.g.
// [a, b, c] would correspond to module a.b.c
pub fn parse_file(ast: &mut AST, tokens: Tokens, modulepath: Vec<String>) {
    // tokens will be stored in Scope corresponding to module, so we create
    // that here with helper function and pass in tokens

    let file_scope_id = match ast.add_file_module(tokens, modulepath.clone()) {
        Ok(scope_id) => scope_id,
        Err(_) => {
            return;
        }
    };

    // set this to indicate current file module being analyzed (its scope)
    // also set for ASTContents (happens inside this helper method)

    ast.set_curr_file_module(file_scope_id);

    // call to parse_expr does syntactic analysis

    let root_expr = ast.do_syntax_analysis();

    // store root expr in info for current file/module

    match &mut ast.objs.scope_mut(file_scope_id).variant {
        ScopeVariant::FileModule(info) => {
            info.root_expr = root_expr;
        }
        _ => {
            panic!("file scope is not a file + module?");
        }
    }

    // eprintln!("{}", ast.expr_to_string(root_expr));

    let modulepath_string = modulepath.join(".");

    if ast.has_errors() {
        eprintln!(
            "One or more syntax errors occurred, module {modulepath_string} will not be compiled."
        );

        ast.display_all_errors();

        return;
    }

    // reenable later to test
    ast.do_semantic_analysis();

    eprintln!("RAN SEMANTIC ANALYSIS FUNC");

    // eprintln!("{}", ast.expr_to_string(root_expr));

    if ast.has_errors() {
        eprintln!("One or more semantic errors occurred, program will not be compiled.");

        ast.display_all_errors();

        return;
    }
}
