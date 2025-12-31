use std::{string::String, sync::LazyLock};

enum ScanMethod {
    Text(&'static str),
    Proc(fn(&str) -> usize),
}


#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum TokenType {
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
    Comma,
    Or,
    And,
    Pipe,
    Carrot,
    Ampersand,
    EqEq,
    BangEq,
    Lt,
    Gt,
    Le,
    Ge,
    LtLt,
    GtGt,
    Plus,
    Minus,
    Star,
    FSlash,
    Percent,
    StarStar,
    Bang,
    Tilde,
    Period,
    ColonEq,
    Eq,
    PipeEq,
    CarrotEq,
    AmpersandEq,
    LtLtEq,
    GtGtEq,
    PlusEq,
    MinusEq,
    StarEq,
    FSlashEq,
    PercentEq,
    StarStarEq,
    Colon,
    PlusPlus,
    MinusMinus,
    DArrow,
    PipeGt,
    Underscore,

    Type,
    New,
    Is,
    With,
    Begin,
    End,
    Import,
    Export,
    From,
    Global,

    If,
    Then,
    Elif,
    Else,
    Loop,
    While,
    For,
    Do,

    Return,
    Goto,
    Break,
    Continue,

    // KW means keyword e.g. the keyword "integer"
    KWInteger,
    KWFloat,
    Function,
    Record,
    Enum,
    Variant,

    DollarNumber,
    Identifier,
    Integer,
    Float,
    String,

    Eof,

    // NOTE: keep this as last and do not set discriminant values
    // this ensures scanning code works correctly
    TokenTypeCount,
}

// some code for stringifying, displaying TokenType

static TTYPE_STRINGS: LazyLock<Vec<String>> = LazyLock::new(|| (0..=(TokenTypeCount as u8)).map(
    |ttype_int|
    format!(
        "{:?}",
        unsafe {
            std::mem::transmute::<u8,TokenType>(ttype_int)
        },
    )
).collect());

impl AsRef<str> for TokenType {
    fn as_ref(&self) -> &str {
        TTYPE_STRINGS[unsafe{
            std::mem::transmute::<TokenType,u8>(*self)
        } as usize].as_str()
    }
}

impl TokenType {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

use ScanMethod::*;
use TokenType::*;

fn scan_identifier(s: &str) -> usize {
    let mut chars = s.chars();

    if let Some(first_c) = chars.next() {
        if first_c != '_' && !first_c.is_alphabetic() {
            return 0;
        }
    }
    else {
        return 0;
    }

    let mut ret = 1;

    for c in chars {
        if c == '_' || c.is_alphanumeric() {
            ret += 1;
        }
        else {
            break;
        }
    }

    ret
}

fn scan_integer(s: &str) -> usize {
    let mut ret = 0;

    for c in s.chars() {
        if c.is_numeric() {
            ret += 1;
        }
        else {
            break;
        }
    }

    return ret;
}

fn scan_float(s: &str) -> usize {
    let mut ret  = scan_integer(s);

    if ret == 0 {
        return 0;
    }

    let mut chars = s.chars();

    match chars.nth(ret) {
        Some('.') => {}
        _ => {
            return 0;
        }
    }

    ret += 1;

    let after_period = scan_integer(chars.as_str());

    if after_period == 0 {
        return 0;
    }

    ret += after_period;

    match chars.nth(after_period) {
        Some('e' | 'E') => {
            ret += 1;

            let mut is_number_after_e = true;

            match chars.next() {
                Some(c) => {
                    if c == '-' || c.is_numeric() {
                        ret += 1;

                        if c == '-' {
                            is_number_after_e = false;
                        }
                    }
                    else {
                        return 0;
                    }
                }
                _ => {
                    return 0;
                }
            }

            let after_e = scan_integer(chars.as_str());

            if after_e == 0 && !is_number_after_e {
                return 0;
            }

            ret += after_e;
        }
        _ => {}
    }

    ret
}

fn scan_string(s: &str) -> usize {
    let mut chars = s.chars();

    match chars.next() {
        Some('"') => {}
        _ => {
            return 0;
        }
    }

    let mut ret = 1;
    let mut is_escaped = false;

    for c in chars {
        ret += 1;

        if is_escaped {
            is_escaped = false;
            continue;
        }

        if c == '\\' {
            is_escaped = true;
        }
        else if c == '"' {
            break;
        }
    }

    ret
}

fn scan_dollar_number(s: &str) -> usize {
    let mut chars = s.chars();

    match chars.next() {
        Some('$') => (),
        _ => return 0,
    }

    let ret = 1 + scan_integer(chars.as_str());

    if ret == 1 {
        return 0;
    }

    ret
}

fn scan_ws_and_comments(s: &str) -> usize {
    let mut len: usize = 0;
    let mut chars = s.chars();

    loop {
        let mut found_ws_or_comment = false;

        // 'a' is acceptable default since loop will quit if next is None
        let c = chars.next().unwrap_or('a');
        
        if c == '/' {
            // check if comment has been found, if not break

            if let Some('/') = chars.next() {
                found_ws_or_comment = true;

                len += 2;

                while let Some(c) = chars.next() {
                    len += 1;
                    
                    if c == '\n' {
                        break;
                    }
                }
            }
            else {
                break;
            }
        }
        else if c.is_whitespace() {
            found_ws_or_comment = true;
            
            len += 1;
        }

        if !found_ws_or_comment {
            break;
        }
    }

    len
}

impl TokenType {
    fn get_scan_method(&self) -> ScanMethod {
        match self {
            Semicolon => Text(";"),
            LParen => Text("("),
            RParen => Text(")"),
            LBrace => Text("["),
            RBrace => Text("]"),
            LCurly => Text("{"),
            RCurly => Text("}"),
            Comma => Text(","),
            Or => Text("or"),
            And => Text ("and"),
            Pipe => Text ("|"),
            Carrot => Text ("^"),
            Ampersand => Text ("&"),
            EqEq => Text ("=="),
            BangEq => Text ("!="),
            Lt => Text ("<"),
            Gt => Text (">"),
            Le => Text ("<="),
            Ge => Text (">="),
            LtLt => Text ("<<"),
            GtGt => Text (">>"),
            Plus => Text("+"),
            Minus => Text("-"),
            Star => Text("*"),
            FSlash => Text("/"),
            Percent => Text("%"),
            StarStar => Text("**"),
            Bang => Text("!"),
            Tilde => Text("~"),
            Period => Text("."),
            ColonEq => Text(":="),
            Eq => Text("="),
            PipeEq => Text("|="),
            CarrotEq => Text("^="),
            AmpersandEq => Text("&="),
            LtLtEq => Text("<<="),
            GtGtEq => Text(">>="),
            PlusEq => Text("+="),
            MinusEq => Text("-="),
            StarEq => Text("*="),
            FSlashEq => Text("/="),
            PercentEq => Text("%="),
            StarStarEq => Text("**="),
            Colon => Text(":"),
            PlusPlus => Text("++"),
            MinusMinus => Text("--"),
            DArrow => Text("=>"),
            PipeGt => Text("|>"),
            Underscore => Text("_"),
            
            Type => Text("type"),
            New => Text("new"),
            Is => Text("is"),
            With => Text("with"),
            Begin => Text("begin"),
            End => Text("end"),
            Import => Text("import"),
            Export => Text("export"),
            From => Text("from"),
            Global => Text("global"),

            If => Text("if"),
            Then => Text("then"),
            Elif => Text("elif"),
            Else => Text("else"),
            Loop => Text("loop"),
            While => Text("while"),
            For => Text("for"),
            Do => Text("do"),

            Return => Text("return"),
            Goto => Text("goto"),
            Break => Text("break"),
            Continue => Text("continue"),

            KWInteger => Text("integer"),
            KWFloat => Text("float"),
            Function => Text("function"),
            Record => Text("record"),
            Enum => Text("enum"),
            Variant => Text("variant"),

            DollarNumber => Proc(scan_dollar_number),
            Identifier => Proc(scan_identifier),
            Integer => Proc(scan_integer),
            Float => Proc(scan_float),
            String => Proc(scan_string),

            Eof => Text(""),

            TokenTypeCount => Text(""),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub ttype: TokenType,
    pub line: u16,
    pub column: u16,

    // NOTE: end_column is NOT inclusive
    pub end_column: u16,

    // overall offset in file
    pub offset: u32,
}

impl Token {
    // s is string where Token was sourced from
    pub fn as_str<'a>(&self, s: &'a str) -> &'a str {
        &s[self.offset as usize..(self.offset + self.end_column as u32 - self.column as u32) as usize]
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            ttype: TokenType::Eof,
            line: 1,
            column: 1,
            end_column: 1,
            offset: 0,
        }
    }
}

pub fn tokenize(
    s: &str
) -> Result<Vec<Token>,String> {
    // offset in file (s)
    let mut offset = 0;

    // line, column are 1-indexed
    let mut line = 1;
    let mut column = 1;

    // iterator for use throughout function and Token vec for ret
    let mut chars = s.chars();
    let mut tokens: Vec<Token> = Vec::new();

    while chars.as_str() != "" {
        // advance past whitespace, comment(s)

        let skip = scan_ws_and_comments(chars.as_str());

        offset += skip;

        // consume chars and modify line, column

        for _ in 0..skip {
            let c = chars.next().expect("bug in ws/comments scan func");

            column += 1;

            if c == '\n' {
                line += 1;
                column = 1;
            }
        }

        if chars.as_str() == "" {
            break;
        }

        // find longest match TokenType

        let mut max_match: usize = 0;
        let mut match_ttype: TokenType = TokenType::Plus;

        // going through each tokentype as u8 and cast
        // to check them all in order

        for ttype_int in 0..(TokenTypeCount as u8) {
            let ttype : TokenType = unsafe {
                std::mem::transmute(ttype_int)
            };

            let curr_match = match ttype.get_scan_method() {
                Text(s) => {
                    if chars.as_str().starts_with(s) {
                        s.chars().count()
                    }
                    else {
                        0
                    }
                },
                Proc(f) => f(chars.as_str()),
            };

            if curr_match > max_match {
                max_match = curr_match;
                match_ttype = ttype;
            }
        }

        if max_match == 0 { // => no match was found
            let s = chars.as_str();
            let len = std::cmp::min(chars.count(), 25);

            let s = &s[..len];

            return Err(format!(
                "Ln {}, Col {}: no matching token found here \"{}\"",
                line,
                column,
                s,
            ));
        }

        // push new token
        // record text as String if necessary

        tokens.push(Token {
            ttype: match_ttype,
            line: line,
            column: column,
            end_column: column + max_match as u16,
            offset: offset as u32,
        });

        // advance column, chars iterator, and offset

        offset += max_match;
        column += max_match as u16;
        chars.nth(max_match - 1);
    }

    // always push EOF token on at the end (zero width)

    tokens.push(Token {
        ttype: Eof,
        line: line,
        column: column,
        end_column: column,
        offset: offset as u32,
    });

    Ok(tokens)
}