enum ScanMethod {
    Text(&'static str),
    Proc(fn(&str) -> usize),
}


#[derive(PartialEq)]
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
    PipePipe,
    AmpersandAmpersand,
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

    Begin,
    End,
    Function,
    Struct,
    Class,
    Import,
    Export,
    From,

    Identifier,
    Integer,
    Float,
    String,

    // NOTE: keep this as last and do not set discriminant values
    // this ensures scanning code works correctly
    TokenTypeCount,
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
            PipePipe => Text("||"),
            AmpersandAmpersand => Text ("&&"),
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
                
            Begin => Text("begin"),
            End => Text("end"),
            Function => Text("function"),
            Struct => Text("struct"),
            Class => Text("class"),
            Import => Text("import"),
            Export => Text("export"),
            From => Text("from"),

            Identifier => Proc(scan_identifier),
            Integer => Proc(scan_integer),
            Float => Proc(scan_float),
            String => Proc(scan_string),

            TokenTypeCount => Text(""),
        }
    }
}

pub struct Token<'a> {
    pub ttype: TokenType,
    pub text: &'a str,
}

pub fn tokenize<'a>(s: &'a str) -> Result<Vec<Token<'a>>,&'static str> {
    let mut chars = s.chars();

    loop {
        let mut max_match: usize = 0;
        let mut match_ttype: TokenType = TokenType::Plus;

        // going through each tokentype as u8 and cast
        // to check them all in order

        for ttype_int in 0..(TokenTypeCount as u8) {
            let ttype : TokenType = unsafe {
                std::mem::transmute(ttype_int)
            };


        }
    }
}