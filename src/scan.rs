use std::{string::String, sync::LazyLock};

enum ScanMethod {
    Text(&'static [u8]),
    Proc(fn(&[u8]) -> usize),
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
    Module,

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

    True,
    False,

    DollarNumber,
    Identifier,
    Integer,
    Float,
    Character,
    String,

    Eof,

    // NOTE: keep this as last and do not set discriminant values
    // this ensures scanning code works correctly
    TokenTypeCount,
}

// some code for stringifying, displaying TokenType

static TTYPE_STRINGS: LazyLock<Vec<String>> = LazyLock::new(|| {
    (0..=(TokenTypeCount as u8))
        .map(|ttype_int| {
            format!("{:?}", unsafe {
                std::mem::transmute::<u8, TokenType>(ttype_int)
            },)
        })
        .collect()
});

impl AsRef<str> for TokenType {
    fn as_ref(&self) -> &str {
        TTYPE_STRINGS[unsafe { std::mem::transmute::<TokenType, u8>(*self) } as usize].as_str()
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

fn scan_identifier(s: &[u8]) -> usize {
    let mut chars = s.iter();

    if let Some(first_c) = chars.next() {
        if *first_c != b'_' && !first_c.is_ascii_alphabetic() {
            return 0;
        }
    } else {
        return 0;
    }

    let mut ret = 1;

    for c in chars {
        if *c == b'_' || c.is_ascii_alphanumeric() {
            ret += 1;
        } else {
            break;
        }
    }

    ret
}

fn is_u8_numeric(value: u8) -> bool {
    value >= b'0' && value <= b'9'
}

fn scan_integer(s: &[u8]) -> usize {
    let mut ret = 0;

    for c in s.iter() {
        if is_u8_numeric(*c) {
            ret += 1;
        } else {
            break;
        }
    }

    return ret;
}

fn scan_float(s: &[u8]) -> usize {
    let mut ret = scan_integer(s);

    if ret == 0 {
        return 0;
    }

    let mut chars = s.iter();

    match chars.nth(ret) {
        Some(b'.') => {}
        _ => {
            return 0;
        }
    }

    ret += 1;

    let after_period = scan_integer(chars.as_slice());

    if after_period == 0 {
        return 0;
    }

    ret += after_period;

    match chars.nth(after_period) {
        Some(b'e' | b'E') => {
            ret += 1;

            let mut is_number_after_e = true;

            match chars.next() {
                Some(c) => {
                    if *c == b'-' || is_u8_numeric(*c) {
                        ret += 1;

                        if *c == b'-' {
                            is_number_after_e = false;
                        }
                    } else {
                        return 0;
                    }
                }
                _ => {
                    return 0;
                }
            }

            let after_e = scan_integer(chars.as_slice());

            if after_e == 0 && !is_number_after_e {
                return 0;
            }

            ret += after_e;
        }
        _ => {}
    }

    ret
}

fn scan_string(s: &[u8]) -> usize {
    let mut chars = s.iter();

    match chars.next() {
        Some(b'"') => {}
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

        if *c == b'\\' {
            is_escaped = true;
        } else if *c == b'"' {
            break;
        }
    }

    ret
}

// separate internal function to use ? operator on next()
fn scan_character_internal(s: &[u8]) -> Option<usize> {
    // quote, char, quote => 3
    // quote, backslash, other, quote => 4 (or 1 extra)

    static CHAR_LEN: usize = 3;
    static BSLASH_CHAR_LEN: usize = CHAR_LEN + 1;

    // which chars you can escape e.g. n for \n (newline)
    // not gonna do as many as C I think these are enough

    static ESC_SEQUENCE_CHARS: &[u8] = b"nrt\\\'";

    let mut chars = s.iter();

    // start quote

    if *chars.next()? != b'\'' {
        return None;
    }

    let is_escaped;

    match chars.next()? {
        b'\\' => {
            is_escaped = true;

            // next char must be one of the valid escape sequence chars

            let c = chars.next()?;

            if !ESC_SEQUENCE_CHARS.contains(c) {
                return None;
            }
        }
        c @ _ => {
            if c.is_ascii() {
                // is ASCII => Ok, record not escaped char and proceed
                is_escaped = false;
            } else {
                return None;
            }
        }
    }

    // end quote

    if *chars.next()? != b'\'' {
        return None;
    }

    if is_escaped {
        Some(BSLASH_CHAR_LEN)
    } else {
        Some(CHAR_LEN)
    }
}

fn scan_character(s: &[u8]) -> usize {
    match scan_character_internal(s) {
        Some(len) => len,
        None => 0,
    }
}

fn scan_dollar_number(s: &[u8]) -> usize {
    let mut chars = s.iter();

    match chars.next() {
        Some(b'$') => (),
        _ => return 0,
    }

    let ret = 1 + scan_integer(chars.as_slice());

    if ret == 1 {
        return 0;
    }

    ret
}

fn scan_ws_and_comments(s: &[u8]) -> usize {
    let mut len: usize = 0;
    let mut chars = s.iter();

    loop {
        let mut found_ws_or_comment = false;

        // 'a' is acceptable default since loop will quit if next is None
        let c = chars.next().map(|c| *c).unwrap_or(b'a');

        if c == b'/' {
            // check if comment has been found, if not break

            if let Some(b'/') = chars.next() {
                found_ws_or_comment = true;

                len += 2;

                while let Some(c) = chars.next() {
                    len += 1;

                    if *c == b'\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        } else if c.is_ascii_whitespace() {
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
            Semicolon => Text(b";"),
            LParen => Text(b"("),
            RParen => Text(b")"),
            LBrace => Text(b"["),
            RBrace => Text(b"]"),
            LCurly => Text(b"{"),
            RCurly => Text(b"}"),
            Comma => Text(b","),
            Or => Text(b"or"),
            And => Text(b"and"),
            Pipe => Text(b"|"),
            Carrot => Text(b"^"),
            Ampersand => Text(b"&"),
            EqEq => Text(b"=="),
            BangEq => Text(b"!="),
            Lt => Text(b"<"),
            Gt => Text(b">"),
            Le => Text(b"<="),
            Ge => Text(b">="),
            LtLt => Text(b"<<"),
            GtGt => Text(b">>"),
            Plus => Text(b"+"),
            Minus => Text(b"-"),
            Star => Text(b"*"),
            FSlash => Text(b"/"),
            Percent => Text(b"%"),
            StarStar => Text(b"**"),
            Bang => Text(b"!"),
            Tilde => Text(b"~"),
            Period => Text(b"."),
            ColonEq => Text(b":="),
            Eq => Text(b"="),
            PipeEq => Text(b"|="),
            CarrotEq => Text(b"^="),
            AmpersandEq => Text(b"&="),
            LtLtEq => Text(b"<<="),
            GtGtEq => Text(b">>="),
            PlusEq => Text(b"+="),
            MinusEq => Text(b"-="),
            StarEq => Text(b"*="),
            FSlashEq => Text(b"/="),
            PercentEq => Text(b"%="),
            StarStarEq => Text(b"**="),
            Colon => Text(b":"),
            PlusPlus => Text(b"++"),
            MinusMinus => Text(b"--"),
            DArrow => Text(b"=>"),
            PipeGt => Text(b"|>"),
            Underscore => Text(b"_"),

            Type => Text(b"type"),
            New => Text(b"new"),
            Is => Text(b"is"),
            With => Text(b"with"),
            Begin => Text(b"begin"),
            End => Text(b"end"),
            Import => Text(b"import"),
            Export => Text(b"export"),
            From => Text(b"from"),
            Global => Text(b"global"),
            Module => Text(b"module"),

            If => Text(b"if"),
            Then => Text(b"then"),
            Elif => Text(b"elif"),
            Else => Text(b"else"),
            Loop => Text(b"loop"),
            While => Text(b"while"),
            For => Text(b"for"),
            Do => Text(b"do"),

            Return => Text(b"return"),
            Goto => Text(b"goto"),
            Break => Text(b"break"),
            Continue => Text(b"continue"),

            KWInteger => Text(b"integer"),
            KWFloat => Text(b"float"),
            Function => Text(b"function"),
            Record => Text(b"record"),
            Enum => Text(b"enum"),
            Variant => Text(b"variant"),

            True => Text(b"true"),
            False => Text(b"false"),

            DollarNumber => Proc(scan_dollar_number),
            Identifier => Proc(scan_identifier),
            Integer => Proc(scan_integer),
            Float => Proc(scan_float),
            Character => Proc(scan_character),
            String => Proc(scan_string),

            Eof => Text(b""),

            TokenTypeCount => Text(b""),
        }
    }
}

#[derive(Clone, Copy, Debug)]
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
    pub fn as_str<'a>(&self, s: &'a [u8]) -> &'a str {
        let s = std::str::from_utf8(s).unwrap();

        &s[self.offset as usize
            ..(self.offset + self.end_column as u32 - self.column as u32) as usize]
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

pub fn tokenize(s: &Vec<u8>) -> Result<Vec<Token>, String> {
    // offset in file (s)
    let mut offset = 0;

    // line, column are 1-indexed
    let mut line = 1;
    let mut column = 1;

    // iterator for use throughout function and Token vec for ret
    let mut chars = s.iter();
    let mut tokens: Vec<Token> = Vec::new();

    while chars.as_slice() != b"" {
        // advance past whitespace, comment(s)

        let skip = scan_ws_and_comments(chars.as_slice());

        offset += skip;

        // consume chars and modify line, column

        for _ in 0..skip {
            let c = chars.next().expect("bug in ws/comments scan func");

            column += 1;

            if *c == b'\n' {
                line += 1;
                column = 1;
            }
        }

        if chars.as_slice() == b"" {
            break;
        }

        // find longest match TokenType

        let mut max_match: usize = 0;
        let mut match_ttype: TokenType = TokenType::Plus;

        // going through each tokentype as u8 and cast
        // to check them all in order

        for ttype_int in 0..(TokenTypeCount as u8) {
            let ttype: TokenType = unsafe { std::mem::transmute(ttype_int) };

            let curr_match = match ttype.get_scan_method() {
                Text(s) => {
                    if chars.as_slice().starts_with(s) {
                        s.iter().count()
                    } else {
                        0
                    }
                }
                Proc(f) => f(chars.as_slice()),
            };

            if curr_match > max_match {
                max_match = curr_match;
                match_ttype = ttype;
            }
        }

        if max_match == 0 {
            // => no match was found
            let s = chars.as_slice();
            let len = std::cmp::min(chars.count(), 25);

            let s = &s[..len];

            return Err(format!(
                "Ln {}, Col {}: no matching token found here \"{}\"",
                line,
                column,
                crate::util::ascii_to_string(s.to_vec()),
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
