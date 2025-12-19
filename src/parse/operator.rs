#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum OperatorVariant {
    Prefix,
    Postfix,
    Infix,
    Around, // e.g. paren
    PostfixAround, // e.g. function call
    OperatorVariantCount,
}

use std::sync::LazyLock;

use OperatorVariant::*;

use crate::scan::TokenType;

#[derive(Clone, Copy, PartialEq)]

pub enum Assoc {
    Left, // left-to-right
    Right, // right-to-left
}

pub struct OperatorInfo {
    pub ttype: TokenType,

    pub variant: OperatorVariant,
    pub assoc: Assoc,

    // is rhs arg optional (e.g. comma for tuples)
    pub is_rhs_optional: bool,
}

static DEFAULT_OP_INFO : OperatorInfo = OperatorInfo{
    ttype: TokenType::Plus,
    variant: Infix,
    assoc: Assoc::Left,
    is_rhs_optional: false,
};

// very large table defining all language operators alongside info for parsing
// e.g. token for operator, is it infix, does it have left associativity
// later in table => higher binding power
pub static OP_INFO_TABLE : &[&[OperatorInfo]] = &[
    &[
        OperatorInfo{
            ttype: TokenType::Semicolon,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: true,
        },
    ],
    &[ // parens e.g. (5) are thought of as operator
        OperatorInfo{
            ttype: TokenType::LParen,
            variant: Around,
            assoc: Assoc::Left,
            is_rhs_optional: true,
        },
        OperatorInfo{
            ttype: TokenType::LBrace,
            variant: Around,
            assoc: Assoc::Left,
            is_rhs_optional: true,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Return,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: true,
        },
        OperatorInfo{
            ttype: TokenType::Break,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: true,
        },
        OperatorInfo{
            ttype: TokenType::Continue,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: true,
        },

        OperatorInfo{
            ttype: TokenType::Goto,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false, // requires label
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Comma,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: true,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::ColonEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Eq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::PipeEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::CarrotEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::AmpersandEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::LtLtEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::GtGtEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::PlusEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::MinusEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::StarEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::FSlashEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::PercentEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::StarStarEq,
            variant: Infix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Colon,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Or,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::And,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    // DIFFERENCE FROM C: comparison operators bind looser than bitwise
    &[
        OperatorInfo{
            ttype: TokenType::EqEq,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::BangEq,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Lt,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Le,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Gt,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Ge,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Pipe,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Carrot,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Ampersand,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::LtLt,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::GtGt,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Plus,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Minus,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Star,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::FSlash,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Percent,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::StarStar,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Plus,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Minus,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Bang,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Tilde,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Ampersand,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::Star,
            variant: Prefix,
            assoc: Assoc::Right,
            is_rhs_optional: false,
        },
    ],
    &[
        OperatorInfo{
            ttype: TokenType::Period,
            variant: Infix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },

        OperatorInfo{
            ttype: TokenType::PlusPlus,
            variant: Postfix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },
        OperatorInfo{
            ttype: TokenType::MinusMinus,
            variant: Postfix,
            assoc: Assoc::Left,
            is_rhs_optional: false,
        },

        OperatorInfo{
            ttype: TokenType::LParen,
            variant: PostfixAround,
            assoc: Assoc::Left,
            is_rhs_optional: true,
        },
    ],
];

// includes binding power
#[derive(Clone, Copy)]
pub struct BpAndOperatorInfo {
    pub is_empty: bool,
    pub left_bp: usize,
    pub right_bp: usize,
    pub op_info: &'static OperatorInfo,
}

// generates mapping from (TokenType, OperatorVariant e.g. Infix)
// to OperatorInfo and binding power
fn generate_type_to_operator_info() -> [
    [BpAndOperatorInfo; OperatorVariantCount as usize];
    TokenType::TokenTypeCount as usize
]{
    let mut table: [
        [BpAndOperatorInfo; OperatorVariantCount as usize];
        TokenType::TokenTypeCount as usize
    ] = [
        [
            BpAndOperatorInfo{
                is_empty: true,
                left_bp: 0,
                right_bp: 0,
                op_info: &DEFAULT_OP_INFO,
            };
            OperatorVariantCount as usize
        ];
        TokenType::TokenTypeCount as usize
    ];

    // later row => higher bp so use enumerate to get bp
    for (mut bp, row) in OP_INFO_TABLE.iter().enumerate() {
        // scaling it up so we have room to make left/right bp
        // larger without overlapping precedence levels
        bp *= 2;

        for op_info in (*row).iter() {
            let ret_entry = &mut table[op_info.ttype as usize][op_info.variant as usize];

            ret_entry.is_empty = false;

            ret_entry.left_bp = bp;
            ret_entry.right_bp = bp;

            if op_info.assoc == Assoc::Left {
                ret_entry.right_bp += 1;
            }
            else {
                ret_entry.left_bp += 1;
            }

            ret_entry.op_info = op_info;
        }
    }

    return table;
}

// maps (TokenType, OperatorVariant) -> Binding Power And OperatorInfo
// so if you have (Plus, Infix) you will find the info for the binary plus
pub static TTYPE_TO_OPERATOR_INFO: LazyLock<[
    [BpAndOperatorInfo; OperatorVariantCount as usize];
    TokenType::TokenTypeCount as usize
]> = LazyLock::new(generate_type_to_operator_info);