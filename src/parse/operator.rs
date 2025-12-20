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

enum Assoc {
    Left, // left-to-right
    Right, // right-to-left
}

struct OperatorInfo {
    pub ttype: TokenType,

    pub variant: OperatorVariant,
    pub assoc: Assoc,

    // is rhs arg optional (e.g. comma for tuples)
    pub is_rhs_optional: bool,
}

// very large table defining all language operators alongside info for parsing
// e.g. token for operator, is it infix, does it have left associativity
// later in table => higher binding power
static OP_INFO_TABLE : &[&[OperatorInfo]] = &[
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

// struct for recording left, right binding powers of operators in a table
#[derive(Clone, Copy)]
pub struct OperatorBp {
    // used in a table where some entries will be empty
    pub is_empty: bool,

    // left, right binding power for parsing exprs
    pub left_bp: u8,
    pub right_bp: u8,
}

// table type using above struct to map ttype to binding power
type TTypeBpTable = [
    [OperatorBp; OperatorVariantCount as usize];
    TokenType::TokenTypeCount as usize
];

// generates mapping from (TokenType, OperatorVariant e.g. Infix)
// to binding power
fn generate_ttype_to_operator_bp() -> TTypeBpTable{
    let mut table: [
        [OperatorBp; OperatorVariantCount as usize];
        TokenType::TokenTypeCount as usize
    ] = [
        [
            OperatorBp{
                is_empty: true,
                left_bp: 0,
                right_bp: 0,
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

            ret_entry.left_bp = bp as u8;
            ret_entry.right_bp = bp as u8;

            // for correct parsing need to modify based on Assoc
            if op_info.assoc == Assoc::Left {
                ret_entry.right_bp += 1;
            }
            else {
                ret_entry.left_bp += 1;
            }
        }
    }

    return table;
}

// maps (TokenType, OperatorVariant) -> Binding Power And OperatorInfo
// so if you have (Plus, Infix) you will find the info for the binary plus
static TTYPE_TO_OPERATOR_BP: LazyLock<TTypeBpTable> = LazyLock::new(generate_ttype_to_operator_bp);

// function which will be used during parsing to access info from this Rust
// file test if given token is an operator with given OperatorVariant
// e.g. does there exist an Infix operator whose token is Plus? (Yes)
// and then you will receive (left_bp, right_bp) if it is present
pub fn get_bp(
    ttype: TokenType,
    op_variant: OperatorVariant,
) -> Option<(u8, u8)> {
    if ttype == TokenType::TokenTypeCount || op_variant == OperatorVariantCount {
        return None;
    }

    // access relevant entry in table and if it is not empty then return it

    let maybe_ret: OperatorBp = TTYPE_TO_OPERATOR_BP[
        ttype as usize
    ][
        op_variant as usize
    ];

    if maybe_ret.is_empty {
        return None;
    }

    Some((maybe_ret.left_bp, maybe_ret.right_bp))
}