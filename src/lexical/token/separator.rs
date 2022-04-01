///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

#[repr(u16)]
#[allow(dead_code)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum SeparatorKind {
    Additive = 0x1,
    Assign = 0x2,
    BitAnd = 0x4,
    BitOr = 0x8,
    BitXor = 0x10,
    Equality = 0x20,
    LogicalAnd = 0x40,
    LogicalOr = 0x80,
    Multiplicative = 0x100,
    Range = 0x200,
    Relational = 0x400,
    Separator = 0x800,
    Shift = 0x1000,
    Unary = 0x2000,
}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Separator {
    /** `!` */ Not = 1,
    /** `%` */ Rem = 2,
    /** `&` */ And = 3,
    /** `(` */ LeftParen = 4,
    /** `)` */ RightParen = 5,
    /** `*` */ Mul = 6,
    /** `+` */ Add = 7,
    /** `,` */ Comma = 8,
    /** `-` */ Sub = 9,
    /** `.` */ Dot = 10,
    /** `/` */ Div = 11,
    /** `:` */ Colon = 12,
    /** `;` */ SemiColon = 13,
    /** `<` */ Lt = 14,
    /** `=` */ Eq = 15,
    /** `>` */ Gt = 16,
    /** `[` */ LeftBracket = 17,
    /** `]` */ RightBracket = 18,
    /** `^` */ Caret = 19,
    /** `{` */ LeftBrace = 20,
    /** `|` */ Or = 21,
    /** `}` */ RightBrace = 22,
    /** `~` */ Tilde = 23,
    /** `!=` */ NotEq = 24,
    /** `%=` */ RemEq = 25,
    /** `&&` */ AndAnd = 26,
    /** `&=` */ AndEq = 27,
    /** `*=` */ MulEq = 28,
    /** `+=` */ AddEq = 29,
    /** `-=` */ SubEq = 30,
    /** `->` */ Arrow = 31,
    /** `..` */ DotDot = 32,
    /** `/=` */ DivEq = 33,
    /** `::` */ ColonColon = 34,
    /** `<<` */ LtLt = 35,
    /** `<=` */ LtEq = 36,
    /** `==` */ EqEq = 37,
    /** `>=` */ GtEq = 38,
    /** `>>` */ GtGt = 39,
    /** `^=` */ CaretEq = 40,
    /** `|=` */ OrEq = 41,
    /** `||` */ OrOr = 42,
    /** `<<=` */ LtLtEq = 43,
    /** `>>=` */ GtGtEq = 44,
}

const VALUES: &[&str] = &[
    "", "!", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=",
    ">", "[", "]", "^", "{", "|", "}", "~", "!=", "%=", "&&", "&=", "*=", "+=", "-=",
    "->", "..", "/=", "::", "<<", "<=", "==", ">=", ">>", "^=", "|=", "||", "<<=",
    ">>=",
];
const KINDS: &[u16] = &[
    0, 8192, 256, 8196, 2048, 2048, 256, 1, 2048, 8193, 2048, 256, 2048, 2048, 1024,
    2, 1024, 2048, 2048, 16, 2048, 8, 2048, 8192, 32, 2, 64, 2, 2, 2, 2, 2048, 512,
    2, 2048, 4096, 1024, 32, 1024, 4096, 2, 2, 128, 2, 2,
];

impl Separator {

    pub fn display(self) -> &'static str {
        VALUES[self as u8 as usize]
    }

    pub fn kind(self, kind: SeparatorKind) -> bool {
        let kind = kind as u16;
        KINDS[self as u8 as usize] & kind == kind
    }
}

use Separator::*;
const BUCKET1: &[Option<Separator>] = &[
    Some(Or), Some(RightBrace), Some(Tilde), None, Some(Colon), Some(SemiColon),
    Some(Lt), Some(Eq), Some(Gt), None, Some(LeftBracket), Some(Not), Some(RightBracket),
    Some(Caret), None, Some(Rem), Some(And), None, Some(LeftParen), Some(RightParen),
    Some(Mul), Some(Add), Some(Comma), Some(Sub), Some(Dot), Some(Div), Some(LeftBrace),
];
const BUCKET2: &[Option<Separator>] = &[
    None, None, None, Some(RemEq), Some(AndEq), None, Some(CaretEq), None, Some(MulEq),
    Some(AddEq), None, Some(SubEq), None, Some(DivEq), None, Some(OrEq), Some(LtEq),
    Some(EqEq), Some(GtEq), None, None, Some(NotEq),
];
const BUCKET3: &[Option<Separator>] = &[
    Some(DotDot), None, None, Some(LtLt), None, None, Some(OrOr), Some(GtGt), None,
    Some(Arrow), Some(AndAnd), None, Some(ColonColon),
];

#[inline]
fn hash(c: char) -> u32 {
    let ord = c as u32;
    if ord < 48 { ord + 32 } else if ord > 120 { ord - 43 } else { ord }
}

impl Separator {
    pub fn parse(c1: char, c2: char, c3: char) -> Option<(Separator, usize)> {
        match [c1, c2, c3] {
            ['<', '<', '='] => Some((LtLtEq, 3)),
            ['>', '>', '='] => Some((GtGtEq, 3)),
            [c1, '=', _] => {
                BUCKET2[(hash(c1) % 22) as usize]
                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 2)) } else { None })
                    .or_else(|| BUCKET1[(hash(c1) % 27) as usize]
                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 1)) } else { None }))
            },
            [c1, c2, _] => {
                BUCKET3[((hash(c1) + hash(c2)) % 13) as usize]
                    .and_then(|s| if s.display().as_bytes()[0..2] == [c1 as u8, c2 as u8] { Some((s, 2)) } else { None })
                    .or_else(|| BUCKET1[(hash(c1) % 27) as usize]
                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 1)) } else { None }))
            },
        }
    }
}
