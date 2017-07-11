///! fff-lang
///!
///! lexical/seperator
///! Attention: contens are auto generated by token.py, do not modify this file

#[allow(non_snake_case)]
#[allow(non_upper_case_globals)]
pub mod SeperatorCategory {
    pub const       Additive: u16 = 0x0001;
    pub const         Assign: u16 = 0x0002;
    pub const         BitAnd: u16 = 0x0004;
    pub const          BitOr: u16 = 0x0008;
    pub const         BitXor: u16 = 0x0010;
    pub const       Equality: u16 = 0x0020;
    pub const     LogicalAnd: u16 = 0x0040;
    pub const      LogicalOr: u16 = 0x0080;
    pub const Multiplicative: u16 = 0x0100;
    pub const          Range: u16 = 0x0200;
    pub const     Relational: u16 = 0x0400;
    pub const       Reserved: u16 = 0x0800;
    pub const      Seperator: u16 = 0x1000;
    pub const          Shift: u16 = 0x2000;
    pub const          Unary: u16 = 0x4000;
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Seperator {
    LogicalNot,
    AttributeSign,
    Rem,
    BitAnd,
    LeftParenthenes,
    RightParenthenes,
    Mul,
    Add,
    Comma,
    Sub,
    Dot,
    Div,
    Colon,
    SemiColon,
    Less,
    Assign,
    Great,
    LeftBracket,
    RightBracket,
    BitXor,
    LeftBrace,
    BitOr,
    RightBrace,
    BitNot,
    NotEqual,
    RemAssign,
    LogicalAnd,
    BitAndAssign,
    MulAssign,
    AddAssign,
    SubAssign,
    NarrowRightArrow,
    Range,
    DivAssign,
    NamespaceSeperator,
    ShiftLeft,
    LessEqual,
    Equal,
    WideRightArrow,
    GreatEqual,
    ShiftRight,
    BitXorAssign,
    BitOrAssign,
    LogicalOr,
    ShiftLeftAssign,
    ShiftRightAssign,
}

const CHAR_MAX_CODEPOINT: u64 = 0x10FFFF;
const LEN1_EMPTY: (u32, u8) = (37, 0);
const LEN2_EMPTY: (u64, u8) = (1241244434433, 0);
const LEN1_BUCKET: &[(u32, u8)] = &[
    (37, 2), (38, 3), LEN1_EMPTY, (40, 4), (41, 5), (42, 6), 
    (43, 7), (44, 8), (45, 9), (46, 10), (47, 11), LEN1_EMPTY, 
    (123, 20), (124, 21), (125, 22), (126, 23), LEN1_EMPTY, (91, 17), 
    LEN1_EMPTY, (93, 18), (94, 19), (58, 12), (59, 13), (60, 14), 
    (61, 15), (62, 16), LEN1_EMPTY, LEN1_EMPTY, LEN1_EMPTY, LEN1_EMPTY, 
    LEN1_EMPTY, LEN1_EMPTY, LEN1_EMPTY, (33, 0), LEN1_EMPTY, (35, 1), 
    LEN1_EMPTY, 
];
const LEN2_BUCKET1: &[(u64, u8)] = &[
    (67960816, 6), LEN2_EMPTY, (67960818, 9), (69074943, 14), 
    (69074944, 16), (67960865, 17), (42336256, 2), LEN2_EMPTY, 
    (64618496, 10), (69074927, 7), (67960804, 0), LEN2_EMPTY, 
    LEN2_EMPTY, (67960895, 18), (67960808, 1), (67960809, 3), 
    (67960832, 13), (67960833, 15), LEN2_EMPTY, (67960813, 4), 
    (67960814, 5), LEN2_EMPTY, 
];
const LEN2_BUCKET2: &[(u64, u8)] = &[
    LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, 
    LEN2_EMPTY, LEN2_EMPTY, (66846720, 11), LEN2_EMPTY, 
    (138149888, 19), LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, 
    LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, (67960831, 12), 
    LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, LEN2_EMPTY, 
    (51249152, 8), LEN2_EMPTY, 
];
impl Seperator {

    pub fn parse1(ch: char) -> Option<Seperator> {
        let hash = ch as u32;
        match LEN1_BUCKET[(hash % 37) as usize] {
            (key, _) if key != hash => None,
            (_, index) => unsafe { Some(::std::mem::transmute(index as u8)) },
        }
    }
    pub fn parse3(ch1: char, ch2: char, ch3: char) -> Option<(Seperator, usize)> {
        let hash2 = ch1 as u64 + ch2 as u64 * CHAR_MAX_CODEPOINT;
        let hash1 = ch1 as u32;
        match &[ch1 as u8, ch2 as u8, ch3 as u8] {
            b"<<=" => unsafe { Some((::std::mem::transmute(44u8), 3)) },
            b">>=" => unsafe { Some((::std::mem::transmute(45u8), 3)) },
            _ => match LEN2_BUCKET1[(hash2 % 22) as usize] {
                (key, _) if key != hash2 => match LEN2_BUCKET2[(hash2 % 22) as usize] {
                    (key, _) if key != hash2 => match LEN1_BUCKET[(hash1 % 37) as usize] {
                        (key, _) if key != hash1 => None,
                        (_, index) => unsafe { Some((::std::mem::transmute(index), 1)) },
                    },
                    (_, index) => unsafe { Some((::std::mem::transmute(index + 24), 2)) },
                },
                (_, index) => unsafe { Some((::std::mem::transmute(index + 24), 2)) },
            },
        }
    }
}
impl ::std::fmt::Debug for Seperator {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        const SEP_VALUES: &[&str] = &[
            "!", "#", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", 
            ":", ";", "<", "=", ">", "[", "]", "^", "{", "|", "}", "~", 
            "!=", "%=", "&&", "&=", "*=", "+=", "-=", "->", "..", "/=", "::", "<<", 
            "<=", "==", "=>", ">=", ">>", "^=", "|=", "||", "<<=", ">>="
        ];
        unsafe { write!(f, "{}",
            SEP_VALUES[::std::mem::transmute_copy::<Seperator, u8>(self) as usize]
        ) }
    }
}
impl Seperator {

    pub fn is_category(&self, cat: u16) -> bool {
        const SEP_CATS: &[u16] = &[
            16384, 2048, 256, 4, 4096, 4096, 256, 1, 4096, 16385, 4096, 256, 
            4096, 4096, 1024, 2, 1024, 4096, 4096, 16, 4096, 8, 4096, 16384, 
            32, 2, 64, 2, 2, 2, 2, 4096, 512, 2, 4096, 8192, 
            1024, 32, 2048, 1024, 8192, 2, 2, 128, 2, 2
        ];
        unsafe {
            (SEP_CATS[::std::mem::transmute_copy::<Seperator, u8>(self) as usize] & cat) == cat
        }
    }
}
#[cfg(test)] #[test]
fn seperator_debug() {

    assert_eq!{ format!("{:?}", Seperator::BitOr), "|" }
    assert_eq!{ format!("{:?}", Seperator::Range), ".." }
    assert_eq!{ format!("{:?}", Seperator::RightBracket), "]" }
    assert_eq!{ format!("{:?}", Seperator::WideRightArrow), "=>" }
    assert_eq!{ format!("{:?}", Seperator::MulAssign), "*=" }
    assert_eq!{ format!("{:?}", Seperator::BitAnd), "&" }
    assert_eq!{ format!("{:?}", Seperator::WideRightArrow), "=>" }
    assert_eq!{ format!("{:?}", Seperator::RightBracket), "]" }
    assert_eq!{ format!("{:?}", Seperator::AttributeSign), "#" }
    assert_eq!{ format!("{:?}", Seperator::DivAssign), "/=" }
}
#[cfg(test)] #[test]
fn seperator_is_cat() {

    assert_eq!{ Seperator::ShiftLeftAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::ShiftLeftAssign.is_category(SeperatorCategory::BitOr), false }
    assert_eq!{ Seperator::RemAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::RemAssign.is_category(SeperatorCategory::Reserved), false }
    assert_eq!{ Seperator::Sub.is_category(SeperatorCategory::Additive), true }
    assert_eq!{ Seperator::Sub.is_category(SeperatorCategory::Seperator), false }
    assert_eq!{ Seperator::LogicalOr.is_category(SeperatorCategory::LogicalOr), true }
    assert_eq!{ Seperator::LogicalOr.is_category(SeperatorCategory::Relational), false }
    assert_eq!{ Seperator::BitAndAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::BitAndAssign.is_category(SeperatorCategory::Multiplicative), false }
    assert_eq!{ Seperator::DivAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::DivAssign.is_category(SeperatorCategory::BitOr), false }
    assert_eq!{ Seperator::AddAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::AddAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::Colon.is_category(SeperatorCategory::Seperator), true }
    assert_eq!{ Seperator::Colon.is_category(SeperatorCategory::BitXor), false }
    assert_eq!{ Seperator::ShiftRight.is_category(SeperatorCategory::Shift), true }
    assert_eq!{ Seperator::ShiftRight.is_category(SeperatorCategory::LogicalOr), false }
    assert_eq!{ Seperator::RemAssign.is_category(SeperatorCategory::Assign), true }
    assert_eq!{ Seperator::RemAssign.is_category(SeperatorCategory::Shift), false }
}
#[cfg(test)] #[test]
fn seperator_parse() {

    assert_eq!{ Seperator::parse3('<', '<', '='), Some((Seperator::ShiftLeftAssign, 3)) }
    assert_eq!{ Seperator::parse3('+', ' ', '1'), Some((Seperator::Add, 1)) }
    assert_eq!{ Seperator::parse3('{', ' ', 'a'), Some((Seperator::LeftBrace, 1)) }
    assert_eq!{ Seperator::parse3('&', '&', ' '), Some((Seperator::LogicalAnd, 2)) }
    assert_eq!{ Seperator::parse3('Х', '9', ' '), None }
}
