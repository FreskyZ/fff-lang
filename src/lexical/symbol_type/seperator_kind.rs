
// Seperator kind

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SeperatorKind {
    Add,            // +
    Sub,            // -
    Mul,            // *
    Div,            // /
    Rem,            // %
    AddAssign,      // +=
    SubAssign,      // -=
    MulAssign,      // *=
    DivAssign,      // /=
    RemAssign,      // %=

    BitNot,         // ~
    BitAnd,         // &
    BitOr,          // |
    BitXor,         // ^
    ShiftLeft,      // <<
    ShiftRight,     // >>
    BitAndAssign,   // &=
    BitOrAssign,    // |=
    BitXorAssign,   // ^=
    // ShiftLeftAssign,  // need preview 2, currently not support
    // ShiftRightAssign, // need preview 2, currently not support

    LogicalNot,     // !
    LogicalAnd,     // &&
    LogicalOr,      // ||

    Assign,         // =
    Less,           // <
    Equal,          // ==
    Great,          // >
    LessEqual,      // <=
    GreatEqual,     // >=
    NotEqual,       // !=

    LeftParenthenes,    // (
    RightParenthenes,   // )
    LeftBracket,        // [
    RightBracket,       // ]
    LeftBrace,          // {
    RightBrace,         // }
    Comma,              // ,
    Colon,              // :
    SemiColon,          // ;
    Dot,                // .

    NarrowRightArrow,   // ->
    WideRightArrow,     // =>
    NamespaceSeperator, // ::

    AttributeSign,  // #
}

use common::TryFrom;
impl TryFrom<char> for SeperatorKind {
    
    fn try_from(ch: char) -> Option<SeperatorKind> {
        match ch {
            '=' => Some(SeperatorKind::Assign),
            '+' => Some(SeperatorKind::Add),
            '-' => Some(SeperatorKind::Sub),
            '*' => Some(SeperatorKind::Mul),
            '/' => Some(SeperatorKind::Div),
            '%' => Some(SeperatorKind::Rem),
            '~' => Some(SeperatorKind::BitNot),
            '&' => Some(SeperatorKind::BitAnd),
            '|' => Some(SeperatorKind::BitOr),
            '^' => Some(SeperatorKind::BitXor),
            '!' => Some(SeperatorKind::LogicalNot),
            '<' => Some(SeperatorKind::Less),
            '>' => Some(SeperatorKind::Great),
            '(' => Some(SeperatorKind::LeftParenthenes),
            ')' => Some(SeperatorKind::RightParenthenes),
            '[' => Some(SeperatorKind::LeftBracket),
            ']' => Some(SeperatorKind::RightBracket),
            '{' => Some(SeperatorKind::LeftBrace),
            '}' => Some(SeperatorKind::RightBrace),
            ',' => Some(SeperatorKind::Comma),
            ':' => Some(SeperatorKind::Colon),
            ';' => Some(SeperatorKind::SemiColon),
            '.' => Some(SeperatorKind::Dot),
            '#' => Some(SeperatorKind::AttributeSign),
            _ => None,
        }
    }
}

impl TryFrom<(char, char)> for SeperatorKind {

    fn try_from(chs: (char, char)) -> Option<SeperatorKind> {
        match (chs.0, chs.1) {
            ('=', '=') => Some(SeperatorKind::Equal),
            ('+', '=') => Some(SeperatorKind::AddAssign),
            ('-', '=') => Some(SeperatorKind::SubAssign),
            ('*', '=') => Some(SeperatorKind::MulAssign),
            ('/', '=') => Some(SeperatorKind::DivAssign),
            ('%', '=') => Some(SeperatorKind::RemAssign),

            ('>', '>') => Some(SeperatorKind::ShiftRight),
            ('<', '<') => Some(SeperatorKind::ShiftRight),
            ('&', '&') => Some(SeperatorKind::LogicalAnd),
            ('|', '|') => Some(SeperatorKind::LogicalOr),

            ('<', '=') => Some(SeperatorKind::LessEqual),
            ('>', '=') => Some(SeperatorKind::GreatEqual),
            ('!', '=') => Some(SeperatorKind::NotEqual),
            ('-', '>') => Some(SeperatorKind::NarrowRightArrow),
            ('=', '>') => Some(SeperatorKind::WideRightArrow),
            (':', ':') => Some(SeperatorKind::NamespaceSeperator),
            (ch ,  _ ) => SeperatorKind::try_from(ch),
        }
    }
}

impl SeperatorKind {
    
    pub fn len(&self) -> usize {
        use self::SeperatorKind::*;
        match *self {
            Add => 1,            // +
            Sub => 1,            // -
            Mul => 1,            // *
            Div => 1,            // /
            Rem => 1,            // %
            AddAssign => 2,      // +=
            SubAssign => 2,      // -=
            MulAssign => 2,      // *=
            DivAssign => 2,      // /=
            RemAssign => 2,      // %=

            BitNot => 1,         // ~
            BitAnd => 1,         // &
            BitOr => 1,          // |
            BitXor => 1,         // ^
            ShiftLeft => 2,      // <<
            ShiftRight => 2,     // >>
            BitAndAssign => 2,   // &=
            BitOrAssign => 2,    // |=
            BitXorAssign => 2,   // ^=
            // ShiftLeftAssign,  // need preview 2, currently not support
            // ShiftRightAssign, // need preview 2, currently not support

            LogicalNot => 1,     // !
            LogicalAnd => 2,     // &&
            LogicalOr => 2,      // ||

            Assign => 1,         // =
            Less => 1,           // <
            Equal => 2,          // ==
            Great => 1,          // >
            LessEqual => 2,      // <=
            GreatEqual => 2,     // >=
            NotEqual => 2,       // !=

            LeftParenthenes => 1,    // (
            RightParenthenes => 1,   // )
            LeftBracket => 1,        // [
            RightBracket => 1,       // ]
            LeftBrace => 1,          // {
            RightBrace => 1,         // }
            Comma => 1,              // :
            Colon => 1,              // ,
            SemiColon => 1,          // ;
            Dot => 1,                // .

            NarrowRightArrow => 2,   // ->
            WideRightArrow => 2,     // =>
            NamespaceSeperator => 2, // ::

            AttributeSign => 1, // #
        }
    }
}

// TODO: implement by simple and more maintainable macros