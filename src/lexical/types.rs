
// Common public type defs
// May move string literal, numeric literal and identifier def here

#[derive(Debug, Clone)]
pub enum Keyword {
    FnDef,
    If,
    Else,
    While,
    Break,
    Continue,
    Struct,
}

impl Keyword {

    pub fn from(name: &str) -> Option<Keyword> {
        match name {
            "fn" => Some(Keyword::FnDef),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "struct" => Some(Keyword::Struct),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul, 
    Div, 
    Rem,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,

    Not,
    BitAnd, 
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    // ShiftLeftAssign,  // need preview 2, currently not support
    // ShiftRightAssign, // need preview 2, currently not support

    LogicalAnd,
    LogicalOr,

    Less,
    Equal,
    Great,
    LessEqual,
    GreatEqual,
    NotEqual,

    // Not continuous, consider later
    FunctionCall,
    Index, 
}

impl Operator {
    
    pub fn from1(ch: char) -> Option<Operator> {
        match ch {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '%' => Some(Operator::Rem),
            '!' => Some(Operator::Not),
            '&' => Some(Operator::BitAnd),
            '|' => Some(Operator::BitOr),
            '^' => Some(Operator::BitXor),
            '<' => Some(Operator::Less),
            '>' => Some(Operator::Great),
            _ => None,
        }
    }

    pub fn from2(ch1: char, ch2: char) -> Option<Operator> {
        match (ch1, ch2) {
            ('+', '=') => Some(Operator::AddAssign),
            ('-', '=') => Some(Operator::SubAssign),
            ('*', '=') => Some(Operator::MulAssign),
            ('/', '=') => Some(Operator::DivAssign),
            ('%', '=') => Some(Operator::RemAssign),

            ('>', '>') => Some(Operator::ShiftRight),
            ('<', '<') => Some(Operator::ShiftRight),
            ('&', '&') => Some(Operator::LogicalAnd),
            ('|', '|') => Some(Operator::LogicalOr),

            ('<', '=') => Some(Operator::LessEqual),
            ('>', '=') => Some(Operator::GreatEqual),
            ('!', '=') => Some(Operator::NotEqual),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Seperator {
    WhiteSpace,
    LeftParenthenes,
    RightParenthenes,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    SemiColon,
}

impl Seperator {

    pub fn from(ch: char) -> Option<Seperator> {
        match ch {
            '(' => Some(Seperator::LeftParenthenes),
            ')' => Some(Seperator::RightParenthenes),
            '[' => Some(Seperator::LeftBracket),
            ']' => Some(Seperator::RightBracket),
            '{' => Some(Seperator::LeftBrace),
            '}' => Some(Seperator::RightBrace),
            ',' => Some(Seperator::Comma),
            ':' => Some(Seperator::Colon),
            ';' => Some(Seperator::SemiColon),
            ' ' | '\t' | '\n' => Some(Seperator::WhiteSpace),
            _ => None,
        }
    }
}