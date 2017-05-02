///! fff-lang
///!
///! lexical/seperator

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SeperatorCategory {
    Assign,     // =, +=, -=, *=, /=, %=, &=, ^=, |=
    Unary,      // ++, --, -, !, ~
    Multiplicative,  // *, /, %
    Additive,   // +, -
    Shift,      // >>, <<
    Relational, // <, >, <=, >=
    BitAnd,     // &
    BitXor,     // ^
    BitOr,      // |
    Equality,   // ==, !=
    LogicalAnd, // &&
    LogicalOr,  // ||
    Seperator,  // [], (), {}, ,, ;, ->
}

macro_rules! define_seperator {
    (
        $enum_name: ident,
        [
            $($ch1: expr => $name1: ident, $category1: expr,)*
        ]
        [
            $($ch21: expr, $ch22: expr => $name2: ident, $category2: expr,)*
        ]
        [
            $($ch31: expr, $ch32: expr, $ch33: expr => $name3: ident, $category3: expr, )*
        ]
        [
            $($ch4: expr => $name4: ident, $category41: expr, $category42: expr, )*   // for multiple category...
        ]
    ) => (
        #[derive(Clone, Eq, PartialEq)]
        pub enum $enum_name {
            $(
                $name1, 
            )*
            $(
                $name2,
            )*
            $(
                $name3,
            )*
            $(
                $name4,
            )*
        }

        use std::fmt;
        impl fmt::Debug for $enum_name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
                match *self {
                    $(
                        $enum_name::$name1 => write!(f, "`{}`({})", $ch1, stringify!($name1)),
                    )*
                    $(
                        $enum_name::$name2 => write!(f, "`{}{}`({})", $ch21, $ch22, stringify!($name2)),
                    )*
                    $(
                        $enum_name::$name3 => write!(f, "`{}{}{}`({})", $ch31, $ch32, $ch33, stringify!($name3)),
                    )*
                    $(
                        $enum_name::$name4 => write!(f, "`{}`({})", $ch4, stringify!($name4)),
                    )*
                }
            }
        }
        impl fmt::Display for $enum_name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
                match *self {
                    $(
                        $enum_name::$name1 => write!(f, "{}", $ch1),
                    )*
                    $(
                        $enum_name::$name2 => write!(f, "{}{}", $ch21, $ch22),
                    )*
                    $(
                        $enum_name::$name3 => write!(f, "{}{}{}", $ch32, $ch32, $ch33),
                    )*
                    $(
                        $enum_name::$name4 => write!(f, "{}", $ch4),
                    )*
                }
            }
        }

        impl $enum_name {
            pub fn try_from1(ch: char) -> Option<($enum_name, usize)> {
                match ch {
                    $(
                        $ch1 => Some(($enum_name::$name1, 1)),
                    )*
                    $(
                        $ch4 => Some(($enum_name::$name4, 1)),
                    )*
                    _ => None,
                }
            }

            pub fn try_from2(ch1: char, ch2: char) -> Option<($enum_name, usize)> {
                match (ch1, ch2) {
                    $(
                        ($ch21, $ch22) => Some(($enum_name::$name2, 2)),
                    )*
                    (ch, _) => $enum_name::try_from1(ch),
                }
            }

            pub fn try_from3(ch1: char, ch2: char, ch3: char) -> Option<($enum_name, usize)> {
                match (ch1, ch2, ch3) {
                    $(
                        ($ch31, $ch32, $ch33) => Some(($enum_name::$name3, 3)),
                    )*
                    (ch1, ch2, _) => $enum_name::try_from2(ch1, ch2),
                }
            }
        }

        impl $enum_name {
            pub fn len(&self) -> usize {
                match *self {
                    $(
                        $enum_name::$name1 => 1,
                    )*
                    $(
                        $enum_name::$name2 => 2,
                    )*
                    $(
                        $enum_name::$name3 => 3,
                    )*
                    $(
                        $enum_name::$name4 => 1,
                    )*
                }
            }

            pub fn is_category(&self, expect: SeperatorCategory) -> bool {
                match *self {
                    $(
                        $enum_name::$name1 => $category1 == expect,
                    )*
                    $(
                        $enum_name::$name2 => $category2 == expect,
                    )*
                    $(
                        $enum_name::$name3 => $category3 == expect,
                    )*
                    $(
                        $enum_name::$name4 => $category41 == expect || $category42 == expect, 
                    )*
                }
            }
        }
    );
}

define_seperator!{ SeperatorKind,
    [
    //  ch  => var,                 category,
        '=' => Assign,              SeperatorCategory::Assign,
        '+' => Add,                 SeperatorCategory::Additive,
        '*' => Mul,                 SeperatorCategory::Multiplicative,
        '/' => Div,                 SeperatorCategory::Multiplicative,
        '%' => Rem,                 SeperatorCategory::Multiplicative,
        '~' => BitNot,              SeperatorCategory::Unary,
        '&' => BitAnd,              SeperatorCategory::BitAnd,
        '|' => BitOr,               SeperatorCategory::BitOr,
        '^' => BitXor,              SeperatorCategory::BitXor,
        '!' => LogicalNot,          SeperatorCategory::Unary,
        '<' => Less,                SeperatorCategory::Relational,
        '>' => Great,               SeperatorCategory::Relational,
        '(' => LeftParenthenes,     SeperatorCategory::Seperator,
        ')' => RightParenthenes,    SeperatorCategory::Seperator,
        '[' => LeftBracket,         SeperatorCategory::Seperator,
        ']' => RightBracket,        SeperatorCategory::Seperator,
        '{' => LeftBrace,           SeperatorCategory::Seperator,
        '}' => RightBrace,          SeperatorCategory::Seperator,
        ',' => Comma,               SeperatorCategory::Seperator,
        ':' => Colon,               SeperatorCategory::Seperator,           
        ';' => SemiColon,           SeperatorCategory::Seperator,
        '.' => Dot,                 SeperatorCategory::Seperator,
        // '#' => AttributeSign,    SeperatorCategory::Seperator,
    ]
    [
    //  ch1, ch2 => var,                    categray,
        '=', '=' => Equal,                  SeperatorCategory::Equality,
        '+', '=' => AddAssign,              SeperatorCategory::Assign,
        '-', '=' => SubAssign,              SeperatorCategory::Assign,
        '*', '=' => MulAssign,              SeperatorCategory::Assign,
        '/', '=' => DivAssign,              SeperatorCategory::Assign,
        '%', '=' => RemAssign,              SeperatorCategory::Assign,
        '&', '=' => BitAndAssign,           SeperatorCategory::Assign,
        '|', '=' => BitOrAssign,            SeperatorCategory::Assign,
        '^', '=' => BitXorAssign,           SeperatorCategory::Assign,
        '>', '>' => ShiftRight,             SeperatorCategory::Shift,
        '<', '<' => ShiftLeft,              SeperatorCategory::Shift,
        '&', '&' => LogicalAnd,             SeperatorCategory::LogicalAnd,
        '|', '|' => LogicalOr,              SeperatorCategory::LogicalOr,
        '<', '=' => LessEqual,              SeperatorCategory::Relational,
        '>', '=' => GreatEqual,             SeperatorCategory::Relational,
        '!', '=' => NotEqual,               SeperatorCategory::Equality,
        '-', '>' => NarrowRightArrow,       SeperatorCategory::Seperator,
        '.', '.' => Range,                  SeperatorCategory::Seperator,
        // '+', '+' => Increase,               SeperatorCategory::Unary,  // removed, goodbye
        // '-', '-' => Decrease,               SeperatorCategory::Unary,  // removed, goodbye
        // '=', '>' => WideRightArrow,      SeperatorCategory::Seperator,
        // ':', ':' => NamespaceSeperator,  SeperatorCategory::Seperator,
    ]
    [
    //  ch1, ch2, ch3 => var,               category
        '>', '>', '=' => ShiftRightAssign,  SeperatorCategory::Assign,
        '<', '<', '=' => ShiftLeftAssign,   SeperatorCategory::Assign,
    ]
    [
    //  ch,    var,                 category1,                      category2
        '-' => Sub,                 SeperatorCategory::Additive,    SeperatorCategory::Unary, 
    ]
}
