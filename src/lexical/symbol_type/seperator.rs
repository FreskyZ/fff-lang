
// Seperator kind
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
            $($ch: expr => $name1: ident, $category1: expr,)*
        ]
        [
            $($ch1: expr, $ch2: expr => $name2: ident, $category2: expr,)*
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
        }

        use std::fmt;
        impl fmt::Debug for $enum_name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
                match *self {
                    $(
                        $enum_name::$name1 => write!(f, "`{}`({})", $ch, stringify!($name1)),
                    )*
                    $(
                        $enum_name::$name2 => write!(f, "`{}{}`({})", $ch1, $ch2, stringify!($name2)),
                    )*
                }
            }
        }

        use common::TryFrom;
        impl TryFrom<char> for $enum_name {
            fn try_from(ch: char) -> Option<$enum_name> {
                match ch {
                    $(
                        $ch => Some($enum_name::$name1),
                    )*
                    _ => None,
                }
            }
        }

        impl TryFrom<(char, char)> for $enum_name {
            fn try_from(chs: (char, char)) -> Option<$enum_name> {
                match chs {
                    $(
                        ($ch1, $ch2) => Some($enum_name::$name2),
                    )*
                    (ch, _) => $enum_name::try_from(ch),
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
                }
            }

            pub fn category(&self) -> SeperatorCategory {
                match *self {
                    $(
                        $enum_name::$name1 => $category1,
                    )*
                    $(
                        $enum_name::$name2 => $category2,
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
        '-' => Sub,                 SeperatorCategory::Additive,
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
        // ':' => Colon,            SeperatorCategory::Seperator,           
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
        // '=', '>' => WideRightArrow,      SeperatorCategory::Seperator,
        // ':', ':' => NamespaceSeperator,  SeperatorCategory::Seperator,
        '.', '.' => Range,                  SeperatorCategory::Seperator,
        '+', '+' => Increase,               SeperatorCategory::Unary,
        '-', '-' => Decrease,               SeperatorCategory::Unary,
    ]
}
