
// Seperator kind
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SeperatorCategray {
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

            pub fn categray(&self) -> SeperatorCategray {
                match *self {
                    $(
                        $enum_name::$name1 => $category1,
                    )*
                    $(
                        $enum_name::$name2 => $category2,
                    )*
                }
            }
        }
    );
}

define_seperator!{ SeperatorKind,
    [
    //  ch  => var,                 category,
        '=' => Assign,              SeperatorCategray::Assign,
        '-' => Sub,                 SeperatorCategray::Additive,
        '*' => Mul,                 SeperatorCategray::Multiplicative,
        '/' => Div,                 SeperatorCategray::Multiplicative,
        '%' => Rem,                 SeperatorCategray::Multiplicative,
        '~' => BitNot,              SeperatorCategray::Unary,
        '&' => BitAnd,              SeperatorCategray::BitAnd,
        '|' => BitOr,               SeperatorCategray::BitOr,
        '^' => BitXor,              SeperatorCategray::BitXor,
        '!' => LogicalNot,          SeperatorCategray::Unary,
        '<' => Less,                SeperatorCategray::Relational,
        '>' => Great,               SeperatorCategray::Relational,
        '(' => LeftParenthenes,     SeperatorCategray::Seperator,
        ')' => RightParenthenes,    SeperatorCategray::Seperator,
        '[' => LeftBracket,         SeperatorCategray::Seperator,
        ']' => RightBracket,        SeperatorCategray::Seperator,
        '{' => LeftBrace,           SeperatorCategray::Seperator,
        '}' => RightBrace,          SeperatorCategray::Seperator,
        ',' => Comma,               SeperatorCategray::Seperator,
        // ':' => Colon,            SeperatorCategray::Seperator,           
        ';' => SemiColon,           SeperatorCategray::Seperator,
        '.' => Dot,                 SeperatorCategray::Seperator,
        // '#' => AttributeSign,    SeperatorCategray::Seperator,
    ]
    [
    //  ch1, ch2 => var,                categray,
        '=', '=' => Equal,              SeperatorCategray::Equality,
        '+', '=' => AddAssign,          SeperatorCategray::Assign,
        '-', '=' => SubAssign,          SeperatorCategray::Assign,
        '*', '=' => MulAssign,          SeperatorCategray::Assign,
        '/', '=' => DivAssign,          SeperatorCategray::Assign,
        '%', '=' => RemAssign,          SeperatorCategray::Assign,
        '&', '=' => BitAndAssign,       SeperatorCategray::Assign,
        '|', '=' => BitOrAssign,        SeperatorCategray::Assign,
        '^', '=' => BitXorAssign,       SeperatorCategray::Assign,
        '>', '>' => ShiftLeft,          SeperatorCategray::Shift,
        '<', '<' => ShiftRight,         SeperatorCategray::Shift,
        '&', '&' => LogicalAnd,         SeperatorCategray::LogicalAnd,
        '|', '|' => LogicalOr,          SeperatorCategray::LogicalOr,
        '<', '=' => LessEqual,          SeperatorCategray::Relational,
        '>', '=' => GreatEqual,         SeperatorCategray::Relational,
        '!', '=' => NotEqual,           SeperatorCategray::Equality,
        '-', '>' => NarrowRightArrow,   SeperatorCategray::Seperator,
        // '=', '>' => WideRightArrow,  SeperatorCategray::Seperator,
        // ':', ':' => NamespaceSeperator, SeperatorCategray::Seperator,
        '.', '.' => Range,              SeperatorCategray::Seperator,
        '+', '+' => Increase,           SeperatorCategray::Unary,
        '-', '-' => Decrease,           SeperatorCategray::Unary,
    ]
}
