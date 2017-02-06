
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
        [
            $($ch3: expr => $name3: ident, $category31: expr, $category32: expr, )*   // for multiple category...
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
                    $(
                        $enum_name::$name3 => write!(f, "`{}`({})", $ch3, stringify!($name3)),
                    )*
                }
            }
        }
        impl fmt::Display for $enum_name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
                match *self {
                    $(
                        $enum_name::$name1 => write!(f, "operator{}", $ch),
                    )*
                    $(
                        $enum_name::$name2 => write!(f, "operator{}{}", $ch1, $ch2),
                    )*
                    $(
                        $enum_name::$name3 => write!(f, "operator{}", $ch3),
                    )*
                }
            }
        }

        use util::TryFrom;
        impl TryFrom<char> for $enum_name {
            fn try_from(ch: char) -> Option<$enum_name> {
                match ch {
                    $(
                        $ch => Some($enum_name::$name1),
                    )*
                    $(
                        $ch3 => Some($enum_name::$name3),
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
                    $(
                        $enum_name::$name3 => 1,
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
                        $enum_name::$name3 => $category31 == expect || $category32 == expect, 
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
        // ':' => Colon,            SeperatorCategory::Seperator,           
        ';' => SemiColon,           SeperatorCategory::Seperator,
        '.' => Dot,                 SeperatorCategory::Seperator,
        ':' => Range,               SeperatorCategory::Seperator,   // Change from `..` to here because 1..2 is regarded as due decimal point in num lit, a serious problem
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
        '+', '+' => Increase,               SeperatorCategory::Unary,
        '-', '-' => Decrease,               SeperatorCategory::Unary,
    ]
    [
    //  ch,    var,                 category1,                      category2
        '-' => Sub,                 SeperatorCategory::Additive,    SeperatorCategory::Unary, 
    ]
}
