
// Seperator kind
macro_rules! define_seperator {
    (
        $enum_name: ident,
        [
            $($ch: expr => $name1: ident)*
        ]
        [
            $($ch1: expr, $ch2: expr => $name2: ident)*
        ]
    ) => (
        test_only_attr!{
            [derive(Clone, Eq, PartialEq)]
            ![derive(Clone, Eq, PartialEq)]
            pub enum $enum_name {
                $(
                    $name1, 
                )*
                $(
                    $name2,
                )*
            }
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
        }
    )
}

define_seperator!{
    SeperatorKind,
    [
        '=' => Assign
        '-' => Sub
        '*' => Mul
        '/' => Div
        '%' => Rem
        '~' => BitNot
        '&' => BitAnd
        '|' => BitOr
        '^' => BitXor
        '!' => LogicalNot
        '<' => Less
        '>' => Great
        '(' => LeftParenthenes
        ')' => RightParenthenes
        '[' => LeftBracket
        ']' => RightBracket
        '{' => LeftBrace
        '}' => RightBrace
        ',' => Comma
        ':' => Colon
        ';' => SemiColon
        '.' => Dot
        '#' => AttributeSign
    ]
    [
        '=', '=' => Equal
        '+', '=' => AddAssign
        '-', '=' => SubAssign
        '*', '=' => MulAssign
        '/', '=' => DivAssign
        '%', '=' => RemAssign
        '>', '>' => ShiftLeft
        '<', '<' => ShiftRight
        '&', '&' => LogicalAnd
        '|', '|' => LogicalOr
        '<', '=' => LessEqual
        '>', '=' => GreatEqual
        '!', '=' => NotEqual
        '-', '>' => NarrowRightArrow
        '=', '>' => WideRightArrow
        ':', ':' => NamespaceSeperator
        '.', '.' => Range
    ]
}
