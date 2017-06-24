///! fff-lang
///!
///! lexical/seperator

#[allow(non_snake_case)]
#[allow(non_upper_case_globals)]
pub mod SeperatorCategory {
    pub const         Assign: u16 = 0x0001; // =, +=, -=, *=, /=, %=, &=, ^=, |=
    pub const          Unary: u16 = 0x0002; // -, !, ~
    pub const Multiplicative: u16 = 0x0004; // *, /, %
    pub const       Additive: u16 = 0x0008; // +, -
    pub const          Shift: u16 = 0x0010; // >>, <<
    pub const     Relational: u16 = 0x0020; // <, >, <=, >=
    pub const         BitAnd: u16 = 0x0040; // &
    pub const         BitXor: u16 = 0x0080; // ^
    pub const          BitOr: u16 = 0x0100; // |
    pub const       Equality: u16 = 0x0200; // ==, !=
    pub const     LogicalAnd: u16 = 0x0400; // &&
    pub const      LogicalOr: u16 = 0x0800; // ||
    pub const      Seperator: u16 = 0x1000; // [], (), {}, ,, ;, ->
}
use self::SeperatorCategory as cat;

macro_rules! define_seperator2 {
    (
        $sep_type_name: ident,
        len1:
            $($value1: expr => $name1: ident, $cat1: expr),*;
        len2:
            $($value2: expr => $name2: ident, $cat2: expr),*;
        len3: 
            $($value3: expr => $name3: ident, $cat3: expr),*;
    ) => (

        #[derive(Eq, PartialEq, Copy, Clone)]
        pub enum $sep_type_name {
            $($name1,)*
            $($name2,)*
            $($name3,)*
        }

        struct SepInfo { value: &'static [u8], cat: u16 }
        const SEP_TO_INFO: &[SepInfo] = &[
            $(SepInfo{ value: $value1, cat: $cat1 },)*
            $(SepInfo{ value: $value2, cat: $cat2 },)*
            $(SepInfo{ value: $value3, cat: $cat3 },)*
        ];

        impl ::std::fmt::Debug for $sep_type_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                unsafe {
                    write!(f, "{}", 
                        ::std::mem::transmute::<&[u8], &str>(
                            SEP_TO_INFO[::std::mem::transmute_copy::<$sep_type_name, u8>(self) as usize].value
                        )
                    )
                }
            }
        }
        impl $sep_type_name {

            pub fn parse1(ch: char) -> Option<$sep_type_name> {
                match &[ch as u8] {
                    $($value1 => Some($sep_type_name::$name1),)*
                    _ => None,
                }
            }
            pub fn parse3(ch1: char, ch2: char, ch3: char) -> Option<($sep_type_name, usize)> {
                let (ch1, ch2, ch3) = (ch1 as u8, ch2 as u8, ch3 as u8);
                match &[ch1, ch2, ch3] {
                    $($value3 => Some(($sep_type_name::$name3, 3)),)*
                    _ => match &[ch1, ch2] {
                        $($value2 => Some(($sep_type_name::$name2, 2)),)*
                        _ => match &[ch1] {
                            $($value1 => Some(($sep_type_name::$name1, 1)),)*
                            _ => None,
                        }
                    }
                }
            }

            pub fn is_category(&self, cat: u16) -> bool {
                unsafe {
                    (SEP_TO_INFO[::std::mem::transmute_copy::<$sep_type_name, u8>(self) as usize].cat & cat) == cat
                }
            }
        }
    )
}

define_seperator2! {
    Seperator,
    len1:
        b"="   =>            Assign, cat::Assign,
        b"+"   =>               Add, cat::Additive,
        b"-"   =>               Sub, cat::Additive | cat::Unary, 
        b"*"   =>               Mul, cat::Multiplicative,
        b"/"   =>               Div, cat::Multiplicative,
        b"%"   =>               Rem, cat::Multiplicative,
        b"~"   =>            BitNot, cat::Unary,
        b"&"   =>            BitAnd, cat::BitAnd,
        b"|"   =>             BitOr, cat::BitOr,
        b"^"   =>            BitXor, cat::BitXor,
        b"!"   =>        LogicalNot, cat::Unary,
        b"<"   =>              Less, cat::Relational,
        b">"   =>             Great, cat::Relational,
        b"("   =>   LeftParenthenes, cat::Seperator,
        b")"   =>  RightParenthenes, cat::Seperator,
        b"["   =>       LeftBracket, cat::Seperator,
        b"]"   =>      RightBracket, cat::Seperator,
        b"{"   =>         LeftBrace, cat::Seperator,
        b"}"   =>        RightBrace, cat::Seperator,
        b","   =>             Comma, cat::Seperator,
        b":"   =>             Colon, cat::Seperator,           
        b";"   =>         SemiColon, cat::Seperator,
        b"."   =>               Dot, cat::Seperator;
    len2:
        b"=="  =>              Equal, cat::Equality,
        b"+="  =>          AddAssign, cat::Assign,
        b"-="  =>          SubAssign, cat::Assign,
        b"*="  =>          MulAssign, cat::Assign,
        b"/="  =>          DivAssign, cat::Assign,
        b"%="  =>          RemAssign, cat::Assign,
        b"&="  =>       BitAndAssign, cat::Assign,
        b"|="  =>        BitOrAssign, cat::Assign,
        b"^="  =>       BitXorAssign, cat::Assign,
        b">>"  =>         ShiftRight, cat::Shift,
        b"<<"  =>          ShiftLeft, cat::Shift,
        b"&&"  =>         LogicalAnd, cat::LogicalAnd,
        b"||"  =>          LogicalOr, cat::LogicalOr,
        b"<="  =>          LessEqual, cat::Relational,
        b">="  =>         GreatEqual, cat::Relational,
        b"!="  =>           NotEqual, cat::Equality,
        b"->"  =>   NarrowRightArrow, cat::Seperator,
        b".."  =>              Range, cat::Seperator,
        b"::"  => NamespaceSeperator, cat::Seperator;
    len3:
        b">>=" =>   ShiftRightAssign, cat::Assign,
        b"<<=" =>    ShiftLeftAssign, cat::Assign;
    // future:
        // b"#"   =>   AttributeSign, cat::Seperator,
        // b"=>"  =>  WideRightArrow, cat::Seperator,
}

#[cfg(to_satisfy_stupid_racer_or_rls)] pub enum Seperator { LeftParenthenes, RightParenthenes, LeftBracket, RightBracket, LeftBrace, RightBrace }

#[cfg(test)] #[test]
fn seperator_use() {

    assert_eq!{ Seperator::parse3('<', '<', '='), Some((Seperator::ShiftLeftAssign, 3)) }
    assert_eq!{ Seperator::parse3('+', ' ', '1'), Some((Seperator::Add, 1)) }
    assert_eq!{ Seperator::parse3('{', ' ', 'a'), Some((Seperator::LeftBrace, 1)) }
    assert_eq!{ Seperator::parse3('&', '&', ' '), Some((Seperator::LogicalAnd, 2)) }

    assert_eq!{ Seperator::Add.is_category(SeperatorCategory::Additive), true }
    assert_eq!{ Seperator::Sub.is_category(SeperatorCategory::Additive), true }
    assert_eq!{ Seperator::Sub.is_category(SeperatorCategory::Unary), true }
    assert_eq!{ Seperator::LeftBrace.is_category(SeperatorCategory::Seperator), true }
}