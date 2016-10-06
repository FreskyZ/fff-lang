
// Keyword kind
use std::fmt;
use common::StringPosition;

macro_rules! define_keyword {
    ($enum_name: ident, $($value: expr => $var_name: ident)*) => (

        test_only_attr!{
            test: [derive(Debug, Clone, Eq, PartialEq)]
            not_test: [derive(Debug, Clone)]
            pub enum $enum_name {
                $(
                    $var_name,
                )*
            }
        }

        use common::TryFrom;
        impl <'a> TryFrom<&'a str> for $enum_name {
            
            fn try_from(name: &'a str) -> Option<$enum_name> {
                use self::$enum_name::*;
                match name {
                    $(
                        $value => Some($var_name),
                    )*
                    _ => None,
                }
            }
        }
    )
}

define_keyword!{ KeywordKind,
    "fn" => FnDef
    "if" => If
    "else" => Else
    "while" => While
    "break" => Break
    "continue" => Continue
    "struct" => Struct
    "for" => For
    "return" => Return
    "namespace" => Namespace
    "var" => Var
    "const" => Const
    "u8" => PrimTypeU8
    "i32" => PrimTypeI32
    "u32" => PrimTypeU32
    "u64" => PrimTypeU64
    "f32" => PrimTypeF32
    "f64" => PrimTypeF64
    "char" => PrimTypeChar
    "string" => PrimTypeString
}

impl KeywordKind {

    pub fn is_prim_type(&self) -> bool {
        use self::KeywordKind::*;
        match *self {
            PrimTypeChar | PrimTypeF32 | PrimTypeF64 | PrimTypeI32 
                | PrimTypeString | PrimTypeU32 | PrimTypeU64 | PrimTypeU8 => true,
            _ => false,
        }
    }
}

test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub struct Keyword {
        pub kind: KeywordKind,
        pub pos: StringPosition,
    }
}

impl fmt::Debug for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Keyword {:?} at {:?}", self.kind, self.pos)
    }
}