
// Keyword kind
macro_rules! define_keyword {
    (
        $enum_name: ident, 
        $($value: expr => $var_name: ident, $is_prim_type: expr, $is_reserved: expr,)*
    ) => (

        #[derive(Clone, Eq, PartialEq)]
        pub enum $enum_name {
            $(
                $var_name,
            )*
        }

        use std::fmt;
        impl fmt::Debug for $enum_name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    $(
                        $enum_name::$var_name => 
                            write!(f, "`{}`{}{}", 
                                $value,
                                if $is_prim_type { "(Primitive Type)".to_owned() } else { String::new() },
                                if $is_reserved { "(Reserved)".to_owned() } else { String::new() }),
                    )*
                }
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

        impl $enum_name {

            pub fn is_prim_type(&self) -> bool {
                match *self {
                    $(
                        $enum_name::$var_name => $is_prim_type,
                    )*
                }
            }

            pub fn is_reserved(&self) -> bool {
                match *self {
                    $(
                        $enum_name::$var_name => $is_reserved,
                    )*
                }
            }
        }
    );
}

define_keyword!{ KeywordKind, 
//  value,          var_name,       is_prim_type,   is_reserved,
    "fn" =>         FnDef,          false,          false,
    "if" =>         If,             false,          false,
    "else" =>       Else,           false,          false,
    "while" =>      While,          false,          false,
    "break" =>      Break,          false,          false,
    "continue" =>   Continue,       false,          false,
    "for" =>        For,            false,          false,
    "return" =>     Return,         false,          false,
    "var" =>        Var,            false,          false,
    "const" =>      Const,          false,          false,
    "as" =>         As,             false,          false,

    "u8" =>         PrimTypeU8,     true,           false,
    "i32" =>        PrimTypeI32,    true,           false,
    "u32" =>        PrimTypeU32,    true,           false,
    "u64" =>        PrimTypeU64,    true,           false,
    "f32" =>        PrimTypeF32,    true,           false,
    "f64" =>        PrimTypeF64,    true,           false,
    "char" =>       PrimTypeChar,   true,           false,
    "string" =>     PrimTypeString, true,           false,

    "struct" =>     Struct,         false,          true,
    "namespace" =>  Namespace,      false,          true,
    "type" =>       Type,           false,          true,
    "sm" =>         SM,             false,          true,
    "i8" =>         PrimTypeI8,     true,           true,
    "u16" =>        PrimTypeU16,    true,           true,
    "i16" =>        PrimTypeI16,    true,           true,
    "i64" =>        PrimTypeI64,    true,           true,
    "await" =>      Await,          false,          true,
    "async" =>      Async,          false,          true,
    "yield" =>      Yield,          false,          true,
    "public" =>     Public,         false,          true,
    "private" =>    Private,        false,          true,
    "module" =>     Module,         false,          true,
    "mod" =>        Mod,            false,          true,
    "use" =>        Use,            false,          true,
    "using" =>      Using,          false,          true,
    "extern" =>     Extern,         false,          true,
    "default" =>    Default,        false,          true,
    "new" =>        New,            false,          true,
    "delete" =>     Delete,         false,          true,
    "let" =>        Let,            false,          true,
    "def" =>        Def,            false,          true,
    "impl" =>       Impl,           false,          true,
    "tuple" =>      Tuple,          false,          true,
    "trait" =>      Trait,          false,          true,
    "concept" =>    Concept,        false,          true,
    "interface" =>  Interface,      false,          true,
    "class" =>      Class,          false,          true,
    "template" =>   Template,       false,          true,
    "mutable" =>    Mutable,        false,          true,
    "mut" =>        Mut,            false,          true,
    "function" =>   Function,       false,          true,
    "lambda" =>     Lambda,         false,          true,
    "closure" =>    Closure,        false,          true,
    "array" =>      Array,          false,          true,
}
