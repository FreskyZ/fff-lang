///! fff-lang
///! 
///! lexical/keyword

macro_rules! define_keyword2 {
    (
        $kw_type_name: ident,
        inuse: $($inuse_value: expr => $inuse_name: ident),*;
        primitive: $($primitive_value: expr => $primitive_name: ident),*;
        reserved: $($reserved_value: expr => $reserved_name: ident),*;
    ) => (

        #[derive(Eq, PartialEq, Clone, Copy)]
        pub enum $kw_type_name {
            $($inuse_name,)*
            $($primitive_name,)*
            $($reserved_name,)*
        }

        #[derive(Eq, PartialEq)] enum KeywordCategory { InUse, Primitive, Reserved, }
        #[allow(dead_code)] struct KeywordInfo { kw: $kw_type_name, value: &'static str, category: KeywordCategory }

        #[allow(dead_code)] const VALUE_TO_ID: &[&str] = &[
            $($inuse_value,)*
            $($primitive_value,)*
            $($reserved_value,)*
        ];
        const KEYWORD_TO_INFO: &[KeywordInfo] = &[
            $(KeywordInfo{ kw: $kw_type_name::$inuse_name, value: $inuse_value, category: KeywordCategory::InUse },)*
            $(KeywordInfo{ kw: $kw_type_name::$primitive_name, value: $primitive_value, category: KeywordCategory::Primitive },)*
            $(KeywordInfo{ kw: $kw_type_name::$reserved_name, value: $reserved_value, category: KeywordCategory::Reserved },)*
        ];

        impl ::std::fmt::Debug for $kw_type_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                unsafe {
                    write!(f, "{}", KEYWORD_TO_INFO[::std::mem::transmute_copy::<$kw_type_name, u8>(self) as usize].value)
                }
            }
        }
        impl $kw_type_name {

            #[inline]
            pub fn parse(v: &str) -> Option<$kw_type_name> {
                // TODO! TODO! at 17/6/23
                // want it to be mapped and O(1), failed, 
                // want it to be sorted and binray searched and O(log n), failed,
                // currently linear search, make it faster later
                match v {
                    $($inuse_value => Some($kw_type_name::$inuse_name),)*
                    $($primitive_value => Some($kw_type_name::$primitive_name),)*
                    $($reserved_value => Some($kw_type_name::$reserved_name),)*
                    _ => None,
                }
            }

            #[inline]
            pub fn is_primitive(&self) -> bool {
                unsafe {
                    KEYWORD_TO_INFO[::std::mem::transmute_copy::<$kw_type_name, u8>(self) as usize].category == KeywordCategory::Primitive
                }
            }
            #[inline]
            pub fn is_reserved(&self) -> bool {
                unsafe {
                    KEYWORD_TO_INFO[::std::mem::transmute_copy::<$kw_type_name, u8>(self) as usize].category == KeywordCategory::Reserved
                }
            }
        }
    )
}

define_keyword2!{
    Keyword, 
    inuse:
        "_" =>          Underscore,
        "break" =>      Break,
        "const" =>      Const,
        "continue" =>   Continue,
        "else" =>       Else,
        "false" =>      False,
        "fn" =>         Fn,
        "for" =>        For,
        "if" =>         If,
        "in" =>         In,
        "loop" =>       Loop,
        "return" =>     Return,
        "this" =>       This,
        "true" =>       True,
        "type" =>       Type,
        "var" =>        Var,
        "while" =>      While;
    primitive:
        "i8" =>         I8,
        "u8" =>         U8,
        "i16" =>        I16,
        "u16" =>        U16,
        "i32" =>        I32,
        "u32" =>        U32,
        "i64" =>        I64,
        "u64" =>        U64,
        "f32" =>        F32,
        "f64" =>        F64,
        "char" =>       Char,
        "bool" =>       Bool,
        "string" =>     String;
    reserved:
        "bits8" =>      Bits8, 
        "bits16" =>     Bits16,
        "bits32" =>     Bits32,
        "bits64" =>     Bits64,
        "u128" =>       U128,
        "i128" =>       I128,
        "f128" =>       F128,
        "r32" =>        R32,
        "r64" =>        R64,
        "r128" =>       R128,
        "and" =>        And,
        "alias" =>      Alias,
        "as" =>         As,
        "async" =>      Async,
        "auto" =>       Auto,
        "await" =>      Await,
        "base" =>       Base,
        "catch" =>      Catch,
        "class" =>      Class,
        "closure" =>    Closure,
        "concept" =>    Concept,
        "constexpr" =>  ConstExpr,
        "def" =>        Def,
        "default" =>    Default,
        "delete" =>     Delete,
        "enum" =>       Enum,
        "except" =>     Except,
        "explicit" =>   Explicit,
        "extern" =>     Extern,
        "final" =>      Final,
        "finally" =>    Finally,
        "foreach" =>    Foreach,
        "function" =>   Function,
        "goto" =>       Goto,
        "impl" =>       Impl,
        "implicit" =>   Implicit,
        "interface" =>  Interface,
        "internal" =>   Internal,
        "is" =>         Is,
        "lambda" =>     Lambda,
        "let" =>        Let,
        "match" =>      Match,
        "mod" =>        Mod,
        "module" =>     Module,
        "mut" =>        Mut,
        "mutable" =>    Mutable,
        "namespace" =>  Namespace,
        "new" =>        New,
        "nil" =>        Nil,
        "none" =>       SMNone,
        "null" =>       Null,
        "nullptr" =>    Nullptr,
        "object" =>     Object,
        "or" =>         Or,
        "out" =>        Out,
        "override" =>   Override,
        "params" =>     Params,
        "priv" =>       Priv,
        "private" =>    Private,
        "protected" =>  Protected,
        "pub" =>        Pub,
        "public" =>     Public,
        "ref" =>        Ref,
        "ret" =>        Ret,
        "sealed" =>     Sealed,
        "select" =>     Select,
        "self" =>       SMSelf,
        "static" =>     Static,
        "struct" =>     Struct,
        "super" =>      Super,
        "switch" =>     Switch,
        "template" =>   Template,
        "then" =>       Then,
        "throw" =>      Throw,
        "trait" =>      Trait,
        "try" =>        Try,
        "tuple" =>      Tuple,
        "typedef" =>    TypeDef,
        "typeof" =>     Typeof,
        "undefined" =>  Undefined,
        "unsafe" =>     Unsafe,
        "use" =>        Use,
        "using" =>      Using,
        "volatile" =>   Volatile,
        "where" =>      Where,
        "virtual" =>    Virtual,
        "yield" =>      Yield;
    // removed
    //     "array" =>      Array,
    //     "unit" =>       Unit,
    //     "sm" =>         "SM",
}

#[cfg(test)] #[test]
fn keyword_use() {
    
    assert_eq!{ Keyword::parse("fn"), Some(Keyword::Fn) }
    assert_eq!{ Keyword::parse("await"), Some(Keyword::Await) }

    assert_eq!{ format!("{:?}", Keyword::Def), "def" }
    assert_eq!{ format!("{:?}", Keyword::R64), "r64" }

    assert_eq!{ Keyword::I32.is_primitive(), true }
    assert_eq!{ Keyword::While.is_primitive(), false }
    assert_eq!{ Keyword::Await.is_primitive(), false }

    assert_eq!{ Keyword::Fn.is_reserved(), false }
    assert_eq!{ Keyword::R64.is_reserved(), true }
    assert_eq!{ Keyword::Await.is_reserved(), true }
}
