///! fff-lang
///!
///! lexical/keyword
///! Attention: contens are auto generated by token.py, do not modify this file

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum Keyword {
    Underscore,
    Alias,
    And,
    Array,
    As,
    Async,
    Auto,
    Await,
    Base,
    Bits16,
    Bits32,
    Bits64,
    Bits8,
    Bool,
    Break,
    Catch,
    Char,
    Class,
    Closure,
    Concept,
    Const,
    ConstExpr,
    Continue,
    Def,
    Default,
    Delete,
    Else,
    Enum,
    Except,
    Explicit,
    Extern,
    F128,
    F32,
    F64,
    False,
    Final,
    Finally,
    Fn,
    For,
    Foreach,
    Function,
    Goto,
    I128,
    I16,
    I32,
    I64,
    I8,
    If,
    Impl,
    Implicit,
    In,
    Interface,
    Internal,
    Is,
    Lambda,
    Let,
    Loop,
    Match,
    Mod,
    Module,
    Mut,
    Mutable,
    Namespace,
    New,
    Nil,
    None,
    Null,
    Nullptr,
    Object,
    Or,
    Out,
    Override,
    Params,
    Priv,
    Private,
    Protected,
    Pub,
    Public,
    R128,
    R32,
    R64,
    Ref,
    Ret,
    Return,
    Sealed,
    Select,
    SMSelf,
    Static,
    String,
    Struct,
    Super,
    Switch,
    Template,
    Then,
    This,
    Throw,
    Trait,
    True,
    Try,
    Tuple,
    Type,
    TypeDef,
    Typeof,
    U128,
    U16,
    U32,
    U64,
    U8,
    Undefined,
    Unsafe,
    Use,
    Using,
    Var,
    Virtual,
    Volatile,
    Where,
    While,
    Yield,
}

const KEYWORD_VALUES: &[&str] = &[
    "_", "alias", "and", "array", "as", "async", "auto", 
    "await", "base", "bits16", "bits32", "bits64", "bits8", "bool", 
    "break", "catch", "char", "class", "closure", "concept", "const", 
    "constexpr", "continue", "def", "default", "delete", "else", "enum", 
    "except", "explicit", "extern", "f128", "f32", "f64", "false", 
    "final", "finally", "fn", "for", "foreach", "function", "goto", 
    "i128", "i16", "i32", "i64", "i8", "if", "impl", 
    "implicit", "in", "interface", "internal", "is", "lambda", "let", 
    "loop", "match", "mod", "module", "mut", "mutable", "namespace", 
    "new", "nil", "none", "null", "nullptr", "object", "or", 
    "out", "override", "params", "priv", "private", "protected", "pub", 
    "public", "r128", "r32", "r64", "ref", "ret", "return", 
    "sealed", "select", "self", "static", "string", "struct", "super", 
    "switch", "template", "then", "this", "throw", "trait", "true", 
    "try", "tuple", "type", "typedef", "typeof", "u128", "u16", 
    "u32", "u64", "u8", "undefined", "unsafe", "use", "using", 
    "var", "virtual", "volatile", "where", "while", "yield", 
];

const EMPTY: u8 = 255;
const HASH_MAGIC: u64 = 16557366432705;
const KEYWORD_BUCKET1: &[u8] = &[
    EMPTY, EMPTY, 67, 107, 11, 40, 15, 75, EMPTY, EMPTY, EMPTY, 83, EMPTY, 42, EMPTY, 116, 
    32, EMPTY, 16, 31, 54, 95, 18, EMPTY, 7, 22, EMPTY, 39, 26, 38, EMPTY, EMPTY, 
    EMPTY, 69, 105, EMPTY, 82, 58, 3, 70, 94, 100, 80, EMPTY, 50, 20, EMPTY, 44, 
    EMPTY, EMPTY, EMPTY, 6, 0, EMPTY, 109, 24, 19, EMPTY, 60, 10, EMPTY, 97, EMPTY, EMPTY, 
    EMPTY, 106, 27, 115, EMPTY, 85, EMPTY, 86, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 14, EMPTY, EMPTY, 
    53, EMPTY, 89, EMPTY, EMPTY, 17, 91, 33, EMPTY, 104, 35, EMPTY, 96, 30, 9, EMPTY, 
    47, EMPTY, EMPTY, 62, 25, 63, EMPTY, 23, 88, EMPTY, 13, EMPTY, 99, 71, 45, EMPTY, 
    29, 66, 12, EMPTY, EMPTY, 37, EMPTY, 43, 64, 46, EMPTY, EMPTY, 112, EMPTY, 103, 73, 
    56, 52, 72, EMPTY, 59, EMPTY, EMPTY, 34, 41, 
];
const KEYWORD_BUCKET2: &[u8] = &[
    EMPTY, EMPTY, 77, 79, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 84, EMPTY, EMPTY, EMPTY, 5, 
    EMPTY, EMPTY, 28, EMPTY, 117, EMPTY, EMPTY, EMPTY, EMPTY, 93, EMPTY, EMPTY, 114, EMPTY, EMPTY, EMPTY, 
    EMPTY, 74, 87, EMPTY, EMPTY, EMPTY, 68, 92, 1, 2, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 21, 
    EMPTY, EMPTY, EMPTY, EMPTY, 4, EMPTY, EMPTY, EMPTY, 57, EMPTY, EMPTY, 61, EMPTY, 81, EMPTY, EMPTY, 
    EMPTY, 51, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 90, EMPTY, EMPTY, 
    EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 49, EMPTY, 110, 36, EMPTY, 102, EMPTY, EMPTY, EMPTY, 
    EMPTY, EMPTY, EMPTY, EMPTY, 48, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, 65, EMPTY, EMPTY, EMPTY, 8, EMPTY, 
    EMPTY, 101, 55, EMPTY, EMPTY, 76, EMPTY, EMPTY, EMPTY, 113, EMPTY, EMPTY, 98, EMPTY, EMPTY, 108, 
    EMPTY, EMPTY, 111, EMPTY, 78, EMPTY, EMPTY, EMPTY, EMPTY, 
];
impl Keyword {
    pub fn parse(v: &str) -> Option<Keyword> {
        let mut hash = 1u64;
        for ch in v.chars() {
            if ch as u32 <= 43 { return None; }
            hash = (hash * (ch as u32 - 43u32) as u64) % HASH_MAGIC;
        }
        match KEYWORD_BUCKET1[(hash % 137) as usize] {
            index if index != EMPTY && KEYWORD_VALUES[index as usize] == v
                => Some(unsafe{ ::std::mem::transmute(index) }),
            _empty_or_invalid_key => match KEYWORD_BUCKET2[(hash % 137) as usize] {
                EMPTY => None,
                index if KEYWORD_VALUES[index as usize] == v
                    => Some(unsafe { ::std::mem::transmute(index) }),
                _invalid_index => None,
            },
        }
    }
}
impl ::std::fmt::Debug for Keyword {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", unsafe{ KEYWORD_VALUES[::std::mem::transmute_copy::<Keyword, u8>(self) as usize] })
    }
}

const KEYWORD_CATS: &[u8] = &[
    1, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 
    2, 3, 3, 3, 1, 3, 1, 3, 3, 3, 1, 3, 3, 3, 3, 3, 
    2, 2, 1, 3, 3, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 1, 
    3, 3, 1, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 3, 3, 
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
    3, 3, 3, 1, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 1, 3, 
    3, 1, 3, 3, 1, 3, 3, 3, 2, 2, 2, 2, 3, 3, 3, 3, 
    1, 3, 3, 3, 1, 3, 
];
impl Keyword {
    pub fn is_primitive(&self) -> bool {
        KEYWORD_CATS[unsafe{ ::std::mem::transmute_copy::<Keyword, u8>(self) as usize }] == 2
    }
    pub fn is_reserved(&self) -> bool {
        KEYWORD_CATS[unsafe{ ::std::mem::transmute_copy::<Keyword, u8>(self) as usize }] == 3
    }
}

#[cfg(test)] #[test]
fn keyword_format() {

    assert_eq!{ format!("{:?}", Keyword::Delete), "delete" }
    assert_eq!{ format!("{:?}", Keyword::Sealed), "sealed" }
    assert_eq!{ format!("{:?}", Keyword::String), "string" }
    assert_eq!{ format!("{:?}", Keyword::R32), "r32" }
    assert_eq!{ format!("{:?}", Keyword::Sealed), "sealed" }
    assert_eq!{ format!("{:?}", Keyword::Bits32), "bits32" }
    assert_eq!{ format!("{:?}", Keyword::Except), "except" }
    assert_eq!{ format!("{:?}", Keyword::Then), "then" }
    assert_eq!{ format!("{:?}", Keyword::Namespace), "namespace" }
    assert_eq!{ format!("{:?}", Keyword::Await), "await" }
}
#[cfg(test)] #[test]
fn keyword_cat() {

    assert_eq!{ Keyword::I32.is_primitive(), true }
    assert_eq!{ Keyword::Continue.is_reserved(), false }
    assert_eq!{ Keyword::U32.is_primitive(), true }
    assert_eq!{ Keyword::Fn.is_primitive(), false }
    assert_eq!{ Keyword::Fn.is_reserved(), false }
    assert_eq!{ Keyword::String.is_primitive(), true }
    assert_eq!{ Keyword::Continue.is_primitive(), false }
    assert_eq!{ Keyword::R32.is_primitive(), false }
    assert_eq!{ Keyword::U32.is_reserved(), false }
    assert_eq!{ Keyword::F64.is_reserved(), false }
    assert_eq!{ Keyword::Namespace.is_reserved(), true }
}
#[cfg(test)] #[test]
fn keyword_parse() {

    assert_eq!{ Keyword::parse("fn"), Some(Keyword::Fn) }
    assert_eq!{ Keyword::parse("await"), Some(Keyword::Await) }
    assert_eq!{ Keyword::parse("一个chinese变量"), None }
    assert_eq!{ Keyword::parse("a_中文_var"), None }
    assert_eq!{ Keyword::parse("as"), Some(Keyword::As) }
}
