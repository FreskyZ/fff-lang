///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

#[repr(u8)]
#[allow(dead_code)]
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum KeywordKind {
    Reserved = 0,
    Primitive = 1,
    MaybeIdentifier = 2,
    Normal = 3,
}

#[repr(u8)]
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Keyword {
    Underscore = 1,
    As = 2,
    Async = 3,
    Await = 4,
    Bits16 = 5,
    Bits32 = 6,
    Bits64 = 7,
    Bits8 = 8,
    Bool = 9,
    Break = 10,
    Catch = 11,
    Char = 12,
    Class = 13,
    Const = 14,
    Continue = 15,
    Else = 16,
    Enum = 17,
    Extern = 18,
    F128 = 19,
    F32 = 20,
    F64 = 21,
    False = 22,
    Finally = 23,
    Fn = 24,
    For = 25,
    Goto = 26,
    I128 = 27,
    I16 = 28,
    I32 = 29,
    I64 = 30,
    I8 = 31,
    If = 32,
    Impl = 33,
    In = 34,
    Interface = 35,
    Internal = 36,
    Is = 37,
    Let = 38,
    Loop = 39,
    Match = 40,
    Module = 41,
    Mut = 42,
    Namespace = 43,
    Null = 44,
    Override = 45,
    Private = 46,
    Protected = 47,
    Pub = 48,
    Public = 49,
    R128 = 50,
    R32 = 51,
    R64 = 52,
    Ref = 53,
    Ret = 54,
    Return = 55,
    SelfL = 56,
    SelfU = 57,
    Static = 58,
    Struct = 59,
    Super = 60,
    Switch = 61,
    Throw = 62,
    Trait = 63,
    True = 64,
    Try = 65,
    Type = 66,
    TypeDef = 67,
    Typeof = 68,
    U128 = 69,
    U16 = 70,
    U32 = 71,
    U64 = 72,
    U8 = 73,
    Unsafe = 74,
    Use = 75,
    Var = 76,
    Virtual = 77,
    Volatile = 78,
    Where = 79,
    While = 80,
    Yield = 81,
}

const VALUES: &[&str] = &[
    "", "_", "as", "async", "await", "bits16", "bits32", "bits64", "bits8", "bool",
    "break", "catch", "char", "class", "const", "continue", "else", "enum", "extern",
    "f128", "f32", "f64", "false", "finally", "fn", "for", "goto", "i128", "i16",
    "i32", "i64", "i8", "if", "impl", "in", "interface", "internal", "is", "let",
    "loop", "match", "module", "mut", "namespace", "null", "override", "private",
    "protected", "pub", "public", "r128", "r32", "r64", "ref", "ret", "return", "self",
    "Self", "static", "struct", "super", "switch", "throw", "trait", "true", "try",
    "type", "typedef", "typeof", "u128", "u16", "u32", "u64", "u8", "unsafe", "use",
    "var", "virtual", "volatile", "where", "while", "yield",
];
const KINDS: &[u8] = &[
    0, 2, 3, 0, 0, 0, 0, 0, 0, 1, 3, 0, 1, 3, 3, 3, 3, 3, 0, 0, 1, 1, 2, 0, 3, 3,
    0, 1, 1, 1, 1, 1, 3, 3, 3, 0, 0, 0, 0, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
    1, 0, 0, 3, 2, 2, 0, 3, 0, 0, 0, 0, 2, 0, 3, 0, 0, 1, 1, 1, 1, 1, 0, 3, 3, 0,
    0, 3, 3, 0,
];

impl Keyword {

    pub fn display(self) -> &'static str {
        VALUES[self as u8 as usize]
    }

    pub fn kind(self, kind: KeywordKind) -> bool {
        KINDS[self as u8 as usize] == kind as u8
    }
}

use Keyword::*;
const BUCKETS: &[&[&[Option<Keyword>]]] = &[&[], &[],
    /* 2 */ &[
        &[Some(I8), None, None, Some(As), Some(Fn), None, Some(If), None],
        &[Some(U8), None, None, Some(Is), None, None, Some(In), None],
    ],
    /* 3 */ &[
        &[
            Some(Use), Some(Let), None, None, Some(Var), Some(Mut), Some(F64), None,
            Some(I16), None, None, Some(U16), None, Some(F32), None, Some(Pub), Some(R32),
            Some(For), None, None, Some(I64), Some(Ref), None,
        ],
        &[
            None, Some(Ret), None, None, Some(Try), None, None, None, Some(I32),
            None, None, Some(U32), None, None, None, None, None, Some(R64), None,
            None, Some(U64), None, None,
        ],
    ],
    /* 4 */ &[
        &[
            None, None, None, None, None, Some(Enum), None, None, None, None, None,
            Some(True), Some(Type), Some(U128), Some(Else), Some(R128), None, None,
            Some(SelfL), Some(SelfU), None, Some(F128), Some(Impl), Some(Loop), Some(Null),
        ],
        &[
            None, None, None, None, None, None, None, None, None, None, None, Some(Char),
            Some(Bool), None, Some(Goto), None, None, None, None, Some(I128), None,
            None, None, None, None,
        ],
    ],
    /* 5 */ &[
        &[
            Some(Bits8), Some(Super), Some(Where), Some(Async), Some(Catch), Some(Await),
            Some(While), None, None, Some(Throw), None, Some(Const), Some(Match),
            None, None, Some(False), None, None, None, None, Some(Trait), Some(Class),
            None, None, Some(Yield), Some(Break),
        ],
    ],
    /* 6 */ &[
        &[
            Some(Bits64), Some(Extern), None, Some(Bits16), Some(Module), None, None,
            Some(Typeof), Some(Unsafe), None, Some(Public), Some(Return),
        ],
        &[
            None, None, None, Some(Bits32), Some(Switch), None, None, None, None,
            None, Some(Static), Some(Struct),
        ],
    ],
    /* 7 */ &[
        &[
            None, None, Some(Finally), Some(Private), None, None, None, Some(TypeDef),
            Some(Virtual),
        ],
    ],
    /* 8 */ &[
        &[Some(Internal), Some(Volatile), Some(Override), None, Some(Continue)],
    ],
    /* 9 */ &[
        &[Some(Interface), Some(Protected), Some(Namespace), None],
    ],
];

impl Keyword {

    pub fn parse(v: &str) -> Option<Keyword> {
        debug_assert!(!v.is_empty(), "empty ident");
        if v == "_" { return Some(Underscore); }
        if v.len() < 2 || v.len() > 9 { return None; }
        let hash = v.as_bytes().iter().fold(1usize, |acc, &b| acc * if b > 96 { b as usize - 96 } else if b > 48 { b as usize - 48 } else { b as usize } % 173);
        let buckets = &BUCKETS[v.len()];
        let hash = hash as usize % buckets[0].len();
        buckets.iter().filter_map(|bucket| bucket[hash].and_then(|i| if i.display() == v { Some(i) } else { None })).next()
    }
}
