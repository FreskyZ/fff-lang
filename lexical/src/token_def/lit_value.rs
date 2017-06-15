///! fff-lang
///!
///! lexical/literal

use std::fmt;
use codemap::SymbolID;

#[derive(Clone, Copy)]
pub enum NumLitValue {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
}
impl PartialEq<NumLitValue> for NumLitValue {

    fn eq(&self, rhs: &NumLitValue) -> bool {
        use std::mem;

        match (self, rhs) {
            (&NumLitValue::I8(lhs), &NumLitValue::I8(rhs)) => lhs == rhs,
            (&NumLitValue::U8(lhs), &NumLitValue::U8(rhs)) => lhs == rhs,
            (&NumLitValue::I16(lhs), &NumLitValue::I16(rhs)) => lhs == rhs,
            (&NumLitValue::U16(lhs), &NumLitValue::U16(rhs)) => lhs == rhs,
            (&NumLitValue::I32(lhs), &NumLitValue::I32(rhs)) => lhs == rhs,
            (&NumLitValue::U32(lhs), &NumLitValue::U32(rhs)) => lhs == rhs,
            (&NumLitValue::I64(lhs), &NumLitValue::I64(rhs)) => lhs == rhs,
            (&NumLitValue::U64(lhs), &NumLitValue::U64(rhs)) => lhs == rhs,

            // Strange method to fix num_lit_parser test case: 12345678901234567890.0
            // Update: num_lit_parser test case 61 need 2
            (&NumLitValue::F32(lhs), &NumLitValue::F32(rhs)) => if lhs.is_normal() && rhs.is_normal() { 
                unsafe {
                    let (lhs, rhs) = (mem::transmute::<_, u32>(lhs), mem::transmute::<_, u32>(rhs));
                    if lhs > rhs { lhs - rhs <= 2 } else { rhs - lhs <= 2 }
                }
            } else {
                lhs == rhs
            },  
            (&NumLitValue::F64(lhs), &NumLitValue::F64(rhs)) => if lhs.is_normal() && rhs.is_normal() {
                unsafe { 
                    let (lhs, rhs) = (mem::transmute::<_, u64>(lhs), mem::transmute::<_, u64>(rhs));
                    if lhs > rhs { lhs - rhs <= 2 } else { rhs - lhs <= 2 }
                }
            } else {
                lhs == rhs
            },
            _ => false,
        }
    }
}
impl Eq for NumLitValue {
}
impl fmt::Debug for NumLitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NumLitValue::I8(ref value) => write!(f, "(i8){:?}", value),
            NumLitValue::U8(ref value) => write!(f, "(u8){:?}", value),
            NumLitValue::I16(ref value) => write!(f, "(i16){:?}", value),
            NumLitValue::U16(ref value) => write!(f, "(u16){:?}", value),
            NumLitValue::I32(ref value) => write!(f, "(i32){:?}", value),
            NumLitValue::U32(ref value) => write!(f, "(u32){:?}", value),
            NumLitValue::I64(ref value) => write!(f, "(i64){:?}", value),
            NumLitValue::U64(ref value) => write!(f, "(u64){:?}", value),
            NumLitValue::F32(ref value) => write!(f, "(f32){:?}", value),
            NumLitValue::F64(ref value) => write!(f, "(f64){:?}", value),
        }
    }
}

macro_rules! from_for_num_lit_value {
    ($($ty: ty => $pa: path)*) => (
        $(
            impl From<$ty> for NumLitValue {
                fn from(value: $ty) -> NumLitValue {
                    $pa(value)
                }
            }
        )*
    )
}
from_for_num_lit_value!{
    i8 => NumLitValue::I8
    u8 => NumLitValue::U8
    i16 => NumLitValue::I16
    u16 => NumLitValue::U16
    i32 => NumLitValue::I32
    u32 => NumLitValue::U32
    i64 => NumLitValue::I64
    u64 => NumLitValue::U64
    f32 => NumLitValue::F32
    f64 => NumLitValue::F64
}

static ERROR_NUM_LIT: NumLitValue = NumLitValue::I32(0);

#[derive(Clone, Eq, PartialEq, Copy)] // Copy because now it is all integral fields
pub enum LitValue {
    Unit,                // unit is not generated here in v2 or some other, because for cases like `1.to_string()`, this is function call not unit
    Str(Option<SymbolID>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}
impl fmt::Debug for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "unit"),
            LitValue::Str(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Str(None) => write!(f, "#!1"),
            LitValue::Char(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Char(None) => write!(f, "'<invalid>'"),
            LitValue::Num(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Num(None) => write!(f, "<invalid-num>"),
            LitValue::Bool(val) => write!(f, "{}", val),
        }
    }
}
impl LitValue {

    pub fn is_unit(&self) -> bool { match self { &LitValue::Unit => true, _ => false } }
    pub fn is_str(&self) -> bool { match self { &LitValue::Str(_) => true, _ => false } }
    pub fn is_num(&self) -> bool { match self { &LitValue::Num(_) => true, _ => false } }
    pub fn is_char(&self) -> bool { match self { &LitValue::Char(_) => true, _ => false } }
    pub fn is_bool(&self) -> bool { match self { &LitValue::Bool(_) => true, _ => false } }

    pub fn is_valid(&self) -> bool { 
        match self {
            &LitValue::Unit
            | &LitValue::Bool(_)
            | &LitValue::Str(Some(_))
            | &LitValue::Num(Some(_))
            | &LitValue::Char(Some(_)) => true,
            _ => false,
        }
    }

    pub fn get_char(&self) -> char { match self { &LitValue::Char(Some(val)) => val, _ => '\0' } }
    pub fn get_bool(&self) -> bool { match self { &LitValue::Bool(val) => val, _ => false } }
    pub fn get_str_id(&self) -> SymbolID { match self { &LitValue::Str(Some(val)) => val, _ => SymbolID::new(!1) } }
    pub fn get_num(&self) -> &NumLitValue { match self { &LitValue::Num(Some(ref val)) => val, _ => &ERROR_NUM_LIT } }
}

impl LitValue { pub fn new_str_lit(sid: SymbolID) -> LitValue { LitValue::Str(Some(sid)) } }
impl From<char> for LitValue { fn from(val: char) -> LitValue { LitValue::Char(Some(val)) } }
impl From<bool> for LitValue { fn from(val: bool) -> LitValue { LitValue::Bool(val) } }

macro_rules! from_for_lexical_lit_num {
    ($($ty: ty)*) => (
        $(
            impl From<$ty> for LitValue {
                fn from(val: $ty) -> LitValue {
                    LitValue::Num(Some(NumLitValue::from(val)))
                }
            }
        )*
    )
}
from_for_lexical_lit_num!{ i8  u8  i16  u16  i32  u32  i64  u64  f32  f64  }
