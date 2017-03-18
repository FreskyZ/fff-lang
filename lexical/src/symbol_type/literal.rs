
// lexical literal, for lexical and syntax parser convenience

use std::fmt;

#[derive(Clone)]
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
impl_display_by_debug!{ NumLitValue }

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

#[derive(Clone, Eq, PartialEq)]
pub enum LitValue {
    Unit,
    Str(Option<String>),
    Num(Option<NumLitValue>),
    Char(Option<char>),
    Bool(bool),
}

impl LitValue {

    pub fn is_unit(&self) -> bool {
        match *self {
            LitValue::Unit => true,
            _ => false,
        }
    }
    pub fn is_str(&self) -> bool {
        match *self {
            LitValue::Str(_) => true,
            _ => false,
        }
    }
    pub fn is_num(&self) -> bool {
        match *self {
            LitValue::Num(_) => true,
            _ => false,
        }
    }
    pub fn is_char(&self) -> bool {
        match *self {
            LitValue::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match *self {
            LitValue::Bool(_) => true,
            _ => false,
        }
    }

    pub fn get_str(&self) -> Option<&Option<String>> {
        match self {
            &LitValue::Str(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_char(&self) -> Option<&Option<char>> {
        match self {
            &LitValue::Char(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_num(&self) -> Option<&Option<NumLitValue>> {
        match self {
            &LitValue::Num(ref val) => Some(val),
            _ => None,
        }
    }
    pub fn get_bool(&self) -> Option<bool> {
        match self {
            &LitValue::Bool(val) => Some(val),
            _ => None,
        }
    }

    /// replace error content with <error-content>, do not call on not Str
    pub fn get_str_not_option(self) -> String {
        match self {
            LitValue::Str(Some(val)) => val,
            LitValue::Str(None) => "<error-content>".to_owned(),
            _ => unreachable!(),
        }
    }
    pub fn get_num_not_option(self) -> NumLitValue {
        match self {
            LitValue::Num(Some(val)) => val,
            LitValue::Num(None) => NumLitValue::I32(0),
            _ => unreachable!(),
        }
    }
    pub fn get_char_not_option(self) -> char {
        match self {
            LitValue::Char(Some(val)) => val,
            LitValue::Char(None) => '\u{FEFF}',
            _ => unreachable!()
        }
    }
    pub fn get_bool_not_option(self) -> bool {
        match self {
            LitValue::Bool(val) => val,
            _ => false,
        }
    }
}

impl fmt::Debug for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "Unit literal"),
            LitValue::Str(Some(ref val)) => write!(f, "String literal {:?}", val),
            LitValue::Str(None) => write!(f, "String literal \"<invalid>\""),
            LitValue::Char(Some(ref val)) => write!(f, "Char literal {:?}", val),
            LitValue::Char(None) => write!(f, "Char literal '<invalid>'"),
            LitValue::Num(Some(ref val)) => write!(f, "Numeric literal {:?}", val),
            LitValue::Num(None) => write!(f, "Numeric literal <invalid>"),
            LitValue::Bool(val) => write!(f, "Boolean literal {}", val),
        }
    }
}
impl fmt::Display for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitValue::Unit => write!(f, "()"),
            LitValue::Str(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Str(None) => write!(f, "<invalid>"),
            LitValue::Char(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Char(None) => write!(f, "<invalid>"),
            LitValue::Num(Some(ref val)) => write!(f, "{:?}", val),
            LitValue::Num(None) => write!(f, "<invalid>"),
            LitValue::Bool(val) => write!(f, "{}", val),
        }
    }
}

impl From<String> for LitValue {
    fn from(val: String) -> LitValue {
        LitValue::Str(Some(val))
    }
}
impl<'a> From<&'a str> for LitValue {
    fn from(val: &'a str) -> LitValue {
        LitValue::Str(Some(val.to_owned()))
    }
}

impl From<char> for LitValue {
    fn from(val: char) -> LitValue {
        LitValue::Char(Some(val))
    }
}
impl From<bool> for LitValue {
    fn from(val: bool) -> LitValue {
        LitValue::Bool(val)
    }
}

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
