
// Numeric Literal
// TODO imm: Rename NumLitValue to NumLitVal!!!

use std::fmt;
use lexical_pos::StringPosition;

#[derive(PartialEq, Clone)]
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

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct NumericLiteral {
    pub value: Option<NumLitValue>,
    pub pos: StringPosition,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct NumericLiteral {
    pub value: Option<NumLitValue>,
    pub pos: StringPosition,
}

impl fmt::Debug for NumericLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NumLit {} @ {:?}",
            match self.value {
                Some(ref value) => format!("{:?}", value),
                None => format!("<invalid>"),
            },
            self.pos,
        )
    }
}

impl NumericLiteral {
}