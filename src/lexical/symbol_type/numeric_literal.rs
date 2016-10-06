
// Numeric Literal

use std::fmt;
use common::StringPosition;

#[cfg(test)]
#[derive(PartialEq, Clone)]
pub enum NumericLiteralValue {
    U64(u64),
    U32(u32),
    I32(i32),
    U8(u8),
    F32(f32),
    F64(f64),
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum NumericLiteralValue {
    U64(u64),
    U32(u32),
    I32(i32),
    U8(u8),
    F32(f32),
    F64(f64),
}
#[cfg(test)]
impl Eq for NumericLiteralValue {
}

macro_rules! from_for_num_lit_value {
    ($($ty: ty => $pa: path)*) => (
        $(
            impl From<$ty> for NumericLiteralValue {
                fn from(value: $ty) -> NumericLiteralValue {
                    $pa(value)
                }
            }
        )*
    )
}
from_for_num_lit_value!{
    u8 => NumericLiteralValue::U8
    i32 => NumericLiteralValue::I32
    u32 => NumericLiteralValue::U32
    u64 => NumericLiteralValue::U64
    f32 => NumericLiteralValue::F32
    f64 => NumericLiteralValue::F64
}

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct NumericLiteral {
    pub value: Option<NumericLiteralValue>,
    pub pos: StringPosition,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct NumericLiteral {
    pub value: Option<NumericLiteralValue>,
    pub pos: StringPosition,
}

impl fmt::Debug for NumericLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NumericLiteral at {:?}{}",
            self.pos,
            match self.value {
                Some(NumericLiteralValue::U64(ref value)) => format!(", with value (u64){:?}", value),
                Some(NumericLiteralValue::U32(ref value)) => format!(", with value (u32){:?}", value),
                Some(NumericLiteralValue::I32(ref value)) => format!(", with value (i32){:?}", value),
                Some(NumericLiteralValue::U8(ref value)) => format!(", with value (u8){:?}", value),
                Some(NumericLiteralValue::F32(ref value)) => format!(", with value (f32){:?}", value),
                Some(NumericLiteralValue::F64(ref value)) => format!(", with value (f64){:?}", value),
                None => ", invalid".to_owned(),
            }
        )
    }
}

impl NumericLiteral {
}