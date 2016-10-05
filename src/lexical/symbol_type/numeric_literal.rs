
// Numeric Literal

use std::fmt;
use common::StringPosition;
use message::MessageEmitter;

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

    #[cfg(test)]
    pub fn new(_raw: &str, pos: StringPosition, _messages: &mut MessageEmitter) -> NumericLiteral {
        NumericLiteral{
            value: Some(NumericLiteralValue::I32(0)),
            pos: pos
        }
    }
}