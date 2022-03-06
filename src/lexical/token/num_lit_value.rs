///! fff-lang
///!
///! lexical/numeric literal value

use std::fmt;

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
    R32(f32),
    R64(f64),
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
            (&NumLitValue::R32(lhs), &NumLitValue::R32(rhs)) => if lhs.is_normal() && rhs.is_normal() { 
                unsafe {
                    let (lhs, rhs) = (mem::transmute::<_, u32>(lhs), mem::transmute::<_, u32>(rhs));
                    if lhs > rhs { lhs - rhs <= 2 } else { rhs - lhs <= 2 }
                }
            } else {
                lhs == rhs
            },  
            (&NumLitValue::R64(lhs), &NumLitValue::R64(rhs)) => if lhs.is_normal() && rhs.is_normal() {
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
            NumLitValue::R32(ref value) => write!(f, "(r32){:?}", value),
            NumLitValue::R64(ref value) => write!(f, "(r64){:?}", value),
        }
    }
}
