
// String literal type
// string literal storage, escape and display

use std::fmt;
use common::StringPosition;

// String literal symbol
#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: Option<String>,  // None for invalid
    pub pos: StringPosition,
    pub is_raw: bool,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct StringLiteral {
    pub value: Option<String>,
    pub pos: StringPosition,
    pub is_raw: bool,
}

impl StringLiteral {

    pub fn new<T: Into<Option<String>>>(value: T, pos: StringPosition, is_raw: bool) -> StringLiteral {
        StringLiteral {
            value: value.into(),
            pos: pos,
            is_raw: is_raw,
        }
    }

    #[cfg(test)]
    pub fn new2(value: &str, pos: StringPosition, is_raw: bool) -> StringLiteral {
        StringLiteral {
            value: Some(value.to_owned()),
            pos: pos,
            is_raw: is_raw,
        }
    }
}

impl fmt::Debug for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}tring literal {}", 
            if self.is_raw { "Raw s" } else { "S" },
            match self.value {
                Some(ref value) => format!("{:?} at {:?}", value, self.pos),
                None => format!("at {:?}, invalid", self.pos)
            }
        )
    }
}

impl_display_by_debug!(StringLiteral);
