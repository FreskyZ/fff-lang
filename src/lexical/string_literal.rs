
// String literal type
// string literal storage, escape and display

use position::StringPosition;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub pos: StringPosition,
    pub is_raw: bool,
    pub has_failed: bool
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct StringLiteral {
    pub value: String,
    pub pos: StringPosition,
    pub is_raw: bool,
    pub has_failed: bool
}

impl StringLiteral {

    pub fn new(raw: String, pos: StringPosition, is_raw: bool, has_failed: bool) -> StringLiteral {
        // TODO: escape unicode character

        StringLiteral {
            value: raw,
            pos: pos,
            is_raw: is_raw,
            has_failed: has_failed,
        }
    }
}

use std::fmt;
impl fmt::Debug for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}tring literal {:?} at {:?}{}", 
            if self.is_raw { "Raw s" } else { "S" }, 
            self.value, 
            self.pos, 
            if self.has_failed { ", has failed" } else { "" }
        )
    }
}

impl_display_by_debug!(StringLiteral);