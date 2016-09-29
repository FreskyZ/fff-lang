
// Character literal

use common::StringPosition;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct CharLiteral {
    pub raw: String,
    pub ch: char,
    pub pos: StringPosition,
    pub has_failed: bool,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct CharLiteral {
    pub raw: String,
    pub ch: char,
    pub pos: StringPosition,
    pub has_failed: bool,
}

impl CharLiteral {

    pub fn from(raw: String, pos: StringPosition) -> CharLiteral {
        CharLiteral {
            raw: raw, 
            ch: ' ',
            pos: pos,
            has_failed: false,
        }
    }
}

use std::fmt;
impl fmt::Debug for CharLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Char literal {:?} at {:?}{}", 
            self.raw, self.pos, 
            if self.has_failed {
                ", has failed".to_owned()
            } else {
                format!(", with value {:?}", self.ch)
            }) 
    }
}
