
// Character literal

use std::fmt;
use lexical_pos::StringPosition;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct CharLiteral {
    pub value: Option<char>, // None for invalid
    pub pos: StringPosition,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct CharLiteral {
    pub value: Option<char>,
    pub pos: StringPosition,
}

impl fmt::Debug for CharLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            Some(value) => write!(f, "Char literal {:?} at {:?}", value, self.pos),
            None => write!(f, "Char literal at {:?}, invalid", self.pos)
        }
    }
}

