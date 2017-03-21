///! fff-lang
///
/// codemap/codechar

use std::fmt;

use codepos::Position;

pub const EOFCHAR: char = 0u8 as char;
pub const EOFSCHAR: char = 255u8 as char;

// Char and pos
#[derive(Eq, PartialEq, Clone)]
pub struct CodeChar {
    m_ch: char, 
    m_pos: Position,
}
impl CodeChar {

    pub fn ch(&self) -> char { self.m_ch }
    pub fn pos(&self) -> Position { self.m_pos }
    pub fn is_eof(&self) -> bool { self.m_ch == EOFCHAR }
    pub fn as_tuple(&self) -> (char, Position) { (self.m_ch, self.m_pos) }
}
impl fmt::Debug for CodeChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}, {:?})", self.m_pos, self.m_ch)
    }
}

pub fn new_code_char(ch: char, pos: Position) -> CodeChar {
    CodeChar{ m_ch: ch, m_pos: pos }
}