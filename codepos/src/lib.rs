//! fff-lang 
//! lexical position types
//! 
//! Currently only provides 2 types, Position and StringPosition, 
//! and make_pos! and make_str_pos! macro for convenience in writing test

use std::fmt;

/// Position of a character
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct Position {
    m_row: u32,
    m_col: u32,
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.m_row, self.m_col)
    }
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.m_row, self.m_col)
    }
}
impl Position {

    pub fn new() -> Position {
        Position { m_row: 1, m_col: 1 }
    }

    pub fn next_col(&self) -> Position {
        Position { m_row: self.m_row, m_col: self.m_col + 1 }
    }
    pub fn next_row(&self) -> Position {
        Position { m_row: self.m_row + 1, m_col: 1 }
    } 

    pub fn from2(row: u32, col: u32) -> Position {
        Position{ m_row: row, m_col: col }
    }

    pub fn row(&self) -> u32 {
        self.m_row
    }
    pub fn col(&self) -> u32 {
        self.m_col
    }
}

#[macro_export]
macro_rules! make_pos {
    ($row: expr, $col: expr) => (Position::from2($row, $col))
}

use std::rc::Rc;
/// Position of a string
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct StringPosition {
    m_start_pos: Position,
    m_end_pos: Position,
}

impl fmt::Debug for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.m_start_pos, self.m_end_pos)
    }
}
impl fmt::Display for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.m_start_pos, self.m_end_pos)
    }
}
impl StringPosition { // Construct
    
    pub fn new() -> StringPosition {
        StringPosition { m_start_pos: Position::new(), m_end_pos: Position::new() }
    }

    /// Same as `StringPosition::from2(pos1.start_pos, pos2.end_pos)`
    pub fn merge(pos1: StringPosition, pos2: StringPosition) -> StringPosition {
        StringPosition{ m_start_pos: pos1.m_start_pos, m_end_pos: pos2.m_end_pos }
    }
    pub fn from2(start_pos: Position, end_pos: Position) -> StringPosition {
        StringPosition{ m_start_pos: start_pos, m_end_pos: end_pos }
    }
    pub fn from4(start_row: u32, start_col: u32, end_row: u32, end_col: u32) -> StringPosition {
        StringPosition{ m_start_pos: Position::from2(start_row, start_col), m_end_pos: Position::from2(end_row, end_col) }
    }
}
impl StringPosition { // access

    pub fn start_pos(&self) -> Position {
        self.m_start_pos
    }
    pub fn end_pos(&self) -> Position {
        self.m_end_pos
    }
}

#[macro_export]
macro_rules! make_str_pos {
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from4($row1, $col1, $row2, $col2))
}

#[cfg(test)]
#[test]
fn availability() {
    use std::rc::Rc;
    use std::mem::size_of;
    use std::io::stderr;
    use std::io::Write;

    struct S1 {
        a: u32, 
        b: u32
    }
    struct S2 {
        a: u32, 
        b: u32,
        fileid: u16,
    }
    struct S3 {
        a: u32, 
        b: u32,
        file: Rc<String>,
    }

    stderr().write_all(format!("{}\n", size_of::<[S1; 10]>()).as_bytes());
    stderr().write_all(format!("{}\n", size_of::<[S2; 10]>()).as_bytes());
    stderr().write_all(format!("{}\n", size_of::<[S3; 10]>()).as_bytes());
}