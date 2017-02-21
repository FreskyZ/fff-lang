//! fff-lang 
//! lexical position types
//! 
//! Currently only provides 2 types, Position and StringPosition, 
//! and make_pos! and make_str_pos! macro for convenience in writing test

use std::fmt;

/// Position of a character
#[derive(Eq, PartialEq, PartialOrd, Ord, Clone, Copy)]
pub struct Position {
    m_file_id: u32,
    m_row: u32,
    m_col: u32,
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{}:{}", self.m_file_id, self.m_row, self.m_col)
    }
}
impl Position {

    pub fn new() -> Position {
        Position { m_file_id: 0, m_row: 1, m_col: 1 }
    }
    pub fn with_file_id(file_id: u32) -> Position {
        Position{ m_file_id: file_id, m_row: 1, m_col: 1 }
    }
    pub fn with_row_and_col(row: u32, col: u32) -> Position {
        Position{ m_file_id: 0, m_row: row, m_col: col }
    }
    pub fn with_all(file_id: u32, row: u32, col: u32) -> Position {
        Position{ m_file_id: file_id, m_row: row, m_col: col }
    }

    pub fn next_col(&self) -> Position {
        Position { m_file_id: self.m_file_id, m_row: self.m_row, m_col: self.m_col + 1 }
    }
    pub fn next_row(&self) -> Position {
        Position { m_file_id: self.m_file_id, m_row: self.m_row + 1, m_col: 1 }
    }

    pub fn row(&self) -> u32 {
        self.m_row
    }
    pub fn col(&self) -> u32 {
        self.m_col
    }
    pub fn file_id(&self) -> u32 {
        self.m_file_id
    }
}

#[macro_export]
macro_rules! make_pos {
    ($row: expr, $col: expr) => (Position::with_row_and_col($row, $col));
    ($id: expr, $row: expr, $col: expr) => (Position::with_all($id, $row, $col))
}

/// Position of a string
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct StringPosition {
    m_file_id: u32,
    m_start_row: u32, 
    m_start_col: u32,
    m_end_row: u32,
    m_end_col: u32,
}

impl fmt::Debug for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{}:{}-{}:{}", self.m_file_id, self.m_start_row, self.m_start_col, self.m_end_row, self.m_end_col)
    }
}
impl StringPosition { // Construct
    
    /// New empty
    pub fn new() -> StringPosition {
        StringPosition{ m_file_id: 0, m_start_row: 1, m_start_col: 1, m_end_row: 1, m_end_col: 1 }
    }
    /// New empty with file id
    pub fn with_file_id(file_id: u32) -> StringPosition {
        StringPosition{ m_file_id: file_id, m_start_row: 1, m_start_col: 1, m_end_row: 1, m_end_col: 1 }
    }

    /// Same as `StringPosition::from2(pos1.start_pos, pos2.end_pos)`
    pub fn merge(pos1: StringPosition, pos2: StringPosition) -> StringPosition {
        
        if pos1.m_file_id != pos2.m_file_id {
            panic!("Trying to merge position from different source file")
        } else {
            StringPosition{ 
                m_file_id: pos1.m_file_id, 
                m_start_row: pos1.m_start_row, 
                m_start_col: pos1.m_start_col, 
                m_end_row: pos2.m_end_row, 
                m_end_col: pos2.m_end_col,
            }
        }
    }
    /// New with 2 `Position`
    pub fn from2(start_pos: Position, end_pos: Position) -> StringPosition {
        if start_pos.m_file_id != end_pos.m_file_id {
            panic!("Trying to construct string position from different source file")
        } else {
            StringPosition{ 
                m_file_id: start_pos.m_file_id, 
                m_start_row: start_pos.m_row, 
                m_start_col: start_pos.m_col, 
                m_end_row: end_pos.m_row, 
                m_end_col: end_pos.m_col,
            }
        }
    }
    /// Make string position from single Position by duplicate it
    pub fn double(char_pos: Position) -> StringPosition {
        StringPosition{ 
            m_file_id: char_pos.m_file_id, 
            m_start_row: char_pos.m_row, 
            m_start_col: char_pos.m_col, 
            m_end_row: char_pos.m_row, 
            m_end_col: char_pos.m_col,
        }
    }
    /// New with 4 column or row numbers
    pub fn from4(start_row: u32, start_col: u32, end_row: u32, end_col: u32) -> StringPosition {
        StringPosition{ 
            m_file_id: 0,
            m_start_row: start_row,
            m_start_col: start_col, 
            m_end_row: end_row, 
            m_end_col: end_col,
        }
    }
    /// New with all field
    pub fn with_all(file_id: u32, start_row: u32, start_col: u32, end_row: u32, end_col: u32) -> StringPosition {
        StringPosition{ 
            m_file_id: file_id, 
            m_start_row: start_row,
            m_start_col: start_col, 
            m_end_row: end_row, 
            m_end_col: end_col,
        }
    }
}
impl StringPosition { // access

    pub fn file_id(&self) -> u32 {
        self.m_file_id
    }

    pub fn start_pos(&self) -> Position {
        Position::with_row_and_col(self.m_start_row, self.m_start_col)
    }
    pub fn end_pos(&self) -> Position {
        Position::with_row_and_col(self.m_end_row, self.m_end_col)
    }
}

#[macro_export]
macro_rules! make_str_pos {
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from4($row1, $col1, $row2, $col2));
    ($id: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::with_all($id, $row1, $col1, $row2, $col2))
}
