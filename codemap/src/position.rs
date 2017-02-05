//! fff-lang 
//! lexical position types
//! 
//! Currently only provides 2 types, Position and StringPosition, 
//! and a make_str_pos! macro for convenience in writing test

use std::fmt;

/// Position of a character
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct Position {
    pub row: u32,
    pub col: u32,
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}
impl Position {

    pub fn new() -> Position {
        Position { row: 1, col: 1 }
    }

    pub fn next_col(&self) -> Position {
        Position { row: self.row, col: self.col + 1 }
    }
    pub fn next_row(&self) -> Position {
        Position { row: self.row + 1, col: 1 }
    } 

    pub fn from2(row: u32, col: u32) -> Position {
        Position{ row: row, col: col }
    }
}

#[macro_export]
macro_rules! make_pos {
    ($row: expr, $col: expr) => (Position::from2($row, $col))
}

/// Position of a string
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct StringPosition {
    pub start_pos: Position,
    pub end_pos: Position,
}

impl fmt::Debug for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start_pos, self.end_pos)
    }
}
impl fmt::Display for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start_pos, self.end_pos)
    }
}
impl StringPosition {
    
    pub fn new() -> StringPosition {
        StringPosition { start_pos: Position::new(), end_pos: Position::new() }
    }

    pub fn from2(start_pos: Position, end_pos: Position) -> StringPosition {
        StringPosition{ start_pos: start_pos, end_pos: end_pos }
    }
    pub fn from4(start_row: u32, start_col: u32, end_row: u32, end_col: u32) -> StringPosition {
        StringPosition{ start_pos: Position{ row: start_row, col: start_col }, end_pos: Position{ row: end_row, col: end_col } }
    }
}

#[macro_export]
macro_rules! make_str_pos {
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from4($row1, $col1, $row2, $col2))
}
