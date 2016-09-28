
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct Position {
    pub row: usize,
    pub col: usize,
}

use std::fmt;
impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl_display_by_debug!(Position);

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
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct StringPosition {
    pub start_pos: Position,
    pub end_pos: Position,
}

impl StringPosition {
    
    pub fn new() -> StringPosition {
        StringPosition { start_pos: Position::new(), end_pos: Position::new() }
    }
}

impl fmt::Debug for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start_pos, self.end_pos)
    }
}

impl_display_by_debug!(StringPosition);