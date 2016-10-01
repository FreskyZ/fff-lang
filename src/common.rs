
// Some common

/// Try convert from
pub trait TryFrom<T>
    where Self: Sized {
    fn try_from(t: T) -> Option<Self>;
}

// From with 2 param
pub trait From2<T1, T2> {
    fn from2(t1: T1, t2: T2) -> Self;
}

/// Text position
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

impl From<(usize, usize)> for Position {

    fn from(pos: (usize, usize)) -> Position {
        Position{ row: pos.0, col: pos.1 }
    } 
}
impl From2<usize, usize> for Position {

    fn from2(row: usize, col: usize) -> Position {
        Position{ row: row, col: col }
    }
}

/// Text position of a string
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

impl From<(usize, usize, usize, usize)> for StringPosition {

    fn from(pos: (usize, usize, usize, usize)) -> StringPosition {
        StringPosition{ start_pos: Position{ row: pos.0, col: pos.1 }, end_pos: Position{ row: pos.2, col: pos.3 } }
    }
}

impl From<(Position, Position)> for StringPosition {

    fn from(pos: (Position, Position)) -> StringPosition {
        StringPosition{ start_pos: pos.0, end_pos: pos.1 }
    }
}
impl From2<Position, Position> for StringPosition {

    fn from2(start: Position, end: Position) -> StringPosition {
        StringPosition{ start_pos: start, end_pos: end }
    }
}

impl fmt::Debug for StringPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start_pos, self.end_pos)
    }
}

impl_display_by_debug!(StringPosition);