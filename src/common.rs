
// Some common

// From with 2 param
pub trait From2<T1, T2> {
    fn from2(t1: T1, t2: T2) -> Self;
}

/// Text position
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
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

/// Implement `fmt::Display` by previous implementation of `fmt::Debug`
macro_rules! impl_display_by_debug {
    ($t: ty) => (
        impl fmt::Display for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    )
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
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
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

#[cfg(test)]
macro_rules! test_condition_perrorln {
    ($cond: expr, $format: expr, $($args: expr, )+) => (if $cond { perrorln!($format, $($args, )+); });
    ($cond: expr, $format: expr) => (if $cond { perrorln!($format); });
}
#[cfg(not(test))]
macro_rules! test_condition_perrorln {
    ($cond: expr, $format: expr, $($args: expr, )+) => ();
    ($cond: expr, $format: expr) => ();
}

#[cfg(test)]
macro_rules! make_str_pos {
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from(($row1, $col1, $row2, $col2)))
}

pub fn format_vector_display<T: fmt::Display>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}
pub fn format_vector_debug<T: fmt::Debug>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{:?}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}

/// Macro for printing to the standard error.
/// 
/// Equivalent to `print!` macro except that print to stderr
///
/// Standard error unsually is not buffered and displayed immediately, and 
/// default rust test configuration shut down stdout and keeps stderror open 
///
/// # Panics
/// 
/// Panics if writing to io::stderr() fails.
///
/// # Examples
/// ```rust
/// # #[macro_use] extern crate fsz_common;
/// # fn main() {
/// perror!("Hello ");
/// perror!("{}", "World");
/// perror!("!");      // Get `Hello World!` at stderr immediately
/// # }
/// ```
#[cfg(test)]
macro_rules! perror {
    ($($arg:tt)*) => ({
        use std::io::Write;
        let _ = write!(&mut ::std::io::stderr(), $($arg)* );
    })
}

/// Macros for printing to the standard output, with a newline
///
/// Use the `format!` syntax to write data to the standard error, see 
/// `std::fmt` for more information
///
/// # Panics
/// 
/// Panics if writing to `io::stderr()` fails
///
/// # Examples
/// ```rust
/// # #[macro_use] extern crate fsz_common;
/// # fn main() {
/// perrorln!("Hello world!");
/// perrorln!("format {} arguments", "some");
/// # }
/// ```
macro_rules! perrorln {
    ($($arg:tt)*) => ({
        use std::io::Write;
        let _ = writeln!(&mut ::std::io::stderr(), $($arg)* );
    })
}

/// test_only_attr!([derive(1)] [derive(2) Encodable] struct abc{})
macro_rules! test_only_attr {
    (
        test: [$($attr_test: meta)*] 
        not_test: [$($attr_build: meta)*] 
        $typedef: item
    ) => (
        #[cfg(test)]
        $(#[$attr_test])*
        $typedef

        #[cfg(not(test))]
        $(#[$attr_build])*
        $typedef
    );

    (
        [$($attr_test: meta)*] 
        ![$($attr_build: meta)*] 
        $typedef: item
    ) => (
        #[cfg(test)]
        $(#[$attr_test])*
        $typedef

        #[cfg(not(test))]
        $(#[$attr_build])*
        $typedef
    )
}