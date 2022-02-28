///! source::iter: source content iterator, with forwarded interner

use std::ops::{Add, AddAssign};
use super::{SourceFiles, Symbols, SymId};

pub const EOF: char = 0u8 as char;

/// Character location
///
/// - it is byte index accross all source files, e.g. second file's position starts from first file's byte length (+1)
///   to reduce memory usage because location info is used extremely widely
/// - it is u32 not usize because it is not reasonable to
///   have a file size over 4GB or all source file total size over 4GB for this toy language (possibly for all languages)
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct Position(pub(super) u32);

impl Position {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
        self.0
    }
    pub fn offset(self, offset: i32) -> Self {
        Self(if offset >= 0 { self.0 + offset as u32 } else { self.0 - (-offset) as u32 })
    }
}
impl From<u32> for Position {
    fn from(v: u32) -> Self {
        Self(v)
    }
}

// use `position1 + position2` to merge into span
impl Add for Position {
    type Output = Span;
    fn add(self, rhs: Position) -> Span {
        debug_assert!(rhs.0 >= self.0, "invalid position + position");
        Span::new(self, rhs)
    }
}

/// Character range location
///
/// construct from 2 Positions,
/// while type name is Span, recommend variable name is `loc` or `location`
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    // e.g. Span::new(position1, position2) or Span::new(42, 43)
    pub fn new(start: impl Into<Position>, end: impl Into<Position>) -> Span {
        Span{ start: start.into(), end: end.into() }
    }
}

// use `span1 + span2` to merge span
// ATTENTION: only allow left + right, while gap/overlap both acceptable
impl Add for Span {
    type Output = Span;
    fn add(self, rhs: Span) -> Span {
        debug_assert!(rhs.start.0 >= self.start.0 && rhs.end.0 >= self.end.0, "invalid span + span");
        Span{ start: self.start, end: rhs.end }
    }
}

// or `span + position`
impl Add<Position> for Span {
    type Output = Span;
    fn add(self, rhs: Position) -> Span {
        debug_assert!(rhs.0 >= self.end.0, "invalid span + position");
        Span{ start: self.start, end: rhs }
    }
}

// use `span += position` to update span
impl AddAssign<Position> for Span {
    fn add_assign(&mut self, rhs: Position) {
        debug_assert!(rhs.0 >= self.end.0, "invalid span += position");
        self.end = rhs;
    }
}

// or use `span1 += span2`
// ATTENTION: only allow left += right, while gap/overlap both acceptable
impl AddAssign<Span> for Span {
    fn add_assign(&mut self, rhs: Span) {
        debug_assert!(rhs.start.0 >= self.start.0 && rhs.end.0 >= self.end.0, "invalid span += span");
        self.end = rhs.end;
    }
}

// use `position.into()` to turn position directly into span
impl From<Position> for Span {
    fn from(position: Position) -> Span {
        Span::new(position, position)
    }
}

// this iterator is the exact first layer of processing above source code content, 
// (except test) logically all span comes from position created by the next function
//
// this iterator also forwards the symbol interning interface, because lexical parser need iteration and 
// intern at the same time, but you cannot call mutable borrow methods if source context is immutable borrowed by this
pub struct Chars<'a> {
    pub(super) slice: &'a str, // advancing string slice of SourceFile.content
    pub(super) index: usize,   // index of next character, initial value should be SourceFile.start_index
    pub(super) files: &'a SourceFiles,
    pub(super) symbols: &'a mut Symbols,
}

impl<'a> Chars<'a> {
    /// iterate return char and byte index
    /// 
    /// ignore all bare or not bare CR, return EOF after EOF, fuse
    pub fn next(&mut self) -> (char, Position) {
        loop {
            if self.slice.len() == 0 {
                return (EOF, Position::new(self.index as u32));
            } else if self.slice.as_bytes()[0] == b'\r' {
                self.slice = &self.slice[1..];
                self.index += 1;
                continue;
            } else {
                let bytes = self.slice.as_bytes();
                let (char_length, r#char) = match get_char_width(self.slice, 0) {
                    1 => (1, bytes[0] as u32),
                    2 => (2, (((bytes[0] as u32) & 0b00011111u32) << 6) + ((bytes[1] as u32) & 0b00111111u32)),
                    3 => (3, (((bytes[0] as u32) & 0b00001111u32) << 12) + (((bytes[1] as u32) & 0b00111111u32) << 6) + (((bytes[2] as u32) & 0b00111111u32))),
                    4 => (4, (((bytes[0] as u32) & 0b00000111u32) << 18) + (((bytes[1] as u32) & 0b00111111u32) << 12) + (((bytes[2] as u32) & 0b00111111u32) << 6) + ((bytes[3] as u32) & 0b00111111u32)),
                    _ => panic!("invalid utf-8 sequence"),
                };
                self.slice = &self.slice[char_length..];
                self.index += char_length;
                // SAFETY: invalid char should not cause severe issue in lexical parse and syntax parse
                return (unsafe { char::from_u32_unchecked(r#char) }, Position::new((self.index - char_length) as u32));
            }
        }
    }

    pub fn intern_span(&mut self, location: Span) -> SymId {
        self.symbols.intern_span(location, self.files.map_span_to_content(location))
    }
    pub fn intern_str(&mut self, value: &str) -> SymId {
        self.symbols.intern_str(value)
    }
    pub fn intern_string(&mut self, value: String) -> SymId {
        self.symbols.intern_string(value)
    }
}

// width byte[0]  byte[1]  byte[2]  byte[3]
// 1     0xxxxxxx
// 2     110xxxxx 10xxxxxx
// 3     1110xxxx 10xxxxxx 10xxxxxx
// 4     11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
const WIDTH: [usize; 256] = [
//  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 3
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 4
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 5
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 6
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 7
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 9
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // A
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // B
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // D
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // E
    4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, // F
];

pub fn get_char_width(content: &str, byte_index: usize) -> usize {
    WIDTH[content.as_bytes()[byte_index] as usize]
}
