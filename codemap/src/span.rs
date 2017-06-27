///! fff-lang 
///! 
///! codemap/span
///!
///! Currently only provides 2 types, Position and StringPosition, 
///! and make_charpos! and make_span! macro for convenience in test

use std::fmt;
use std::ops::Range;

const DEFAULT_FILE_ID: usize = ::std::usize::MAX;

/// Byte index of a char
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct CharPos {
    file_id: usize, // used to be u32, but even it is u32, this struct is still 16 byte size, then give it usize
    char_id: usize,
}
impl Default for CharPos {
    fn default() -> CharPos { CharPos{ file_id: DEFAULT_FILE_ID, char_id: 0 } }
}
impl fmt::Debug for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        match self.file_id {
            DEFAULT_FILE_ID => write!(f, "xx"),
            _ => write!(f, "<{}>{}", self.file_id, self.char_id),
        }
    }
}
impl CharPos {

    pub fn new(file_id: usize, char_id: usize) -> CharPos { CharPos{ file_id, char_id } }

    pub fn get_file_id(&self) -> usize { self.file_id }
    pub fn get_char_id(&self) -> usize { self.char_id }
    pub fn is_default(&self) -> bool { self.file_id == DEFAULT_FILE_ID }

    pub fn offset(&self, offset: isize) -> CharPos {
        CharPos{ 
            file_id: self.file_id, 
            char_id: if offset > 0 { self.char_id + offset as usize } else { self.char_id - (-offset) as usize } 
        }
    }
    pub fn as_span(&self) -> Span {
        Span{ file_id: self.file_id, start_id: self.char_id, end_id: self.char_id } 
    }
    pub fn merge(&self, rhs: &CharPos) -> Span {
        // ignore default
        if self.file_id == DEFAULT_FILE_ID { return rhs.as_span() }
        if rhs.file_id == DEFAULT_FILE_ID { return self.as_span() }
        // normal
        Span{ file_id: self.file_id, start_id: self.char_id, end_id: rhs.char_id }
    }
}

#[macro_export]
macro_rules! make_charpos {
    ($char_id: expr) => (CharPos::new(0, $char_id));
    ($file_id: expr, $char_id: expr) => (CharPos::new($file_id, $char_id));
}

/// Position of a string
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct Span {
    file_id: usize,
    start_id: usize,
    end_id: usize,
}
impl Default for Span {
    fn default() -> Span { Span{ file_id: DEFAULT_FILE_ID, start_id: 0, end_id: 0 } }
}
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        match self.file_id {
            DEFAULT_FILE_ID => write!(f, "xx"),
            _ => write!(f, "<{}>{}-{}", self.file_id, self.start_id, self.end_id),
        }
    }
}
impl Span {
    
    pub fn new(file_id: usize, start_id: usize, end_id: usize) -> Span { Span{ file_id, start_id, end_id } }

    pub fn get_file_id(&self) -> usize { self.file_id }
    pub fn get_start_id(&self) -> usize { self.start_id }
    pub fn get_end_id(&self) -> usize { self.end_id }
    pub fn get_start_pos(&self) -> CharPos { CharPos::new(self.file_id, self.start_id) }
    pub fn get_end_pos(&self) -> CharPos { CharPos::new(self.file_id, self.end_id) }

    pub fn is_default(&self) -> bool { self.file_id == DEFAULT_FILE_ID }

    pub fn merge(&self, rhs: &Span) -> Span {
        // ignore default
        if self.file_id == DEFAULT_FILE_ID { return *rhs }
        if rhs.file_id == DEFAULT_FILE_ID { return *self }
        // normal
        Span{ file_id: self.file_id, start_id: self.start_id, end_id: rhs.end_id }
    }

    pub fn slice(&self, range: Range<usize>) -> Span {
        use std::cmp::min;

        Span{ file_id: self.file_id,
            start_id: min(self.end_id, self.start_id + range.start),
            end_id: min(self.end_id, self.start_id + range.end),
        }
    }
}

#[macro_export]
macro_rules! make_span {
    ($file_id: expr, $start_id: expr, $end_id: expr) => (Span::new($file_id, $start_id, $end_id));
    ($start_id: expr, $end_id: expr) => (Span::new(0, $start_id, $end_id))
}

#[cfg(test)] #[test]
fn span_index() {

    assert_eq!(make_span!(1, 5).slice(2..3), make_span!(3, 4));
    assert_eq!(make_span!(3, 4).slice(0..5), make_span!(3, 4));
    assert_eq!(make_span!(10, 15).slice(100..200), make_span!(15, 15));
}

// // Technically remain
// #[allow(dead_code)]
// fn utf8_char_next(bytes: &[u8]) -> char {
//     use std::mem::transmute;

//     unsafe { transmute(
//         if bytes[0] <= 0b_1000_0000 {
//             bytes[0] as u32
//         } else if bytes[0] > 0b11110000u8 {
//             (((bytes[0] as u32) & 0b00000111u32) << 18)
//                 + (((bytes[1] as u32) & 0b00111111u32) << 12)
//                 + (((bytes[2] as u32) & 0b00111111u32) << 6)
//                 + ((bytes[3] as u32) & 0b00111111u32)
//         } else if bytes[0] > 0b11100000u8 {
//             (((bytes[0] as u32) & 0b00001111u32) << 12)
//                 + (((bytes[1] as u32) & 0b00111111u32) << 6)
//                 + (((bytes[2] as u32) & 0b00111111u32))
//         } else if bytes[0] > 0b11000000u8 {
//             (((bytes[0] as u32) & 0b00011111u32) << 6)
//                 + ((bytes[1] as u32) & 0b00111111u32)
//         } else {
//             bytes[0] as u32
//         }
//     )}
// }
// #[cfg(test)] #[test]
// fn utf8_char_next_test() {
    
//     let s = "12你34我5，6";
//     for (pos, ch) in s.char_indices() {
//         assert_eq!(ch, utf8_char_next(&s.as_bytes()[pos..]));
//     }
// }