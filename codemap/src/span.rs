///! fff-lang 
///! 
///! codemap/span
///!
///! Currently only provides 2 types, Position and StringPosition, 
///! and make_charpos! and make_span! macro for convenience in test

use std::fmt;

/// Byte index of a char
#[derive(Eq, PartialEq, Clone, Copy, Default)]
pub struct CharPos {
    file_id: usize, // used to be u32, but even it is u32, this struct is still 16 byte size, then give it usize
    char_id: usize,
}
impl fmt::Debug for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "<{}>{}", self.file_id, self.char_id) }
}
impl CharPos {

    pub fn new(file_id: usize, char_id: usize) -> CharPos { CharPos{ file_id, char_id } }

    pub fn get_file_id(&self) -> usize { self.file_id }
    pub fn get_char_id(&self) -> usize { self.char_id }

    // deprecated
    pub fn double(&self) -> Span { 
        Span{ file_id: self.file_id, start_id: self.char_id, end_id: self.char_id } 
    }
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
        if self.file_id != rhs.file_id { panic!("trying to merge charpos from different source file") }
        Span{ file_id: self.file_id, start_id: self.char_id, end_id: rhs.char_id }
    }
}

#[macro_export]
macro_rules! make_charpos {
    ($char_id: expr) => (CharPos::new(0, $char_id));
    ($file_id: expr, $char_id: expr) => (CharPos::new($file_id, $char_id));
}

/// Position of a string
#[derive(Eq, PartialEq, Clone, Copy, Default)]
pub struct Span {
    file_id: usize,
    start_id: usize,
    end_id: usize,
}
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "<{}>{}-{}", self.file_id, self.start_id, self.end_id) }
}
impl Span {
    
    pub fn new(file_id: usize, start_id: usize, end_id: usize) -> Span { Span{ file_id, start_id, end_id } }

    pub fn get_file_id(&self) -> usize { self.file_id }
    pub fn get_start_id(&self) -> usize { self.start_id }
    pub fn get_end_id(&self) -> usize { self.end_id }
    pub fn get_start_pos(&self) -> CharPos { CharPos::new(self.file_id, self.start_id) }
    pub fn get_end_pos(&self) -> CharPos { CharPos::new(self.file_id, self.end_id) }

    pub fn merge(self, rhs: &Span) -> Span {
        if self.file_id != rhs.file_id { panic!("trying to merge span from different source file") }
        Span{ file_id: self.file_id, start_id: self.start_id, end_id: rhs.end_id }
    }
}
#[macro_export]
macro_rules! make_span {
    ($file_id: expr, $start_id: expr, $end_id: expr) => (Span::new($file_id, $start_id, $end_id));
    ($start_id: expr, $end_id: expr) => (Span::new(0, $start_id, $end_id))
}

// it is unchecked because the input parameters generated from char_indecis which already checked
#[allow(dead_code)]
fn next_char_unchecked(bytes: &[u8]) -> char {
    use std::char;

    char::from_u32(
        if bytes[0] & 0b11111000u8 == 0b11110000u8 {
            (((bytes[0] as u32) & 0b00000111u32) << 18)
                + (((bytes[1] as u32) & 0b00111111u32) << 12)
                + (((bytes[2] as u32) & 0b00111111u32) << 6)
                + ((bytes[3] as u32) & 0b00111111u32)
        } else if bytes[0] & 0b11110000u8 == 0b11100000u8 {
            (((bytes[0] as u32) & 0b00001111u32) << 12)
                + (((bytes[1] as u32) & 0b00111111u32) << 6)
                + (((bytes[2] as u32) & 0b00111111u32))
        } else if bytes[0] & 0b11100000u8 == 0b11000000u8 {
            (((bytes[0] as u32) & 0b00011111u32) << 6)
                + ((bytes[1] as u32) & 0b00111111u32)
        } else if bytes[0] & 0b10000000u8 == 0b00000000u8 {
            bytes[0] as u32
        } else {
            0u32
        }).unwrap()
}
#[cfg(test)] #[test]
fn utf8_iter_my() {
    
    let s = "12你34我5，6";
    for (pos, ch) in s.char_indices() {
        assert_eq!(ch, next_char_unchecked(&s.as_bytes()[pos..]));
    }
}

#[allow(dead_code)]
fn next_char_unchecked2(bytes: &[u8]) -> char {
    use std::mem::transmute;

    unsafe { transmute(
        if bytes[0] > 0b11110000u8 {
            (((bytes[0] as u32) & 0b00000111u32) << 18)
                + (((bytes[1] as u32) & 0b00111111u32) << 12)
                + (((bytes[2] as u32) & 0b00111111u32) << 6)
                + ((bytes[3] as u32) & 0b00111111u32)
        } else if bytes[0] > 0b11100000u8 {
            (((bytes[0] as u32) & 0b00001111u32) << 12)
                + (((bytes[1] as u32) & 0b00111111u32) << 6)
                + (((bytes[2] as u32) & 0b00111111u32))
        } else if bytes[0] > 0b11000000u8 {
            (((bytes[0] as u32) & 0b00011111u32) << 6)
                + ((bytes[1] as u32) & 0b00111111u32)
        } else {
            bytes[0] as u32
        }
    )}
}