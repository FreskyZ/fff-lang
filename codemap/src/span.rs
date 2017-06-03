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
}

#[macro_export]
macro_rules! make_charpos {
    ($char_id: expr) => (CharPos::new(0, $char_id));
    ($file_id: expr, $char_id: expr) => (CharPos::new($file_id, $char_id));
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
    pub fn from2(start_pos: CharPos, end_pos: CharPos) -> StringPosition {
        if start_pos.file_id != end_pos.file_id {
            panic!("Trying to construct string position from different source file")
        } else {
            StringPosition{ 
                m_file_id: start_pos.file_id as u32, 
                m_start_row: 1, 
                m_start_col: start_pos.char_id as u32, 
                m_end_row: 1, 
                m_end_col: end_pos.char_id as u32,
            }
        }
    }
    /// Make string position from single Position by duplicate it
    pub fn double(char_pos: CharPos) -> StringPosition {
        StringPosition{ 
            m_file_id: char_pos.file_id as u32, 
            m_start_row: 1, 
            m_start_col: char_pos.char_id as u32, 
            m_end_row: 1, 
            m_end_col: char_pos.char_id as u32,
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

    pub fn start_pos(&self) -> CharPos {
        CharPos::new(self.m_file_id as usize, self.m_start_col as usize)
    }
    pub fn end_pos(&self) -> CharPos {
        CharPos::new(self.m_file_id as usize, self.m_end_col as usize)
    }
}

#[macro_export]
//#[deprecated]
macro_rules! make_str_pos {
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from4($row1, $col1, $row2, $col2));
    ($id: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::with_all($id, $row1, $col1, $row2, $col2))
}
#[macro_export]
//#[deprecated]
macro_rules! make_strpos { 
    ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::from4($row1, $col1, $row2, $col2));
    ($id: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (StringPosition::with_all($id, $row1, $col1, $row2, $col2))
}
// #[macro_export]
// macro_rules! make_span {

// }

#[cfg(test)] #[test]
fn byte_pos_feas() {
    use std::str::from_utf8;

    let s = "12你34我5，6";
    for (pos, ch) in s.char_indices() {
        println!("{}, {}, {}", pos, ch, from_utf8(&s.as_bytes()[pos..]).unwrap().chars().next().unwrap());
    }

    // let test_string = "你我他".to_owned().repeat(65536);

    assert!(false);
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

// #[cfg(test)] extern crate test;
// #[cfg(test)] use self::test::Bencher;

// #[cfg(test)] #[bench]
// fn string_index_by_from_utf8(b: &mut Bencher) {
//     use std::str::from_utf8;

//     let test_string = "你我他".to_owned().repeat(65536);
//     b.iter(|| assert_eq!(from_utf8(&test_string.as_bytes()[10_0002..]).unwrap().chars().next().unwrap(), '我'));
// }
// #[cfg(test)] #[bench]
// fn string_index_by_chars(b: &mut Bencher) {

//     let test_string = "你我他".to_owned().repeat(65536);
//     b.iter(|| assert_eq!(test_string.chars().nth(3_3334).unwrap(), '我'));
// }
// #[cfg(test)] #[bench]
// fn string_index_by_myself(b: &mut Bencher) {

//     let test_string: String = "你我他".to_owned().repeat(65536);
//     b.iter(|| assert_eq!(next_char_unchecked3(&test_string.as_bytes()[10_0002..]), '我'));
// }

#[allow(dead_code)]
fn next_char_unchecked1(bytes: &[u8]) -> char {
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
#[allow(dead_code)]
fn next_char_unchecked3(bytes: &[u8]) -> char {
    use std::mem::transmute;

    unsafe { transmute(
        if bytes[0] > 0b11110000u8 {
            (((bytes[0] & 0b00000111u8) as u32) << 18)
                + (((bytes[1] & 0b00111111u8) as u32) << 12)
                + (((bytes[2] & 0b00111111u8) as u32) << 6)
                + ((bytes[3] & 0b00111111u8) as u32)
        } else if bytes[0] > 0b11100000u8 {
            (((bytes[0] & 0b00001111u8) as u32) << 12)
                + (((bytes[1] & 0b00111111u8) as u32) << 6)
                + ((bytes[2] & 0b00111111u8) as u32)
        } else if bytes[0] > 0b11000000u8 {
            (((bytes[0] & 0b00011111u8) as u32) << 6)
                + ((bytes[1] & 0b00111111u8) as u32)
        } else {
            bytes[0] as u32
        }
    )}
}
// #[cfg(test)] #[bench]
// fn string_index_by_myself_more1(b: &mut Bencher) {

//     let test_string: String = "你我他".to_owned().repeat(65536);
//     let n = test::black_box(1000);
//     b.iter(|| (0..n).fold(0, |_, _| { assert_eq!(next_char_unchecked1(&test_string.as_bytes()[10_0002..]), '我'); 0 }));
// }
// #[cfg(test)] #[bench]
// fn string_index_by_myself_more2(b: &mut Bencher) {

//     let test_string: String = "你我他".to_owned().repeat(65536);
//     let n = test::black_box(1000);
//     b.iter(|| (0..n).fold(0, |_, _| { assert_eq!(next_char_unchecked2(&test_string.as_bytes()[10_0002..]), '我'); 0 }));
// }
// #[cfg(test)] #[bench]
// fn string_index_by_myself_more3(b: &mut Bencher) {

//     let test_string: String = "你我他".to_owned().repeat(65536);
//     let n = test::black_box(1000);
//     b.iter(|| (0..n).fold(0, |_, _| { assert_eq!(next_char_unchecked3(&test_string.as_bytes()[10_0002..]), '我'); 0 }));
// }