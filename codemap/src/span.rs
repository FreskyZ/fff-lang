///! fff-lang 
///! 
///! codemap/span
///! CharPosition and Span and make_span!

use std::fmt;
use std::ops::Range;

use super::SourceCode;

const DEFAULT_FILE_ID: usize = ::std::usize::MAX;

/// Byte index of a char
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct CharPos {
    pub(super) file_id: usize, // used to be u32, but even it is u32, this struct is still 16 byte size, then give it usize
    pub(super) char_id: usize,
}
impl Default for CharPos {
    fn default() -> CharPos { CharPos{ file_id: DEFAULT_FILE_ID, char_id: 0 } }
}
impl CharPos {
    pub fn format(&self, source: Option<&SourceCode>) -> String {
        match (source, self.file_id) {
            (_, DEFAULT_FILE_ID) => format!("<no-charpos>"),
            (None, _) => format!("<<{}>{}>", self.file_id, self.char_id),
            (Some(source), _) => {
                let (row, column) = source.map_index(*self);
                format!("<{}:{}>", row, column)
            }
        }
    }
}
impl fmt::Debug for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(None)) }
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
    pub(super) file_id: usize,
    pub(super) start_id: usize,
    pub(super) end_id: usize,
}
impl Default for Span {
    fn default() -> Span { Span{ file_id: DEFAULT_FILE_ID, start_id: 0, end_id: 0 } }
}
impl Span {
    pub fn format(&self, source: Option<&SourceCode>) -> String {
        match (source, self.file_id) {
            (_, DEFAULT_FILE_ID) => format!("<no-span>"),
            (None, _) => format!("<<{}>{}-{}>", self.file_id, self.start_id, self.end_id),
            (Some(source), _) => {
                let (start_row, start_column) = source.map_index(self.get_start_pos());
                let (end_row, end_column) = source.map_index(self.get_end_pos());
                format!("<{}:{}-{}:{}>", start_row, start_column, end_row, end_column)
            }
        }
    }
}
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(None)) }
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
        // `make_span!(0, 42).merge(&Span::default())` or Span::default().merge(make_span!(0, 42))` should be `Span::default()`
        if self.file_id == DEFAULT_FILE_ID || rhs.file_id == DEFAULT_FILE_ID { 
            Span::default() 
        } else {
            Span{ file_id: self.file_id, start_id: self.start_id, end_id: rhs.end_id }
        }
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
fn span_slice() {

    assert_eq!(make_span!(1, 5).slice(2..3), make_span!(3, 4));
    assert_eq!(make_span!(3, 4).slice(0..5), make_span!(3, 4));
    assert_eq!(make_span!(10, 15).slice(100..200), make_span!(15, 15));
}

#[cfg(test)] #[test]
fn charpos_format() {
    
    macro_rules! test_case {
        ($input: expr, $([$char_pos: expr => $format_result: expr, case $caseid: expr], )+) => (
            let source = SourceCode::with_test_str(0, $input);
            $(
                assert_eq!{ make_charpos!($char_pos).format(Some(&source)), format!("<{}>", $format_result), "#{}", $caseid }
            )+
        )
    }

    test_case!{ "0123\n56\r8\n01234567\n9", 
        [0 => "1:1", case 0], 
        [2 => "1:3", case 1], 
        [14 => "3:5", case 2], 
        [30 => "4:2", case 3], // still, overflow(EOF) return next char of last char
    }
    test_case!{ "012345678901234567",
        [0 => "1:1", case 4],
        [15 => "1:16", case 5],
        [100 => "1:19", case 6],
    }
    test_case!{ "",
        [0 => "1:1", case 7], // both 'EOF is next char of last char' and 'first position is (1, 1)' requires this to be (1, 1)
        [1 => "1:1", case 8],
        [2 => "1:1", case 9],
        [100 => "1:1", case 10],
    }
    test_case!{ "var 你好 =\n 世界;",
        //     src, row, col, byte
        //       v,   1,   1,    0
        //       a,   1,   2,    1
        //       r,   1,   3,    2
        //     ' ',   1,   4,    3,
        //      你,   1,   5,    4, 5, 6
        //      好,   1,   6,    7, 8, 9,
        //     ' ',   1,   7,    10,
        //       =,   1,   8,    11,
        //      \n,   1,   9,    12,
        //     ' ',   2,   1,    13
        //      世,   2,   2,    14, 15, 16,
        //      界,   2,   3,    17, 18, 19,
        //       ;,   2,   4,    20
        [0 => "1:1", case 11],
        [1 => "1:2", case 12],
        [4 => "1:5", case 13],
        [7 => "1:6", case 14],
        [14 => "2:2", case 15],
        [17 => "2:3", case 16],
        [20 => "2:4", case 17],
        [21 => "2:5", case 18],
        [22 => "2:5", case 19],
        [1024 => "2:5", case 20],
    }

    test_case!{ "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        [2 => "1:1", case 21],
        [3 => "1:2", case 22],
        [4 => "1:3", case 23],
        [11 => "2:4", case 24],
        [26 => "4:2", case 25],
        [30 => "4:6", case 26],
        [10000 => "4:7", case 27],
    }

    test_case!{ "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        [0 => "1:1", case 28],
        [6 => "2:3", case 29],
        [9 => "2:4", case 30],
        [10 => "3:1", case 31],
        [11 => "4:1", case 32],
        [29 => "6:7", case 33],
    }

    assert_eq!{ CharPos::default().format(None), "<no-charpos>" }
    assert_eq!{ CharPos::default().format(Some(&SourceCode::with_test_str(42, "helloworld"))), "<no-charpos>" }
}
#[cfg(test)] #[test]
fn span_format() {

    macro_rules! test_case {
        ($input: expr, $([$start_id: expr, $end_id: expr => $format_result: expr, case $caseid: expr], )+) => (
            let source = SourceCode::with_test_str(0, $input);
            $(
                assert_eq!{ make_span!($start_id, $end_id).format(Some(&source)), format!("<{}>", $format_result), "#{}", $caseid }
            )+
        )
    }

    assert_eq!{ Span::default().format(None), "<no-span>" }
    assert_eq!{ Span::default().format(Some(&SourceCode::with_test_str(40, "helloworld"))), "<no-span>" }

    test_case!{ "0123\n56\r8\n01234567\n9", 
        [0, 2 => "1:1-1:3", case 0],
        [2, 30 => "1:3-4:2", case 1],
        [3, 14 => "1:4-3:5", case 3],
    }
    test_case!{ "012345678901234567",
        [0, 100 => "1:1-1:19", case 4],
        [13, 15 => "1:14-1:16", case 5],
        [0, 0 => "1:1-1:1", case 6],
        [100, 100 => "1:19-1:19", case 7],
    }
    test_case!{ "",
        [0, 0 => "1:1-1:1", case 8],
        [0, 1 => "1:1-1:1", case 9],
        [0, 42 => "1:1-1:1", case 10],
    }
    test_case!{ "var 你好 =\n 世界;",
        //     src, row, col, byte
        //       v,   1,   1,    0
        //       a,   1,   2,    1
        //       r,   1,   3,    2
        //     ' ',   1,   4,    3,
        //      你,   1,   5,    4, 5, 6
        //      好,   1,   6,    7, 8, 9,
        //     ' ',   1,   7,    10,
        //       =,   1,   8,    11,
        //      \n,   1,   9,    12,
        //     ' ',   2,   1,    13
        //      世,   2,   2,    14, 15, 16,
        //      界,   2,   3,    17, 18, 19,
        //       ;,   2,   4,    20
        [0, 1 => "1:1-1:2", case 11],
        [0, 14 => "1:1-2:2", case 12],
        [4, 21 => "1:5-2:5", case 13],
        [7, 17 => "1:6-2:3", case 14],
        [1024, 1024 => "2:5-2:5", case 15],
    }
}