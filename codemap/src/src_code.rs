///! fff-lang
///!
///! codemap/codefile
///! filename and file content owner, yield char and pos from file content

use std::fmt;
use std::path::Path;
use std::path::PathBuf;

use super::Span;
use super::CharPos;
use super::CodeMapError;

pub const EOF_CHAR: char = 0u8 as char;

// UTF8 string iterator, 
// yes, an unchecked utf8 string iterator is like this short

/// Get UTF8 char byte length at index of the bytes
///
/// no check bytes is valid UTF8 squence, no check index is valid index, 
/// also don't know if the panic will happen or how will happen
#[inline]
fn char_len_at_index(bytes: &[u8], index: usize) -> usize {
    match bytes[index] {
        0b_0000_0000...0b_1000_0000 => 1,
        0b_1100_0000...0b_1110_0000 => 2,
        0b_1110_0000...0b_1111_0000 => 3,
        0b_1111_0000...0b_1111_1111 => 4,
        _ => panic!("impossible byte value, I think"),
    }
}

/// Get UTF8 char at index of the bytes
///
/// no check bytes is valid UTF8 sequence, no check index is valid index, 
/// no check bytes length is enough if char length is more than 1, 
/// use interpret cast, no check calculated code point value is valid code point
#[inline]
fn char_at_index(bytes: &[u8], index: usize) -> (usize, char) { // char and its utf8 length at index
    let transmute = |v: u32| -> char { unsafe { ::std::mem::transmute(v) } };
    match char_len_at_index(bytes, index) {
        1 => (1, transmute(bytes[index] as u32)),
        2 => (2, transmute((((bytes[index] as u32) & 0b00011111u32) << 6) + ((bytes[index + 1] as u32) & 0b00111111u32))),
        3 => (3, transmute((((bytes[index] as u32) & 0b00001111u32) << 12) + (((bytes[index + 1] as u32) & 0b00111111u32) << 6) 
                    + (((bytes[index + 2] as u32) & 0b00111111u32)))),
        4 => (4, transmute((((bytes[index] as u32) & 0b00000111u32) << 18) + (((bytes[index + 1] as u32) & 0b00111111u32) << 12)
                    + (((bytes[index + 2] as u32) & 0b00111111u32) << 6) + ((bytes[index + 3] as u32) & 0b00111111u32))),
        _ => panic!("impossible branch"),
    }
}

pub struct SourceCodeIter<'a> {
    src: &'a SourceCode,
    index: usize,
}
impl<'a> SourceCodeIter<'a> {

    fn new(src: &'a SourceCode) -> SourceCodeIter<'a> { SourceCodeIter{ src, index: 0 } }

    /// Iterate through source code string
    ///
    /// ignores all bare or not bare CR, 
    /// return EOF_CHAR after EOF, fuse that
    pub fn next(&mut self) -> (char, CharPos) {
        loop {
            if self.index == self.src.src.len() {
                return (EOF_CHAR, CharPos::new(self.src.id, self.index));
            } else if self.src.src[self.index] == b'\r' {
                self.index += 1;
                continue;
            } else {
                let (len, ch) = char_at_index(&self.src.src, self.index);
                let retval = (ch, CharPos::new(self.src.id, self.index));
                self.index += len;
                return retval;
            }
        }
    }
}

/// Represents a source code file
#[derive(Eq, PartialEq)]
pub struct SourceCode {
    id: usize,
    name: PathBuf,            // absolute path
    src: Box<[u8]>,           // source code string, in form of owned byte slice, because no need of string methods, utf8 iterator also implemented here not depend on std
    endl_indexes: Vec<usize>, // line end byte indexes
}
impl fmt::Debug for SourceCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "source-code#{} {}", self.id, self.name.display())
    }
}
impl SourceCode {

    pub fn with_file_name<T>(id: usize, file_name: T) -> Result<SourceCode, CodeMapError> where T: Into<PathBuf> + Clone { // into for adapt String, Clone for construct error
        use std::io::Read;

        let file_name = file_name.into();
        let file_name = file_name.clone().canonicalize().map_err(|e| CodeMapError::CannotOpenFile(file_name.clone(), e))?;
        let mut src = String::new();
        let mut file = ::std::fs::File::open(&file_name).map_err(|e| CodeMapError::CannotOpenFile(file_name.clone(), e))?;
        let _ = file.read_to_string(&mut src).map_err(|e| CodeMapError::CannotReadFile(file_name.clone(), e))?;

        Ok(SourceCode{ 
            id, 
            name: file_name, 
            endl_indexes: src.char_indices().filter(|indice| indice.1 == '\n').map(|indice| indice.0).collect(),
            src: src.into_bytes().into_boxed_slice(),
        })
    }
    pub fn with_test_str(id: usize, src: &str) -> SourceCode {
        SourceCode{ 
            id, 
            name: format!("<anon#{}>", id).into(),
            endl_indexes: src.char_indices().filter(|indice| indice.1 == '\n').map(|indice| indice.0).collect(),
            src: src.to_owned().into_bytes().into_boxed_slice(),
        }
    }

    pub fn iter<'a>(&'a self) -> SourceCodeIter<'a> { SourceCodeIter::new(self) }
}
impl SourceCode {
    
    pub fn get_absolute_path(&self) -> &Path { &self.name }
    pub fn get_relative_path(&self) -> &Path { &self.name } // FIXME: relative path
    pub fn get_directory(&self) -> &Path { self.name.parent().unwrap() } // as self.name must be valid file, then unwrap must success

    /// map byte index to (row, column)
    pub fn map_index(&self, charpos: CharPos) -> (usize, usize) {
        assert_eq!{ self.id, charpos.file_id, "incorrect file id" }
        
        let char_id = charpos.char_id;
        
        // get row number and row start character's byte index
        let (row_num, row_start_id) = if self.endl_indexes.len() == 0 {
            (1, 0)
        } else if char_id <= self.endl_indexes[0] {
            (1, 0)
        } else {
            // rev iterate through lf_offsets to find input byte index's range
            // `+2` for index = lf_ids[*0*] + 1 is line *2*
            // `+1` for LF must be 1 byte width, lf_ids[xxx] + 1 is exactly first char of next line
            // logically must find, so directly unwrap
            self.endl_indexes.iter().enumerate().rev()
                .find(|&(_, &endl_index)| char_id > endl_index).map(|(id, endl_index)| (id + 2, endl_index + 1)).unwrap()
        };

        // then iterate through this line's chars to find column number
        let mut column_num = 1;
        let mut current_id = row_start_id;
        loop {
            // exact match
            if current_id == char_id { return (row_num, column_num); }         
            // with assume that all char id is valid, this will not happen, but leave it here to make tests happy
            if current_id >= self.src.len() { return (row_num, column_num); } 
            // ignore \r, here you do not need `width == 1 and char == '\r'` because according to utf8, if width > 1 then char > ASCII::MAX also if char == '\r' then width == 1
            if self.src[current_id] != b'\r' { column_num += 1; }               
            current_id += char_len_at_index(&self.src, current_id);
        }
    }

    /// map span to string
    pub fn map_span(&self, span: &Span) -> &str {
        assert_eq!{ self.id, span.get_file_id(), "incorrect file id" }

        // previous 2 cases will not happen in real world, leave them here to satisfy tests
        match (span.start_id >= self.src.len(), span.end_id >= self.src.len()) {
            (true, _) => "",                                                                                // starting overflow
            (false, true) => unsafe { ::std::mem::transmute(&self.src[span.start_id .. self.src.len()]) },  // ending overflow
            (false, false) => unsafe { ::std::mem::transmute(&self.src[span.start_id .. (span.end_id + char_len_at_index(&self.src, span.end_id))]) },
        }
    }

    /// get line by row number, row_num start from 1
    pub fn map_line_num(&self, row_num: usize) -> &str {
        use std::mem::transmute;
        const EOF_STRING: &str = "";

        let lf_ids = &self.endl_indexes;
        if self.src.len() == 0 { return EOF_STRING; }                                       // empty file
        if row_num == 0 { return EOF_STRING; }                                              // downflow
        if row_num > lf_ids.len() + 1 { return EOF_STRING; }                                // normal overflow
        if row_num == 1 && lf_ids.len() == 0 { return unsafe{ transmute::<&[u8], &str>(self.src.as_ref()) } }      // "xxx".get_row(1)
        if row_num == 1 && lf_ids.len() > 0 && lf_ids[0] == 0 { return EOF_STRING; }        // "\nxxx".get_row(1)
        if row_num == lf_ids.len() + 1 && lf_ids[lf_ids.len() - 1] + 1 == self.src.len() { return EOF_STRING; } // last char is \n

        let start_id = if row_num == 1 { 0 } else { lf_ids[row_num - 2] + 1 /* next char of LF */ };
        let end_id = if row_num == lf_ids.len() + 1 { self.src.len() - 1 } else { lf_ids[row_num - 1] - 1 };
        return self.map_span(&Span::new(self.id, start_id, end_id));
    }
}

#[cfg(test)] #[test]
fn char_at_index_test() {
    
    let s = "12你34我5，6";
    for (pos, ch) in s.char_indices() {
        assert_eq!(ch, char_at_index(s.as_bytes(), pos).1);
    }
}

#[cfg(test)] #[test]
fn src_code_from_file_format_attr() {
    use std::fs::File;
    use std::fs::canonicalize;

    let file1 = "../tests/codemap/file1.ff";
    let file1_full = canonicalize(file1).expect("canon 1 failed");
    let source = SourceCode::with_file_name(42, file1).unwrap();
    assert_eq!{ 
        source, 
        SourceCode{
            id: 42,
            name: file1_full.clone(),
            src: "abc\r\nde\r\nfgh".to_owned().into_bytes().into_boxed_slice(),
            endl_indexes: vec![4, 8]
        } 
    }
    assert_eq!{
        format!("{:?}", source),
        format!("source-code#42 {}", file1_full.display())
    }
    assert_eq!{
        source.get_directory(),
        file1_full.parent().unwrap()
    }

    let file2 = "../tests/codemap/file2.ff";
    let file2_full = canonicalize(file2).expect("canon 2 failed");
    let source = SourceCode::with_file_name(43, file2).unwrap();
    assert_eq!{ 
        source, 
        SourceCode{
            id: 43,
            name: file2_full.clone(),
            src: "ijk\r\nlm\r\n".to_owned().into_bytes().into_boxed_slice(),
            endl_indexes: vec![4, 8]
        }
    }
    assert_eq!{
        format!("{:?}", source),
        format!("source-code#43 {}", file2_full.display())
    }
    assert_eq!{
        source.get_directory(),
        file2_full.parent().unwrap()
    }

    assert_eq!{
        SourceCode::with_file_name(0, "not_exist.ff".to_owned()),
        Err(CodeMapError::CannotOpenFile("not_exist.ff".into(), File::open("not_exist.ff").unwrap_err()))
    }
    
    assert_eq!{
        format!("{:?}", SourceCode::with_test_str(43, "helloworld")),
        "source-code#43 <anon#43>"
    }
}

// old v0 finally seperated into these tests
#[cfg(test)] #[test]
fn src_code_index_to_line_column() {

    macro_rules! test_case {
        ($input: expr, $([$char_pos: expr => $row: expr, $col: expr, case $caseid: expr], )+) => (
            let source = SourceCode::with_test_str(0, $input);
            $(
                assert_eq!{ source.map_index(make_charpos!($char_pos)), ($row, $col), "#{}", $caseid }
            )+
        )
    }

    test_case!{ "0123\n56\r8\n01234567\n9", 
        [0 => 1, 1, case 0], 
        [2 => 1, 3, case 1], 
        [14 => 3, 5, case 2], 
        [30 => 4, 2, case 3], // still, overflow(EOF) return next char of last char
    }
    test_case!{ "012345678901234567",
        [0 => 1, 1, case 4],
        [15 => 1, 16, case 5],
        [100 => 1, 19, case 6],
    }
    test_case!{ "",
        [0 => 1, 1, case 7], // both 'EOF is next char of last char' and 'first position is (1, 1)' requires this to be (1, 1)
        [1 => 1, 1, case 8],
        [2 => 1, 1, case 9],
        [100 => 1, 1, case 10],
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
        [0 => 1, 1, case 11],
        [1 => 1, 2, case 12],
        [4 => 1, 5, case 13],
        [7 => 1, 6, case 14],
        [14 => 2, 2, case 15],
        [17 => 2, 3, case 16],
        [20 => 2, 4, case 17],
        [21 => 2, 5, case 18],
        [22 => 2, 5, case 19],
        [1024 => 2, 5, case 20],
    }

    test_case!{ "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        [2 => 1, 1, case 21],
        [3 => 1, 2, case 22],
        [4 => 1, 3, case 23],
        [11 => 2, 4, case 24],
        [26 => 4, 2, case 25],
        [30 => 4, 6, case 26],
        [10000 => 4, 7, case 27],
    }

    test_case!{ "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        [0 => 1, 1, case 28],
        [6 => 2, 3, case 29],
        [9 => 2, 4, case 30],
        [10 => 3, 1, case 31],
        [11 => 4, 1, case 32],
        [29 => 6, 7, case 33],
    }
}
#[cfg(test)] #[test]
fn src_code_span_to_str() {
    
    macro_rules! test_case {
        ($input: expr, $([$start_id: expr, $end_id: expr => $expect: expr, case $caseid: expr], )+) => (
            let source = SourceCode::with_test_str(0, $input);
            $(
                assert_eq!{ source.map_span(&Span::new(0, $start_id, $end_id)), $expect, "#{}", $caseid }
            )+
        )
    }

    test_case!{ "01234567890",
        [0, 2 => "012", case 1],
        [3, 5 => "345", case 2],
        [8, 8 => "8", case 3],
        [0, 10 => "01234567890", case 4],
        [0, 100 => "01234567890", case 5],
        [100, 100 => "", case 6],
    }

    test_case!{ "var 你好 =\n 世界;；",
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
        //      ；,   2,   5,    21, 22, 23
        [0, 3 => "var ", case 7],
        [3, 4 => " 你", case 8],
        [4, 10 => "你好 ", case 9],
        [4, 14 => "你好 =\n 世", case 10],
        [14, 21 => "世界;；", case 11],
    }
}
#[cfg(test)] #[test]
fn src_code_get_row_str() {
    
    let source = SourceCode::with_test_str(0, "0123\n56\r8\n01234567\n9");
    assert_eq!(source.map_line_num(1), "0123");
    assert_eq!(source.map_line_num(2), "56\r8");
    assert_eq!(source.map_line_num(3), "01234567");
    assert_eq!(source.map_line_num(4), "9");
    assert_eq!(source.map_line_num(5), "");
    assert_eq!(source.map_line_num(0), "");

    let source = SourceCode::with_test_str(0, "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n");
    assert_eq!(source.map_line_num(1), "abc");
    assert_eq!(source.map_line_num(2), "def\r\r");
    assert_eq!(source.map_line_num(3), "");
    assert_eq!(source.map_line_num(4), "asd");
    assert_eq!(source.map_line_num(5), "we\rq1da");
    assert_eq!(source.map_line_num(6), "awsedq");
    assert_eq!(source.map_line_num(7), "");

    let source = SourceCode::with_test_str(0, "\nabc\ndef\n");
    assert_eq!(source.map_line_num(0), "");
    assert_eq!(source.map_line_num(1), "");
    assert_eq!(source.map_line_num(2), "abc");
    assert_eq!(source.map_line_num(3), "def");
    assert_eq!(source.map_line_num(4), "");
    
    let source = SourceCode::with_test_str(0, "abcdef");
    assert_eq!(source.map_line_num(1), "abcdef");
    assert_eq!(source.map_line_num(0), "");
    assert_eq!(source.map_line_num(2), "");

    let source = SourceCode::with_test_str(0, "");
    assert_eq!(source.map_line_num(0), "");
    assert_eq!(source.map_line_num(1), "");
}

#[cfg(test)] #[test]
fn src_code_iter() {

    macro_rules! test_case {
        ($input: expr, $($ch: expr, $char_id: expr,)*) => (
            let codefile = SourceCode::with_test_str(0, $input);
            let mut iter = codefile.iter();
            let mut ret_chars = Vec::new();
            loop {
                match iter.next() {
                    (EOF_CHAR, _) => break,
                    v0 => ret_chars.push(v0), // memory for v0
                }
            }

            let expect_chars = &mut Vec::new();
            $(
                expect_chars.push(($ch, make_charpos!(0, $char_id)));
            )*
            assert_eq!(&ret_chars, expect_chars);
        )
    }

    //           0             1              2          3
    //           0 1 2345 6789 0 1 234567 8 9 01234 5678901 2 3 
    test_case!{ "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        'a', 2, 'b', 3, 'c', 4, '\n', 5,
        'd', 6, 'e', 7, 'f', 8, '\n', 11,
        'a', 12, 's', 13, 'd', 14, 'w', 15, 'e', 16, 'q', 20, '1', 21, 'd', 22, 'a', 23, '\n', 24,
        'a', 25, 'w', 26, 's', 27, 'e', 28, 'd', 29, 'q', 30,
    } //         0             1            2 
    //           0123 4567 8 9 0 1234 567 89012 3456789
    test_case!{ "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        'a', 0, 'b', 1, 'c', 2, '\n', 3,
        'd', 4, 'e', 5, 'f', 6, '\n', 9,
        '\n', 10,
        'a', 11, 's', 12, 'd', 13, '\n', 14,
        'w', 15, 'e', 16, 'q', 18, '1', 19, 'd', 20, 'a', 21, '\n', 22,
        'a', 23, 'w', 24, 's', 25, 'e', 26, 'd', 27, 'q', 28, '\n', 29,
    }

    test_case!{ "\nabc\ndef\n", 
        '\n', 0,
        'a', 1, 'b', 2, 'c', 3, '\n', 4,
        'd', 5, 'e', 6, 'f', 7, '\n', 8,
    }

    test_case!{ "", }
}
