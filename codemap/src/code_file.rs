///! fff-lang
///!
///! codemap/codefile
///! filename and file content owner, yield char and pos from file content

use std::str::CharIndices;

use super::CharPos;
use super::CodeMapError;
pub const EOFCHAR: char = 0u8 as char;
pub const EOFSCHAR: char = 255u8 as char;

pub struct CodeFileIter<'a> {
    file_id: usize,
    eof_index: usize,
    chars: CharIndices<'a>,
}
impl<'a> CodeFileIter<'a> {

    fn new(codefile: &'a CodeFile) -> CodeFileIter<'a> {
        CodeFileIter{
            file_id: codefile.id,
            eof_index: codefile.content.as_bytes().len(),
            chars: codefile.content.char_indices(),
        }
    }

    pub fn next(&mut self) -> (char, CharPos) {
        loop {
            match self.chars.next() {
                Some((_, '\r')) => continue,
                Some((index, ch)) => return (ch, CharPos::new(self.file_id, index)),
                None => return (EOFCHAR, CharPos::new(self.file_id, self.eof_index)),
            }
        }
    }
}

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct CodeFile {
    id: usize,
    name: String,
    content: String,
    lf_positions: Vec<usize>,
}
impl CodeFile {

    pub fn from_str(id: usize, content: &str) -> CodeFile {
        CodeFile{ id, name: format!("<dummy-{}>", id),
            lf_positions: content.char_indices().filter(|indice| indice.1 == '\n').map(|indice| indice.0).collect(),
            content: content.to_owned(),
        }
    }
    pub fn from_file(id: usize, file_name: String) -> Result<CodeFile, CodeMapError> {
        use std::fs::File;
        use std::io::Read;

        let mut content = String::new();
        let mut file = File::open(&file_name).map_err(|e| CodeMapError::CannotOpenFile(file_name.clone(), e))?;
        let _ = file.read_to_string(&mut content).map_err(|e| CodeMapError::CannotReadFile(file_name.clone(), e))?;

        Ok(CodeFile{ id, name: file_name, 
            lf_positions: content.char_indices().filter(|indice| indice.1 == '\n').map(|indice| indice.0).collect(),
            content, 
        })
    }

    pub fn iter<'a>(&'a self) -> CodeFileIter<'a> {
        CodeFileIter::new(self)
    }
}
impl CodeFile {
    
    fn utf8_char_len_at_index(&self, charid: usize) -> usize {
        match self.content.as_bytes()[charid] {
            0b_0000_0000...0b_1000_0000 => 1,
            0b_1100_0000...0b_1110_0000 => 2,
            0b_1110_0000...0b_1111_0000 => 3,
            0b_1111_0000...0b_1111_1111 => 4,
            _ => 0,
        }
    }

    /// (row, column)
    pub fn get_position_by_charpos(&self, charpos: CharPos) -> (usize, usize) {
        if charpos.get_file_id() != self.id { panic!("incorrect file id when querying char position") }
        
        let char_id = charpos.get_char_id();
        let lf_ids = &self.lf_positions;
        let lf_ids_len = lf_ids.len();
        println!("char id: {}", char_id);
        
        // not prev LF id because first line has no prev LF
        let (row_num, row_start_id) = if lf_ids_len == 0 {
            (1, 0)
        } else if char_id <= lf_ids[0] {
            (1, 0)
        } else {
            let mut retval = (1, 0);
            for (row_num, lf_id) in lf_ids.into_iter().enumerate() {
                if *lf_id < char_id {
                    retval = (row_num + 2, lf_id + 1); // after lf_ids[0] is line 2; LF is always 1 byte width
                }
            }
            retval
        };

        let bytes = self.content.as_bytes();
        let mut column_num = 1;
        let mut current_id = row_start_id;
        loop {
            if current_id == char_id {
                return (row_num, column_num);
            }
            if current_id >= bytes.len() {
                return (row_num, column_num);
            }
            let current_char_width = self.utf8_char_len_at_index(current_id);
            if current_char_width == 1 && bytes[current_id] == '\n' as u8 {
                panic!("invalid char pos when querying char position")
            }
            if !(current_char_width == 1 && bytes[current_id] == '\r' as u8) { // still ignore \r
                column_num += 1;
            }
            current_id += current_char_width;
        }
    }
}

#[cfg(test)] #[test]
fn code_file_get_pos() {

    macro_rules! test_case {
        ($input: expr, $([$char_pos: expr => $row: expr, $col: expr, case $caseid: expr], )+) => (
            let code_file = CodeFile::from_str(0, $input);
            $(
                assert_eq!{ code_file.get_position_by_charpos(CharPos::new(0, $char_pos)), ($row, $col), "#{}", $caseid }
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
fn code_file_iter() {

    macro_rules! test_case {
        ($input: expr, $($ch: expr, $char_id: expr,)*) => (
            let codefile = CodeFile::from_str(0, $input);
            let mut iter = codefile.iter();
            let mut ret_chars = Vec::new();
            loop {
                match iter.next() {
                    (EOFCHAR, _) => break,
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

#[cfg(test)] #[test]
fn code_file_from_file() {

    assert_eq!{ CodeFile::from_file(0, "../tests/codemap/file1.ff".to_owned()).unwrap().content, "abc\r\nde\r\nfgh" }
    assert_eq!{ CodeFile::from_file(42, "../tests/codemap/file2.ff".to_owned()).unwrap().content, "ijk\r\nlm\r\n" }
    use std::fs::File;
    assert_eq!{
        CodeFile::from_file(0, "not_exist.ff".to_owned()), 
        Err(CodeMapError::CannotOpenFile("not_exist.ff".to_owned(), File::open("not_exist.ff").unwrap_err()))
    }
}