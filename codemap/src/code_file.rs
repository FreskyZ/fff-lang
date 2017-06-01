///! fff-lang
///!
///! codemap/codefile
///! filename and file content owner, yield char and pos from file content

use std::str::Chars;

use codepos::Position;

use super::code_char::EOFCHAR;
use super::code_char::CodeChar;
use super::code_char::new_code_char;
use super::CodeMapError;

pub struct CodeFileIter<'a> {
    text_pos: Position,     // id integrated here
    prev_is_new_line: bool,
    completed: bool,
    current_line: String,

    chars: Chars<'a>,
    lines: &'a mut Vec<String>,
}
impl<'a> CodeFileIter<'a> {

    fn new(codefile: &'a mut CodeFile) -> CodeFileIter<'a> {
        CodeFileIter{
            text_pos: make_pos!(codefile.id, 1, 0),
            prev_is_new_line: false,
            completed: false,
            current_line: String::new(),

            chars: codefile.content.chars(),
            lines: &mut codefile.lines
        }
    }

    // push current line to lines and make current line empty
    fn push_current_line(&mut self) {
        use std::mem;

        // starnge method for `lines.push(current_line); current_line = new();`
        let mut temp_line = String::new();
        mem::swap(&mut self.current_line, &mut temp_line);
        self.lines.push(temp_line);
    }

    // old lexer/v0
    pub fn next(&mut self) -> CodeChar {
        
        loop {
            match self.chars.next() {
                Some('\r') => continue,
                Some(ch) => {
                    if self.prev_is_new_line {
                        self.text_pos = self.text_pos.next_row();
                    } else {
                        self.text_pos = self.text_pos.next_col();
                    }

                    self.prev_is_new_line = ch == '\n';
                    if self.prev_is_new_line {
                        self.push_current_line();
                    } else {
                        self.current_line.push(ch);
                    }
                    return new_code_char(ch, self.text_pos);
                }
                None => {
                    if !self.completed {
                        self.push_current_line();
                        if self.prev_is_new_line {
                            self.text_pos = self.text_pos.next_row();
                        } else {
                            self.text_pos = self.text_pos.next_col();
                        }
                        self.completed = true;
                    }
                    return new_code_char(EOFCHAR, self.text_pos);
                }
            }
        }
    }
}

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct CodeFile {
    id: u32,
    name: String,
    content: String,
    lines: Vec<String>, 
}
impl CodeFile {

    pub fn from_str(id: u32, content: &str) -> CodeFile {
        CodeFile {
            id: id,
            name: format!("<dummy-{}>", id),
            content: content.to_owned(),
            lines: Vec::new(),
        }
    }
    pub fn from_file(id: u32, file_name: String) -> Result<CodeFile, CodeMapError> {
        use std::fs::File;
        use std::io::Read;

        let mut content = String::new();
        let mut file = File::open(&file_name).map_err(|e| CodeMapError::CannotOpenFile(file_name.clone(), e))?;
        let _ = file.read_to_string(&mut content).map_err(|e| CodeMapError::CannotReadFile(file_name.clone(), e))?;

        Ok(CodeFile{ 
            id: id, 
            name: file_name, 
            content: content, 
            lines: Vec::new() 
        })
    }

    pub fn iter<'a>(&'a mut self) -> CodeFileIter<'a> {
        CodeFileIter::new(self)
    }
}


#[cfg(test)] #[test]
fn code_file_v0() {

    macro_rules! test_case {
        (
            $id: expr, $input: expr,                        // input
            [$($ch: expr, $row: expr, $col: expr, )*]       // iter chars
            $eof_pos: expr,
            [$($line: expr, )*]                             // lines
        ) => (
            let mut codefile = CodeFile::from_str($id, $input);
            {
                let mut iter = codefile.iter();
                let mut ret_chars = Vec::new();
                loop {
                    match iter.next().as_tuple() {
                        (EOFCHAR, eof_pos) => {
                            assert_eq!(eof_pos, $eof_pos);
                            break;
                        }
                        v0 => ret_chars.push(v0),
                    }
                }

                let expect_chars = &mut Vec::new();
                $(
                    expect_chars.push(($ch, make_pos!($id, $row, $col)));
                )*
                assert_eq!(&ret_chars, expect_chars);
            }

            let expect_lines = &mut Vec::<String>::new();
            $(
                expect_lines.push($line.to_owned());
            )*
            assert_eq!(&codefile.lines, expect_lines);
        )
    }

    test_case!(
        2, "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r", [
            'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
            'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
            'a', 3, 1, 's', 3, 2, 'd', 3, 3, 'w', 3, 4, 'e', 3, 5, 'q', 3, 6, '1', 3, 7, 'd', 3, 8, 'a', 3, 9, '\n', 3, 10,
            'a', 4, 1, 'w', 4, 2, 's', 4, 3, 'e', 4, 4, 'd', 4, 5, 'q', 4, 6,
        ] make_pos!(2, 4, 7), [
            "abc", "def", "asdweq1da", "awsedq",
        ]
    );

    test_case!(
        42, "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n", [
            'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
            'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
            '\n', 3, 1,
            'a', 4, 1, 's', 4, 2, 'd', 4, 3, '\n', 4, 4,
            'w', 5, 1, 'e', 5, 2, 'q', 5, 3, '1', 5, 4, 'd', 5, 5, 'a', 5, 6, '\n', 5, 7,
            'a', 6, 1, 'w', 6, 2, 's', 6, 3, 'e', 6, 4, 'd', 6, 5, 'q', 6, 6, '\n', 6, 7,
        ] make_pos!(42, 7, 1), [
            "abc", "def", "", "asd", "weq1da", "awsedq", "",
        ]
    );

    test_case!(
        233, "\nabc\ndef\n", [
            '\n', 1, 1,
            'a', 2, 1, 'b', 2, 2, 'c', 2, 3, '\n', 2, 4,
            'd', 3, 1, 'e', 3, 2, 'f', 3, 3, '\n', 3, 4,
        ] make_pos!(233, 4, 1), [
            "", "abc", "def", "",
        ]
    );

    test_case!(1, "", [] make_pos!(1, 1, 1), ["", ]); // directly return EOF, an empty line
}

#[cfg(test)] #[test]
fn code_file_from_file() {

    macro_rules! test_case {
        (
            $id: expr, $input: expr,                        // input
            $content: expr,                                 // content
            [$($ch: expr, $row: expr, $col: expr, )*]       // iter chars
            [$($line: expr, )*]                             // lines
        ) => (
            let mut codefile = CodeFile::from_file($id, $input.to_owned()).expect("open or read failed");
            assert_eq!(codefile.content, $content);

            {
                let mut iter = codefile.iter();
                let mut ret_chars = Vec::new();
                loop {
                    match iter.next().as_tuple() {
                        (EOFCHAR, _) => break,
                        v0 => ret_chars.push(v0),
                    }
                }

                let expect_chars = &mut Vec::new();
                $(
                    expect_chars.push(($ch, make_pos!($id, $row, $col)));
                )*
                assert_eq!(&ret_chars, expect_chars);
            }

            let expect_lines = &mut Vec::<String>::new();
            $(
                expect_lines.push($line.to_owned());
            )*
            assert_eq!(&codefile.lines, expect_lines);
        )
    }

    test_case!{
        2, "../tests/codemap/file1.ff", 
        "abc\r\nde\r\nfgh", [
            'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
            'd', 2, 1, 'e', 2, 2, '\n', 2, 3,
            'f', 3, 1, 'g', 3, 2, 'h', 3, 3, 
        ] [
            "abc", "de", "fgh", 
        ]
    }

    test_case!{
        42, "../tests/codemap/file2.ff",
        "ijk\r\nlm\r\n", [
            'i', 1, 1, 'j', 1, 2, 'k', 1, 3, '\n', 1, 4,
            'l', 2, 1, 'm', 2, 2, '\n', 2, 3,
        ] [
            "ijk", "lm", "",
        ]
    }

    use std::fs::File;
    assert_eq!{
        CodeFile::from_file(0, "not_exist.ff".to_owned()), 
        Err(CodeMapError::CannotOpenFile("not_exist.ff".to_owned(), File::open("not_exist.ff").unwrap_err()))
    }
}