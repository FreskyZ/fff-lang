#![allow(dead_code)]
///! fff-lang
///!
///! codemap, from source files to source file string's  
///! iterator<(char, position)>,  
///! which is <0.1.0>'s filemap and lexical's layer 0

#[macro_use] mod span;
mod error;
mod code_char;
mod code_file;

use code_char::new_code_char;
use code_file::CodeFile;
use code_file::CodeFileIter;
pub use code_char::EOFCHAR;
pub use code_char::EOFSCHAR;
pub use code_char::CodeChar;
pub use error::CodeMapError;
pub use span::Position;
pub use span::StringPosition;

// Iterator to get chars, this is my multi source file core logic processor
pub struct CodeChars<'a> {
    files: Vec<CodeFileIter<'a>>,
    current_index: Option<usize>,   // none for EOFs
    last_eofpos: Position,          // for furthur returns
}
impl<'a> CodeChars<'a> {

    fn new(codemap: &'a mut CodeMap) -> CodeChars {
        
        let current_index = if codemap.files.len() > 0 { Some(0) } else { None };
        CodeChars{ 
            files: codemap.files.iter_mut().map(|mut file| file.iter()).collect(),
            current_index: current_index,
            last_eofpos: Position::new(),
        }
    }

    pub fn next(&mut self) -> CodeChar {

        match self.current_index {
            Some(index) => {
                let ret_val = self.files[index].next();
                if ret_val.is_eof() {
                    self.last_eofpos = ret_val.pos();
                    if index == self.files.len() - 1 {
                        self.current_index = None;
                    } else {
                        self.current_index = Some(index + 1);
                    }
                }
                return ret_val;
            }
            None => return new_code_char(EOFSCHAR, self.last_eofpos.clone()),
        }
    }
}

// CodeMap, source code file and content manager
// # Examples
// ```
/// let mut codemap = CodeMap::with_str("123\ndef");
/// codemap.input_str("456");
/// let mut iter = codemap.iter();
/// assert_eq!(iter.next().as_tuple(), ('1', make_pos!(0, 1, 1)));
/// assert_eq!(iter.next().as_tuple(), ('2', make_pos!(0, 1, 2)));
/// assert_eq!(iter.next().as_tuple(), ('3', make_pos!(0, 1, 3)));
/// assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(0, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), ('d', make_pos!(0, 2, 1)));
/// assert_eq!(iter.next().as_tuple(), ('e', make_pos!(0, 2, 2)));
/// assert_eq!(iter.next().as_tuple(), ('f', make_pos!(0, 2, 3)));;
/// assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(0, 2, 4)));
/// assert_eq!(iter.next().as_tuple(), ('4', make_pos!(1, 1, 1)));
/// assert_eq!(iter.next().as_tuple(), ('5', make_pos!(1, 1, 2)));
/// assert_eq!(iter.next().as_tuple(), ('6', make_pos!(1, 1, 3)));
/// assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(1, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(1, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(1, 1, 4)));
// ```
pub struct CodeMap {
    files: Vec<CodeFile>,
}
impl CodeMap {
    
    pub fn new() -> CodeMap {
        CodeMap{ files: Vec::new() }
    }

    pub fn with_files(file_names: Vec<String>) -> Result<CodeMap, CodeMapError> {
        let mut codemap = CodeMap::new();
        codemap.input_files(file_names)?;
        return Ok(codemap);
    }

    // test helper, may panic, but this will not
    pub fn with_test_str(program: &str) -> CodeMap {
        let mut codemap = CodeMap::new();
        codemap.input_str(program);
        return codemap;
    }
}
impl CodeMap {

    pub fn input_files(&mut self, file_names: Vec<String>) -> Result<(), CodeMapError> {

        let len = self.files.len();
        for (index, file_name) in file_names.into_iter().enumerate() {
            self.files.push(CodeFile::from_file((len + index) as u32, file_name)?);
        }
        Ok(())
    }
    pub fn input_str(&mut self, content: &str) {

        let len = self.files.len() as u32;
        self.files.push(CodeFile::from_str(len, content));
    }

    pub fn iter<'a>(&'a mut self) -> CodeChars<'a> {
        CodeChars::new(self)
    }

    // pub fn get_line(file_id: u32, line: u32) -> &str {

    // }
}

#[cfg(test)] #[test]
fn codemap_input() {
    use self::code_char::EOFCHAR;

    let file1 = "../tests/codemap/file1.ff".to_owned();
    let file2 = "../tests/codemap/file2.ff".to_owned();

    let mut codemap = CodeMap::new();
    let _ = codemap.input_files(vec![file1, file2]).expect("unexpectedly input file fail");
    let _ = codemap.input_str("some\nstr\n");

    // current feature is, simple merge and yield char
    let mut iter = codemap.iter();
    assert_eq!(iter.next().as_tuple(), ('a', make_pos!(0, 1, 1)));
    assert_eq!(iter.next().as_tuple(), ('b', make_pos!(0, 1, 2)));
    assert_eq!(iter.next().as_tuple(), ('c', make_pos!(0, 1, 3)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(0, 1, 4)));
    assert_eq!(iter.next().as_tuple(), ('d', make_pos!(0, 2, 1)));
    assert_eq!(iter.next().as_tuple(), ('e', make_pos!(0, 2, 2)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(0, 2, 3)));
    assert_eq!(iter.next().as_tuple(), ('f', make_pos!(0, 3, 1)));
    assert_eq!(iter.next().as_tuple(), ('g', make_pos!(0, 3, 2)));
    assert_eq!(iter.next().as_tuple(), ('h', make_pos!(0, 3, 3)));
    assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(0, 3, 4))); 
    assert_eq!(iter.next().as_tuple(), ('i', make_pos!(1, 1, 1)));
    assert_eq!(iter.next().as_tuple(), ('j', make_pos!(1, 1, 2)));
    assert_eq!(iter.next().as_tuple(), ('k', make_pos!(1, 1, 3)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(1, 1, 4)));
    assert_eq!(iter.next().as_tuple(), ('l', make_pos!(1, 2, 1)));
    assert_eq!(iter.next().as_tuple(), ('m', make_pos!(1, 2, 2)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(1, 2, 3)));
    assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(1, 3, 1)));
    assert_eq!(iter.next().as_tuple(), ('s', make_pos!(2, 1, 1)));
    assert_eq!(iter.next().as_tuple(), ('o', make_pos!(2, 1, 2)));
    assert_eq!(iter.next().as_tuple(), ('m', make_pos!(2, 1, 3)));
    assert_eq!(iter.next().as_tuple(), ('e', make_pos!(2, 1, 4)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(2, 1, 5)));
    assert_eq!(iter.next().as_tuple(), ('s', make_pos!(2, 2, 1)));
    assert_eq!(iter.next().as_tuple(), ('t', make_pos!(2, 2, 2)));
    assert_eq!(iter.next().as_tuple(), ('r', make_pos!(2, 2, 3)));
    assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(2, 2, 4)));
    assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(2, 3, 1)));
}
