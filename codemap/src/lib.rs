#![allow(dead_code)]
///! fff-lang
///!
///! codemap, from source files to source file string's  
///! iterator<(char, position)>,  
///! which is <0.1.0>'s filemap and lexical's layer 0

#[macro_use] mod span;
mod error;
mod code_file;

use code_file::CodeFile;
use code_file::CodeFileIter;
pub use code_file::EOFCHAR;
pub use code_file::EOFSCHAR;
pub use error::CodeMapError;
pub use span::CharPos;
pub use span::StringPosition;

// Iterator to get chars, this is my multi source file core logic processor
pub struct CodeChars<'a> {
    files: Vec<CodeFileIter<'a>>,
    current_index: Option<usize>,   // none for EOFs
    last_eofpos: CharPos,          // for furthur returns
}
impl<'a> CodeChars<'a> {

    fn new(codemap: &'a CodeMap) -> CodeChars {
        
        let current_index = if codemap.files.len() > 0 { Some(0) } else { None };
        CodeChars{ 
            files: codemap.files.iter().map(|file| file.iter()).collect(),
            current_index: current_index,
            last_eofpos: CharPos::default(),
        }
    }

    pub fn next(&mut self) -> (char, CharPos) {

        match self.current_index {
            Some(index) => {
                let (ret_ch, ret_pos) = self.files[index].next();
                if ret_ch == EOFCHAR {
                    self.last_eofpos = ret_pos;
                    if index == self.files.len() - 1 {
                        self.current_index = None;
                    } else {
                        self.current_index = Some(index + 1);
                    }
                }
                return (ret_ch, ret_pos);
            }
            None => return (EOFSCHAR, self.last_eofpos.clone()),
        }
    }
}

// CodeMap, source code file and content manager
// # Examples
// ```
/// let mut codemap = CodeMap::with_str("123\ndef");
/// codemap.input_str("456");
/// let mut iter = codemap.iter();
/// assert_eq!(iter.next(), ('1', make_charpos!(0, 1, 1)));
/// assert_eq!(iter.next(), ('2', make_charpos!(0, 1, 2)));
/// assert_eq!(iter.next(), ('3', make_charpos!(0, 1, 3)));
/// assert_eq!(iter.next(), ('\n', make_charpos!(0, 1, 4)));
/// assert_eq!(iter.next(), ('d', make_charpos!(0, 2, 1)));
/// assert_eq!(iter.next(), ('e', make_charpos!(0, 2, 2)));
/// assert_eq!(iter.next(), ('f', make_charpos!(0, 2, 3)));;
/// assert_eq!(iter.next(), (EOFCHAR, make_charpos!(0, 2, 4)));
/// assert_eq!(iter.next(), ('4', make_charpos!(1, 1, 1)));
/// assert_eq!(iter.next(), ('5', make_charpos!(1, 1, 2)));
/// assert_eq!(iter.next(), ('6', make_charpos!(1, 1, 3)));
/// assert_eq!(iter.next(), (EOFCHAR, make_charpos!(1, 1, 4)));
/// assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(1, 1, 4)));
/// assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(1, 1, 4)));
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
            self.files.push(CodeFile::from_file(len + index, file_name)?);
        }
        Ok(())
    }
    pub fn input_str(&mut self, content: &str) {

        let len = self.files.len();
        self.files.push(CodeFile::from_str(len, content));
    }

    pub fn iter<'a>(&'a self) -> CodeChars<'a> {
        CodeChars::new(self)
    }

    // pub fn get_line(file_id: u32, line: u32) -> &str {

    // }
}

#[cfg(test)] #[test]
fn codemap_input() {

    let file1 = "../tests/codemap/file1.ff".to_owned();
    let file2 = "../tests/codemap/file2.ff".to_owned();

    let mut codemap = CodeMap::new();
    let _ = codemap.input_files(vec![file1, file2]).expect("unexpectedly input file fail");
    let _ = codemap.input_str("some\nstr\n");

    // current feature is, simple merge and yield char
    let mut iter = codemap.iter();
    assert_eq!(iter.next(), ('a', make_charpos!(0, 1)));
    assert_eq!(iter.next(), ('b', make_charpos!(0, 2)));
    assert_eq!(iter.next(), ('c', make_charpos!(0, 3)));
    assert_eq!(iter.next(), ('\n', make_charpos!(0, 4)));
    assert_eq!(iter.next(), ('d', make_charpos!(0, 1)));
    assert_eq!(iter.next(), ('e', make_charpos!(0, 2)));
    assert_eq!(iter.next(), ('\n', make_charpos!(0, 3)));
    assert_eq!(iter.next(), ('f', make_charpos!(0, 1)));
    assert_eq!(iter.next(), ('g', make_charpos!(0, 2)));
    assert_eq!(iter.next(), ('h', make_charpos!(0, 3)));
    assert_eq!(iter.next(), (EOFCHAR, make_charpos!(0, 4))); 
    assert_eq!(iter.next(), ('i', make_charpos!(1, 1)));
    assert_eq!(iter.next(), ('j', make_charpos!(1, 2)));
    assert_eq!(iter.next(), ('k', make_charpos!(1, 3)));
    assert_eq!(iter.next(), ('\n', make_charpos!(1, 4)));
    assert_eq!(iter.next(), ('l', make_charpos!(1, 1)));
    assert_eq!(iter.next(), ('m', make_charpos!(1, 2)));
    assert_eq!(iter.next(), ('\n', make_charpos!(1, 3)));
    assert_eq!(iter.next(), (EOFCHAR, make_charpos!(1, 1)));
    assert_eq!(iter.next(), ('s', make_charpos!(2, 1)));
    assert_eq!(iter.next(), ('o', make_charpos!(2, 2)));
    assert_eq!(iter.next(), ('m', make_charpos!(2, 3)));
    assert_eq!(iter.next(), ('e', make_charpos!(2, 4)));
    assert_eq!(iter.next(), ('\n', make_charpos!(2, 5)));
    assert_eq!(iter.next(), ('s', make_charpos!(2, 1)));
    assert_eq!(iter.next(), ('t', make_charpos!(2, 2)));
    assert_eq!(iter.next(), ('r', make_charpos!(2, 3)));
    assert_eq!(iter.next(), ('\n', make_charpos!(2, 4)));
    assert_eq!(iter.next(), (EOFCHAR, make_charpos!(2, 1)));
    assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(2, 1)));
    assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(2, 1)));
    assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(2, 1)));
    assert_eq!(iter.next(), (EOFSCHAR, make_charpos!(2, 1)));
}
