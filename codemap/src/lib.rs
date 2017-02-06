//! fff-lang
//!
//! codemap, from source files to source file string's  
//! iterator<(char, position)>,  
//! which is <0.1.0>'s filemap and lexical's layer 0

#[macro_use]
extern crate codepos;

#[cfg(test)]
use codepos::Position;

// pub struct CodeChar {
//     ch: char, 
//     pos: Position,
// }
// impl CodeChar {

//     pub fn ch() -> char {

//     }

//     pub fn pos() -> &Position {

//     }
// }

// pub struct CodeIterator {

// }
// impl Iterator for CodeIterator {
//     fn next() {
//           
//     }
// }

// pub type FileName = String;

// pub struct FileNameCollection {
//     items: Vec<FileName>,
// }

// pub struct CodeMap {
//     file_names: FileNameCollection, 
// }
// impl CodeMap {
    
//     pub fn new() -> CodeMap {

//     }

//     pub fn input_files(&mut self, filenames: Vec<&str>) -> Result<(), Error> {

//     }

//     pub fn iter() -> CodeIterator {

//     }
// }

#[cfg(test)]
#[test]
fn codemap_something() {

    // target
    // let file1 = "../tests/codemap/file1.ff".to_owned();
    // let file2 = "../tests/codemap/file2.ff".to_owned();
    // let mut codemap = CodeMap::new();

    // let _ = codemap.input_files(vec![&file1, &file2]).expect("Some err");
    // assert_eq!(codemap.file_names, vec![file1, file2]);

    // let iter = codemap.iter();
    // assert_eq!(iter.next().unwrap(), CodeChar::new('a', make_pos!(0, 1, 1)));
    // assert_eq!(iter.next().unwrap(), CodeChar::new('b', make_pos!(0, 1, 2)));
    // assert_eq!(iter.next().unwrap(), CodeChar::new('c', make_pos!(1, 1, 1)));
    // assert_eq!(iter.next(), None);
}
