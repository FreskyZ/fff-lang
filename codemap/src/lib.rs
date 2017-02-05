//! fff-lang
//!
//! codemap, from source files to source file string's  
//! iterator<(char, position)>,  
//! which is <0.1.0>'s filemap and lexical's layer 0

#[macro_use]
mod position;

pub use position::Position;
pub use position::StringPosition;

// pub struct CodeChar {
//     ch: char, 
//     pos: Position,
//     file: Rc<FileName>,
// }
// impl CodeChar {

//     pub fn ch() -> char {

//     }

//     pub fn pos() -> Position {

//     }
// }

// pub struct CodeIterator {

// }
// impl Iterator for CodeIterator {

// }

// pub struct CodeMap {

// }
// impl CodeMap {
    
//     pub fn new() -> CodeMap {

//     }

//     pub fn input_files(filenames: Vec<&str>) -> Result<(), Error> {

//     }

//     pub fn iter() -> CodeIterator {

//     }
// }

#[cfg(test)]
#[test]
fn it_works() {

}
