#![cfg_attr(not(test), deny(warnings))]
///! fff-lang
///!
///! codemap, source code manager
///! 
///! read input file, stores source code string, provide iterator through chars and their locations, string interner

use std::path::PathBuf;
use std::rc::Rc;

#[macro_use] mod span;
#[macro_use] mod symbol_def;
mod error;
mod src_code;

pub use span::CharPos;
pub use span::Span;
pub use error::CodeMapError;
pub use src_code::EOF_CHAR;
pub use src_code::SourceCode;
pub use src_code::SourceCodeIter;
pub use symbol_def::SymbolID;
pub use symbol_def::SymbolCollection;

pub struct SourceMap {
    items: Vec<Rc<SourceCode>>, // because some syntax tree node want it
}
impl SourceMap {

    pub fn new<T>(main_file: T) -> Result<SourceMap, CodeMapError> where T: Into<PathBuf> + Clone {
        Ok(SourceMap {
            items: vec![Rc::new(SourceCode::with_file_name(0, main_file)?)],
        })
    }
    pub fn add_file<T>(&mut self, path: T) -> Result<Rc<SourceCode>, CodeMapError> where T: Into<PathBuf> + Clone {
        let new_id = self.items.len();
        self.items.push(Rc::new(SourceCode::with_file_name(new_id, path)?));
        Ok(self.items[new_id].clone())
    }

    // this is not ops::Index because I want to return rc'd source code
    pub fn index(&self, id: usize) -> Rc<SourceCode> {
        self.items[id].clone()
    }

    /// for following macro use
    pub fn new_items(items: Vec<Rc<SourceCode>>) -> SourceMap { SourceMap{ items } }
}

/// for test
#[macro_export]
macro_rules! make_sources {
    ($($x:expr),*) => ({
        let mut retval = Vec::new();
        {
            let _retval = &mut retval; // `&mut` for statisfy 'unused mut', `_` for statisfy unused var
            $(
                _retval.push($x);
            )*
        }
        SourceMap::new_items(retval)
    });
    ($($x:expr,)*) => (make_sources![$($x),*])
}
