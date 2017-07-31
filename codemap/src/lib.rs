#![deny(warnings)]
///! fff-lang
///!
///! codemap, source code manager
///! 
///! read input file, stores source code string, provide iterator through chars and their locations, string interner
///! 

use std::path::Path;
use std::path::PathBuf;

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
    items: Vec<SourceCode>,
}
impl SourceMap {

    pub fn new<T>(main_file: T) -> Result<SourceMap, CodeMapError> where T: Into<PathBuf> + Clone, for<'a> &'a T: AsRef<Path> {
        Ok(SourceMap {
            items: vec![SourceCode::with_file_name(0, main_file)?],
        })
    }

    pub fn index(&self, id: usize) -> &SourceCode {
        &self.items[id]
    }
}
