///! fff-lang
///!
///! codemap, source code manager
///! 
///! read input file, stores source code string, provide iterator through chars and their locations, string interner
///! 

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

    pub fn new() -> SourceMap { SourceMap{ items: Vec::new() } }
    pub fn with_test_str(src: &str) -> SourceMap { SourceMap{ items: vec![SourceCode::with_test_str(0, src)] } }

    pub fn add_file(&mut self, filename: String) -> Result<&SourceCode, CodeMapError> {
        let id = self.items.len();
        self.items.push(SourceCode::with_file_name(id, filename)?);
        Ok(&self.items[id])
    }

    pub fn index(&self, index: usize) -> &SourceCode { &self.items[index] }
    pub fn index_by_span(&self, index: Span) -> &SourceCode{ &self.items[index.file_id] }
}
