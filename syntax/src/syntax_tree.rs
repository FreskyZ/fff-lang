///! fff-lang
///!
///! syntax/root
///! root = { item }

use std::fmt;
use std::rc::Rc;

use codemap::SourceMap;
use codemap::SourceCode;
use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::TokenStream;

use super::Module;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;
use super::Formatter;
use super::ISyntaxFormat;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub modules: Vec<Module>, 
}
impl ISyntaxFormat for SyntaxTree {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("syntax-tree");
        if self.modules.len() == 0 {
            f = f.endl().indent1().lit("no-item");
        }
        for module in &self.modules {
            f = f.endl().apply1(module);
        }
        f.finish()
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl SyntaxTree {

    fn new_module(source: Rc<SourceCode>, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> ParseResult<Module> {
        let tokens = TokenStream::new(source.as_ref(), messages, symbols);
        let mut sess = ParseSession::new(source, &tokens, messages, symbols);
        return Module::parse(&mut sess);
    }

    /// require ref mut source code because will add more source code if imported
    pub fn new(sources: &mut SourceMap, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> Result<SyntaxTree, ()> {
        let main_module = SyntaxTree::new_module(sources.index(0), messages, symbols)?;
        // let mut processed_modules = Vec::new();
        let previous_modules = vec![main_module];
        // let mut next_modules = Vec::new();
        loop {
            for import in previous_modules.iter().flat_map(Module::import_statements) {
                println!("{:?}", import);
            }
            break;
        }
        Ok(SyntaxTree{ modules: previous_modules })
    }
}

#[cfg(test)] #[test]
fn syntax_tree_recursive() {

    let mut sources = SourceMap::new("../tests/syntax/mod/main.ff").expect("open main file failed");
    let mut messages = MessageCollection::new();
    let mut symbols = SymbolCollection::new();

    match SyntaxTree::new(&mut sources, &mut messages, &mut symbols) {
        Ok(tree) => println!("{:?}", tree),
        Err(()) => println!("unexpected failed: {:?}", messages),
    }
    //panic!("no reason");
}
