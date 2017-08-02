///! fff-lang
///!
///! syntax/root
///! root = { item }

use std::fmt;
use std::rc::Rc;
use std::path::Path;
use std::path::PathBuf;
use std::path::MAIN_SEPARATOR;

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

    // search module in dir
    // 4 versions, for module 'xxx_yyy_zzz', in priority
    // './xxx-yyy-zzz.ff', './xxx_yyy_zzz.ff', './xxx-yyy-zzz/module.ff', './xxx_yyy_zzz.module.ff'
    fn _search_module(dir: &Path, module_name: &str) -> Option<PathBuf> {
        let hypened_name: String = module_name.chars().map(|ch| if ch == '_' { '-' } else { ch }).collect();
        let maybe_names: Vec<PathBuf> = if hypened_name != module_name {
            vec![ // because Path API is hard to use in a simple expression, so complete format! it and convert back
                format!("{}{}{}.ff", dir.display(), MAIN_SEPARATOR, hypened_name).into(),
                format!("{}{}{}.ff", dir.display(), MAIN_SEPARATOR, module_name).into(),
                format!("{}{}{}{}module.ff", dir.display(), MAIN_SEPARATOR, hypened_name, MAIN_SEPARATOR).into(),
                format!("{}{}{}{}module.ff", dir.display(), MAIN_SEPARATOR, module_name, MAIN_SEPARATOR).into(),
            ]
        } else {
            vec![
                format!("{}{}{}.ff", dir.display(), MAIN_SEPARATOR, module_name).into(),
                format!("{}{}{}{}module.ff", dir.display(), MAIN_SEPARATOR, module_name, MAIN_SEPARATOR).into(),
            ]
        };
        return maybe_names.into_iter().find(|maybe_name| ::std::fs::metadata(&maybe_name).is_ok());
    }

    /// require ref mut source code because will add more source code if imported
    pub fn new(sources: &mut SourceMap, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> Result<SyntaxTree, ()> {
        let main_module = SyntaxTree::new_module(sources.index(0), messages, symbols)?;
        // let mut processed_modules = Vec::new();
        let previous_modules = vec![main_module];
        // let mut next_modules = Vec::new();
        loop {
            // TODO: if previous moodules is empty, break
            for import in previous_modules.iter().flat_map(Module::import_statements) {
                // TODO: get relative path from source code, try 3 versions: hyphen, underscore, /module.ff
                // if find any, construct source code by source map, construct token stream, construct module
                // move into next modules
                println!("{:?}", import);
            }
            // TODO: move previous modules into processed modules, move next modules into previous modules
            // remove this break
            break;
        }
        // TODO: construct with processed modules
        Ok(SyntaxTree{ modules: previous_modules })
    }
}

#[cfg(test)] #[test]
fn syntax_tree_search_module() {

    // assert_eq!(SyntaxTree::search_module(&::std::env::current_dir().unwrap(), "some_name"), Some("".into()));
    // assert_eq!(SyntaxTree::search_module(&::std::env::current_dir().unwrap(), "somename"), None);
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
    // panic!("no reason");
}
