///! fff-lang
///!
///! syntax/root
///! root = { item }

use std::fmt;
use std::rc::Rc;
use std::path::Path;
use std::path::PathBuf;
use std::path::MAIN_SEPARATOR;

use codemap::SymbolID;
use codemap::SourceMap;
use codemap::SourceCode;
use codemap::SymbolCollection;
use message::Message;
use message::MessageCollection;
use lexical::TokenStream;

use super::Module;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;
use super::Formatter;
use super::ISyntaxFormat;

#[allow(dead_code)] const SOURCE_FILE_EXT: &str = ".ff"; // don't known why rustc thinks SOURCE_FILE_EXT is never used

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ImportMap {
    pub file_id: usize,
    pub import_name: SymbolID,
    pub imported_file_id: usize,
}
impl ImportMap {
    pub fn new(file_id: usize, import_name: SymbolID, imported_file_id: usize) -> ImportMap { ImportMap{ file_id, import_name, imported_file_id } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub modules: Vec<Module>, 
    pub import_maps: Vec<ImportMap>, 
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

        f = f.endl().indent1().lit(if self.import_maps.is_empty() { "no-import" } else { "import-maps" });
        for map in &self.import_maps {
            f = f.endl().indent2().debug(&map.file_id).lit(", ").sym(map.import_name).lit(" => ").debug(&map.imported_file_id);
        }
        f.finish()
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl SyntaxTree {
    /// public constructor for test
    pub fn new_modules(modules: Vec<Module>, import_maps: Vec<ImportMap>) -> SyntaxTree { SyntaxTree{ modules, import_maps } }
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
    // if success, return path of the module, if failed, return possible path of the module
    fn search_module(dir: &Path, module_name: &str) -> Result<PathBuf, Vec<PathBuf>> {
        let hypened_name: String = module_name.chars().map(|ch| if ch == '_' { '-' } else { ch }).collect();
        let maybe_names: Vec<PathBuf> = if hypened_name != module_name {
            vec![ // because Path API is hard to use in a simple expression, so complete format! it and convert back
                format!("{}{}{}{}", dir.display(), MAIN_SEPARATOR, hypened_name, SOURCE_FILE_EXT).into(),
                format!("{}{}{}{}", dir.display(), MAIN_SEPARATOR, module_name, SOURCE_FILE_EXT).into(),
                format!("{}{}{}{}module{}", dir.display(), MAIN_SEPARATOR, hypened_name, MAIN_SEPARATOR, SOURCE_FILE_EXT).into(),
                format!("{}{}{}{}module{}", dir.display(), MAIN_SEPARATOR, module_name, MAIN_SEPARATOR, SOURCE_FILE_EXT).into(),
            ]
        } else {
            vec![
                format!("{}{}{}{}", dir.display(), MAIN_SEPARATOR, module_name, SOURCE_FILE_EXT).into(),
                format!("{}{}{}{}module{}", dir.display(), MAIN_SEPARATOR, module_name, MAIN_SEPARATOR, SOURCE_FILE_EXT).into(),
            ]
        };
        return maybe_names.iter().find(|maybe_name| ::std::fs::metadata(maybe_name).is_ok())
            .map(PathBuf::to_owned)
            .ok_or(maybe_names);
    }

    /// construct syntax tree, also multiple file support core, BFS search
    pub fn new(sources: &mut SourceMap, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> Result<SyntaxTree, ()> {

        let main_module = SyntaxTree::new_module(sources.index(0), messages, symbols)?;
        let mut import_maps = Vec::new();
        let mut processed_modules = Vec::new();
        let mut previous_modules = vec![main_module];
        let mut next_modules = Vec::new();
        loop { 
            if previous_modules.is_empty() { break; }
            'modules: for previous_module in &previous_modules {
                'imports: for import in previous_module.import_statements() {
                    if previous_module.source.get_file_id() != 0 
                        && previous_module.source.get_file_stem() != Some("module") {
                        // if not root module or sub root module, is not allowed to import module
                        messages.push(Message::with_help_by_str("cannot import module in this scope", 
                            vec![(import.all_span, "import statement here")], 
                            vec!["can only import modules in root module or sub root module"]
                        ));
                        continue 'modules;  // ignore this module all imports, continue next
                    }
                     // unwrap for cannot fail on valid syntax nodes
                     // to_owned for next need to parse a module with mutable reference of symbols, logically you have to cancel one of the reference
                     // todo long: this may not be a problem after non lexical lifetime
                    let module_name = symbols.get(import.name.value).unwrap().to_owned();
                    match SyntaxTree::search_module(&previous_module.source.get_directory(), &module_name) {
                        Ok(next_path) => {
                            let source = sources.add_file(next_path).map_err(|e| { messages.push(Message::new_simple(&format!("{:?}", e))); })?;
                            import_maps.push(ImportMap::new(previous_module.source.get_file_id(), import.name.value, source.get_file_id()));
                            next_modules.push(SyntaxTree::new_module(source, messages, symbols)?);
                        }
                        Err(possible_paths) => {
                            let mut helps = vec![format!("check existence or accessibility of these files:")];
                            let mut possible_paths = possible_paths.into_iter().map(|path| format!("{}", path.display()));
                            helps.extend(&mut possible_paths);
                            messages.push(Message::with_help(format!("failed to find module {}", module_name),
                                vec![(import.name.span, "import declared here".to_owned())], helps));
                        }
                    }
                }
            }
            processed_modules.append(&mut previous_modules);
            previous_modules.append(&mut next_modules);
        }
        Ok(SyntaxTree{ modules: processed_modules, import_maps: import_maps })
    }
}

#[cfg(test)] #[test]
fn syntax_tree_search_module() {
    // although it's designed to use absolute path, but relative path should be the same

    let search_module = SyntaxTree::search_module;
    macro_rules! collect { ($($component: expr),*) => ([$($component),*].into_iter().collect::<PathBuf>()) }

    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod"], "abc"), Ok(collect!["..", "tests", "syntax", "mod", "abc.ff"]));
    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod"], "efg"), Ok(collect!["..", "tests", "syntax", "mod", "efg", "module.ff"]));
    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod"], "other_name"), Ok(collect!["..", "tests", "syntax", "mod", "other_name.ff"]));
    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod"], "some_name"), Ok(collect!["..", "tests", "syntax", "mod", "some-name.ff"]));
    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod", "efg"], "this_name"), 
        Ok(collect!["..", "tests", "syntax", "mod", "efg", "this_name", "module.ff"])
    );
    assert_eq!(search_module(&collect!["..", "tests", "syntax", "mod", "efg"], "that_name"), 
        Ok(collect!["..", "tests", "syntax", "mod", "efg", "that-name", "module.ff"])
    );

    assert_eq!(search_module(&collect![".."], "xxx"), Err(vec![
        collect!["..", "xxx.ff"], collect!["..", "xxx", "module.ff"]
    ]));
    assert_eq!(search_module(&collect![".."], "xxx_yyy"), Err(vec![
        collect!["..", "xxx-yyy.ff"], collect!["..", "xxx_yyy.ff"], collect!["..", "xxx-yyy", "module.ff"], collect!["..", "xxx_yyy", "module.ff"]
    ]));
}

#[cfg(test)] #[test]
fn syntax_tree_recursive() {
    use codemap::Span;
    use super::Item;
    use super::ImportStatement;
    use super::SimpleName;
    macro_rules! collect { ($($component: expr),*) => ([$($component),*].into_iter().collect::<PathBuf>()) }

    let mut sources = SourceMap::new(collect!["..", "tests", "syntax", "mod", "main.ff"]).expect("open main file failed");
    let mut messages = make_messages![]; 
    let mut symbols = make_symbols!["abc", "efg", "some_name", "other_name", "this_name", "that_name"];

    let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).unwrap();
    assert_eq!(messages.is_empty(), true, "{:?}", messages);
    assert_eq!(syntax_tree, SyntaxTree::new_modules(vec![
        Module::new(sources.index(0).clone(), vec![
            Item::Import(ImportStatement::new_default(make_span!(0, 0, 10), SimpleName::new(make_id!(1), make_span!(0, 7, 9)))),
            Item::Import(ImportStatement::new_default(make_span!(0, 39, 49), SimpleName::new(make_id!(2), make_span!(0, 46, 48)))),
            Item::Import(ImportStatement::new_default(make_span!(0, 77, 93), SimpleName::new(make_id!(3), make_span!(0, 84, 92)))),
            Item::Import(ImportStatement::new_default(make_span!(0, 163, 180), SimpleName::new(make_id!(4), make_span!(0, 170, 179)))),
        ]),
        Module::new(sources.index(1).clone(), vec![]),
        Module::new(sources.index(2).clone(), vec![
            Item::Import(ImportStatement::new_default(make_span!(2, 0, 16), SimpleName::new(make_id!(5), make_span!(2, 7, 15)))),
            Item::Import(ImportStatement::new_default(make_span!(2, 69, 85), SimpleName::new(make_id!(6), make_span!(2, 76, 84)))),
        ]),
        Module::new(sources.index(3).clone(), vec![]),
        Module::new(sources.index(4).clone(), vec![]),
        Module::new(sources.index(5).clone(), vec![]),
        Module::new(sources.index(6).clone(), vec![]),
    ], vec![ // here import name = imported id is an accident
        ImportMap::new(0, make_id!(1), 1),
        ImportMap::new(0, make_id!(2), 2), 
        ImportMap::new(0, make_id!(3), 3),
        ImportMap::new(0, make_id!(4), 4),
        ImportMap::new(2, make_id!(5), 5),
        ImportMap::new(2, make_id!(6), 6)
    ]));

    // check source file name
    assert_eq!(sources.index(0).get_relative_path(), collect!["..", "tests", "syntax", "mod", "main.ff"]);
    assert_eq!(sources.index(1).get_relative_path(), collect!["..", "tests", "syntax", "mod", "abc.ff"]);
    assert_eq!(sources.index(2).get_relative_path(), collect!["..", "tests", "syntax", "mod", "efg", "module.ff"]);
    assert_eq!(sources.index(3).get_relative_path(), collect!["..", "tests", "syntax", "mod", "some-name.ff"]);
    assert_eq!(sources.index(4).get_relative_path(), collect!["..", "tests", "syntax", "mod", "other_name.ff"]);
    assert_eq!(sources.index(5).get_relative_path(), collect!["..", "tests", "syntax", "mod", "efg", "this_name", "module.ff"]);
    assert_eq!(sources.index(6).get_relative_path(), collect!["..", "tests", "syntax", "mod", "efg", "that-name", "module.ff"]);
}

#[cfg(test)] #[test]
fn syntax_tree_invalid_import() {
    // IMPL THIS
}