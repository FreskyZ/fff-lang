///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SourceMap;
use codemap::SymbolCollection;

use super::Item;
use super::Formatter;
use super::FromSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct Module {
    pub module_id: usize,
    pub items: Vec<Item>,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for Module {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("module").lit("#").debug(&self.module_id).endl()
            .this_scope1(&self.this_scope)
            .foreach(&self.items, |f, item| f.endl().apply1(item)).finish()
    }

    type SyntaxItem = syntax::Module;

    // here the trait method's second parameter changes from parent_scope to this_scope
    // because multiple file support in Package need more control of module's scope
    fn from_syntax(node: syntax::Module, sess: FromSession) -> Module {
        Module{
            module_id: node.source.get_file_id(),
            items: node.items.into_iter().map(|item| Item::from_syntax(item, sess.clone_scope())).collect(),
            this_scope: sess.into_scope(),
        }
    }

    fn collect_type_declarations(&mut self) {

    }
}
impl Module {

    pub fn buildup_imports(&mut self, import_maps: &Vec<syntax::ImportMap>, modules: &mut Vec<syntax::Module>, sources: &SourceMap, symbols: &SymbolCollection) {

        let module_id = self.module_id; // if use `self.module_id` in the for expr then rustc complains about mutably borrowed self in `&mut self.items` ... hope non lexical lifetime solve this
        for item in &mut self.items {
            if let &mut Item::Import(ref mut import_stmt) = item {
                let imported_file_id = import_maps.into_iter()
                    .filter(|import_map| import_map.file_id == module_id && import_map.import_name == import_stmt.name.value)
                    .next().unwrap().imported_file_id;  // valid syntax tree will make sure this next and this unwrap is success
                let mut imported_module = Module::from_syntax(
                    modules[imported_file_id].move_out(), 
                    FromSession::new(self.this_scope.clone(), sources.index(imported_file_id).as_ref(), symbols).sub_with_symbol(import_stmt.name.value)
                );
                imported_module.buildup_imports(import_maps, modules, sources, symbols);
                import_stmt.module = Some(imported_module);
            }
        }
    }
}

#[cfg(test)] #[test]
fn scope_management_integration() {
    use std::fs::File;
    use std::io::Read;
    use syntax::TestInput;
    use syntax::WithTestInput;
    use syntax::SyntaxTree;
    use super::Package;

    let mut index_file = File::open("../tests/syntax/inter/index-semantic-p1.txt").expect("cannot open index.txt");
    let mut test_cases = String::new();
    let _length = index_file.read_to_string(&mut test_cases).expect("cannot read index.txt");
    for line in test_cases.lines() {
        let src_path = "../tests/syntax/inter/".to_owned() + line + "_src.ff";
        let mut src_file = File::open(&src_path).expect(&format!("cannot open src file {}", src_path));
        let mut src = String::new();
        let _length = src_file.read_to_string(&mut src).expect(&format!("cannot read src file {}", src_path));
        let result_path = "../tests/syntax/inter/".to_owned() + line + "_result.txt";
        let mut result_file = File::open(&result_path).expect(&format!("cannot open result file {}", result_path));
        let mut expect = String::new();
        let _length = result_file.read_to_string(&mut expect).expect(&format!("cannot read result file {}", result_path));
        let _expect = expect.replace("\r\n", "\n");
        
        let (maybe_result, source, mut messages, mut symbols) = syntax::Module::with_test_input(TestInput::new(&src));
        let syntax_module = maybe_result.expect(&format!("failed to syntax parse, messages: {:?}", messages));
        let sources = make_sources![source];
        let package = Package::new(SyntaxTree::new_modules(vec![syntax_module], vec![]), &sources, &mut symbols, &mut messages);
        println!("{}", package.main_module.format(Formatter::new(Some(sources.index(0).as_ref()), Some(&symbols))));
    }
    // panic!("no reason");
}