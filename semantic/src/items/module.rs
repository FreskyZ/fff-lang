///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SourceMap;
use codemap::SymbolCollection;

use super::super::Item;
use super::super::Formatter;
use super::super::FromSession;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct Module {
    pub module_id: usize,
    pub items: Vec<Item>,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for Module {

    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("module").lit("#").debug(&self.module_id).space().debug(&self.this_scope).endl();
        for item in &self.items {
            f = f.apply1(item);
        }
        f.finish()
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
    use super::super::Package;

    let mut index_file = File::open("../tests/syntax/inter/index.txt").expect("cannot open index.txt");
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
        let sources = make_sources![source];
        let package = Package::new(SyntaxTree::new_modules(vec![maybe_result.unwrap()], vec![]), &sources, &mut symbols, &mut messages);
        println!("{:?}", package);
        println!("{}", package.main_module.display());

        // let result = TestInput::new(&src).apply::<syntax::Module, _>().expect_no_message();
        // let actual = result.get_result().unwrap().format(Formatter::new(Some(result.get_source()), Some(result.get_symbols())));
        // if actual != expect {
        //     println!("case '{}' failed:", line);
        //     for (linenum, (actual_line, expect_line)) in actual.split_terminator('\n').zip(expect.split_terminator('\n')).enumerate() {
        //         if actual_line == expect_line {
        //             println!("={}) {}", linenum, actual_line);
        //         } else {
        //             println!("x{}) {:?}", linenum, actual_line);
        //             println!("x{}) {:?}", linenum, expect_line);
        //         }
        //     }
        //     panic!("case failed")
        // }
    }
    panic!("no reason");
}