///! fff-lang
///! 
///! syntax/module, a source code file is a module
///! module = { item }

use crate::syntax::prelude::*;
use super::{Item, ImportStatement};

#[cfg_attr(test, derive(PartialEq))]
pub struct Module {
    pub items: Vec<Item>,
}
impl ISyntaxFormat for Module {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("module");
        if self.items.len() == 0 {
            f = f.endl().indent1().lit("no-item");
        }
        for item in &self.items {
            f = f.endl().apply1(item);
        }
        f.finish()
    }
}
impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl Module {
    pub fn new(items: Vec<Item>) -> Module { Module{ items } }

    // TODO: update to `impl Iterator<Item = Item>` and remove collect after stabilize
    pub fn import_statements(&self) -> Vec<&ImportStatement> {
        self.items.iter()
            .map(|ref item| if let &&Item::Import(ref import_stmt) = item { Some(import_stmt) } else { None })
            .filter(|maybe_import| maybe_import.is_some())
            .map(|maybe_import| maybe_import.unwrap())
            .collect()
    }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for Module where F: FileSystem {
    type Output = Module;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Module> {
        let mut items = Vec::new();
        loop {
            if Item::matches_first(sess.current_tokens()) {
                items.push(Item::parse(sess)?);
            } else if matches!(sess.current_tokens()[0], &Token::EOF) { // as module is special, specially allow self.current_tokens in parse
                break;
            } else {
                return sess.push_unexpect("if, while, for, var, const, expr");
            }
        }
        return Ok(Module::new(items));
    }
}

#[cfg(test)] #[test]
fn module_parse() {
    use super::{make_node, LitValue, LitExpr, Name, SimpleName, ImportStatement, SimpleExprStatement, UseStatement};
    //              0123456789012345678901234
    assert_eq!{ make_node!("use a; import b; 3; b; a;" as Module, [], ["a", "b"]),
        Module::new(vec![
            Item::Use(UseStatement::new_default(Span::new(0, 5), 
                Name::new(Span::new(4, 4), vec![
                    SimpleName::new(1, Span::new(4, 4))
                ])
            )),
            Item::Import(ImportStatement::new_default(Span::new(7, 15), 
                SimpleName::new(2, Span::new(14, 14))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(17, 18), 
                LitExpr::new(LitValue::from(3i32), Span::new(17, 17))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(20, 21), 
                SimpleName::new(2, Span::new(20, 20))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(23, 24), 
                SimpleName::new(1, Span::new(23, 23))
            )),
        ])
    }
}

#[cfg(test)] #[test]
fn module_integration() {
    // use std::fs::File;
    // use std::io::Read;

    // let mut index_file = File::open("tests/syntax/inter/index.txt").expect("cannot open index.txt");
    // let mut test_cases = String::new();
    // let _length = index_file.read_to_string(&mut test_cases).expect("cannot read index.txt");
    // for line in test_cases.lines() {
    //     let src_path = "tests/syntax/inter/".to_owned() + line + "_src.ff";
    //     let mut src_file = File::open(&src_path).expect(&format!("cannot open src file {}", src_path));
    //     let mut src = String::new();
    //     let _length = src_file.read_to_string(&mut src).expect(&format!("cannot read src file {}", src_path));
    //     let result_path = "tests/syntax/inter/".to_owned() + line + "_result.txt";
    //     let mut result_file = File::open(&result_path).expect(&format!("cannot open result file {}", result_path));
    //     let mut expect = String::new();
    //     let _length = result_file.read_to_string(&mut expect).expect(&format!("cannot read result file {}", result_path));
    //     let expect = expect.replace("\r\n", "\n");

    //     let result = TestInput::new(&src).apply::<Module, _>().expect_no_message();
    //     let actual = result.get_result().unwrap().format(Formatter::new(Some(result.get_source()), Some(result.get_symbols())));
    //     if actual != expect {
    //         println!("case '{}' failed:", line);
    //         let (mut linenum, mut actual_iter, mut expect_iter) = (0, actual.split_terminator('\n'), expect.split_terminator('\n'));
    //         loop {
    //             match (actual_iter.next(), expect_iter.next()) {
    //                 (Some(actual_line), Some(expect_line)) if actual_line == expect_line => {
    //                     println!("={}) {}", linenum, actual_line);
    //                 }
    //                 (Some(actual_line), Some(expect_line)) if actual_line != expect_line => {
    //                     println!("x{}) {:?}", linenum, actual_line);
    //                     println!("x{}) {:?}", linenum, expect_line);
    //                 }
    //                 (Some(_actual_line), Some(_expect_line)) => {
    //                     panic!("what's the case??");
    //                 }
    //                 (Some(actual_line), None) => {
    //                     println!("^{}) {}", linenum, actual_line);
    //                 }
    //                 (None, Some(expect_line)) => {
    //                     println!("v{}) {}", linenum, expect_line);
    //                 }
    //                 (None, None) => break,
    //             }
    //             linenum += 1;
    //         }
    //         panic!("case failed")
    //     }
    // }
}
