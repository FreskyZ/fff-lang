///! fff-lang
///! 
///! syntax/module, a source code file is a module
///! module = { item }

use crate::source::FileId;
use super::prelude::*;
use super::{Item};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct Module {
    pub file: FileId,
    pub items: Vec<Item>,
}

impl Parser for Module {
    type Output = Module;

    fn parse(cx: &mut ParseContext) -> Result<Module, Unexpected> {
        let mut items = Vec::new();
        while !cx.eof() {
            items.push(cx.expect::<Item>()?);
        }
        Ok(Module{ items, file: cx.get_file_id() })
    }
}

impl Node for Module {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_module(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for item in &self.items {
            v.visit_item(item)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn module_parse() {
    use super::{ModuleStatement, SimpleExprStatement, UseStatement};
    //                      0123456789012345678901234
    case!{ "use a; module b; 3; b; a;" as Module,
        Module{ file: FileId::new(1), items: vec![
            Item::Use(UseStatement{ all_span: Span::new(0, 5), alias: None,
                name: make_name!(simple bare 4:4 #2) }),
            Item::Import(ModuleStatement{ name: IsId::new(3), name_span: Span::new(14, 14), path: None, all_span: Span::new(7, 15) }),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(17, 18), 
                make_lit!(3, 17, 17)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(20, 21), 
                make_name!(simple 20:20 #3)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(23, 24), 
                make_name!(simple 23:23 #2)
            )),
        ] }
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
