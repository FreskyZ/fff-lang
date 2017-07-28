///! fff-lang
///! 
///! syntax/module, a source code file is a module
///! module = { item }

use std::fmt;

use lexical::Token;

use super::super::Item;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Module {
    pub items: Vec<Item>,
}
impl ISyntaxFormat for Module {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("module");
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
}
impl ISyntaxParse for Module {
    type Output = Module;

    fn parse(sess: &mut ParseSession) -> ParseResult<Module> {
        let mut items = Vec::new();
        loop {
            if Item::matches_first(sess.current_tokens()) {
                items.push(Item::parse(sess)?);
            } else if sess.current_tokens()[0] == &Token::EOF { // as module is special, specially allow self.current_tokens in parse
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
    use codemap::Span;
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::TestInput;
    use super::super::UseStatement;
    use super::super::Name;
    use super::super::SimpleName;
    use super::super::ImportStatement;
    use super::super::SimpleExprStatement;
    use super::super::LitExpr;

    //              0123456789012345678901234
    TestInput::new("use a; import b; 3; b; a;")
        .set_syms(make_symbols!["a", "b"])
        .apply::<Module, _>()
        .expect_no_message()
        .expect_result(Module::new(vec![
            Item::Use(UseStatement::new_default(make_span!(0, 5), 
                Name::new(make_span!(4, 4), vec![
                    SimpleName::new(make_id!(1), make_span!(4, 4))
                ])
            )),
            Item::Import(ImportStatement::new_default(make_span!(7, 15), 
                SimpleName::new(make_id!(2), make_span!(14, 14))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(make_span!(17, 18), 
                LitExpr::new(LitValue::from(3), make_span!(17, 17))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(make_span!(20, 21), 
                SimpleName::new(make_id!(2), make_span!(20, 20))
            )),
            Item::SimpleExpr(SimpleExprStatement::new(make_span!(23, 24), 
                SimpleName::new(make_id!(1), make_span!(23, 23))
            )),
        ]))
    .finish();
}

#[cfg(test)] #[test]
fn module_integration() {
    use std::fs::File;
    use std::io::Read;
    use super::super::TestInput;

    let mut index_file = File::open("../tests/syntax/index.txt").expect("cannot open index.txt");
    let mut test_cases = String::new();
    let _length = index_file.read_to_string(&mut test_cases).expect("cannot read index.txt");
    for line in test_cases.lines() {
        let src_path = "../tests/syntax/".to_owned() + line + "_src.ff";
        let mut src_file = File::open(&src_path).expect(&format!("cannot open src file {}", src_path));
        let mut src = String::new();
        let _length = src_file.read_to_string(&mut src).expect(&format!("cannot read src file {}", src_path));
        let result_path = "../tests/syntax/".to_owned() + line + "_result.txt";
        let mut result_file = File::open(&result_path).expect(&format!("cannot open result file {}", result_path));
        let mut expect = String::new();
        let _length = result_file.read_to_string(&mut expect).expect(&format!("cannot read result file {}", result_path));
        
        let result = TestInput::new(&src).apply::<Module, _>().expect_no_message();
        let actual = result.get_result().unwrap().format(Formatter::new(Some(result.get_source()), Some(result.get_symbols())));
        if actual != expect {
            panic!("case '{}' failed, actual:\n`{}`\nexpect:\n`{}`", line, actual, expect)
        }
    }
}