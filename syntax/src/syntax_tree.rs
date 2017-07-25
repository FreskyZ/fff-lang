///! fff-lang
///!
///! syntax/root
///! root = { item }

use std::fmt;

use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;

use super::Item;
use super::Formatter;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxItemParse;
use super::ISyntaxFormat;
use super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub items: Vec<Item>,
}
impl ISyntaxFormat for SyntaxTree {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("syntax-tree");
        for item in &self.items {
            f = f.endl().apply1(item);
        }
        f.finish()
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl SyntaxTree {
    pub fn new_items(items: Vec<Item>) -> SyntaxTree { SyntaxTree{ items } }
}
impl ISyntaxItemParse for SyntaxTree {
    type Target = SyntaxTree;

    fn parse(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {

        let mut items = Vec::new();
        loop {
            if Item::is_first_final(sess) {
                items.push(Item::parse(sess)?);
            } else if sess.tk == &Token::EOF {
                break;
            } else {
                return sess.push_unexpect("if, while, for, var, const, expr");
            }
        }
        return Ok(SyntaxTree::new_items(items));
    }
}
impl SyntaxTree {
    pub fn new(tokens: &TokenStream, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> SyntaxTree {
        let mut sess = ParseSession::new(tokens, messages, symbols);
        match SyntaxTree::parse(&mut sess) {
            Ok(tree) => tree,
            Err(_) => SyntaxTree::new_items(Vec::new()),
        }
    }
}

#[cfg(test)] #[test]
fn syntax_tree_parse() {
    use std::fs::File;
    use std::io::Read;
    use super::TestInput;

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
        
        let result = TestInput::new(&src).apply::<SyntaxTree, _>().expect_no_message();
        let actual = result.get_result().unwrap().format(Formatter::new(Some(result.get_source()), Some(result.get_symbols())));
        if actual != expect {
            panic!("case '{}' failed, actual:\n`{}`\nexpect:\n`{}`", line, actual, expect)
        }
    }
}

// TODO: case `fn main() { println("hello") }
// current message: Unexpect symbol, meet }, expect assignment operator, semicolon
// expect message: unexpect symbol, expect `;`, `.`, `(`, `+`, etc., meet `}`
// and recover this case
