///! fff-lang
///!
///! syntax/root
///! root = { stmt }

use std::fmt;

use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;

use super::super::Statement;
use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SyntaxTree {
    pub items: Vec<Statement>,
}
impl ISyntaxItemFormat for SyntaxTree {
    fn format(&self, indent: u32) -> String {
        format!("{}SyntaxTree{}", 
            SyntaxTree::indent_str(indent),
            self.items.iter().fold(String::new(), |mut buf, item| { buf.push_str("\n"); buf.push_str(&item.format(indent + 1)); buf }))
    }
}
impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl SyntaxTree {
    pub fn new_items(items: Vec<Statement>) -> SyntaxTree { SyntaxTree{ items } }
}
impl ISyntaxItemParse for SyntaxTree {
    type Target = SyntaxTree;

    fn parse(sess: &mut ParseSession) -> ParseResult<SyntaxTree> {

        let mut items = Vec::new();
        loop {
            if Statement::is_first_final(sess) {
                items.push(Statement::parse(sess)?);
            } else if sess.tk == &Token::EOF {
                break;
            } // else if sess.tk == &Token::EOFs { break; }
        }
        return Ok(SyntaxTree::new_items(items));
    }
}
impl SyntaxTree {
    pub fn new(tokens: &mut TokenStream, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> SyntaxTree {
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
    use codemap::SymbolCollection;
    use super::super::ISyntaxItemWithStr;

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
        
        let actual = SyntaxTree::with_test_input(&src, &mut make_symbols![]).format(0);
        if actual != expect {
            panic!("case {} failed, actual:\n`{}`\nexpect:\n`{}`", line, actual, expect)
        }
    }

    // test_case!("../tests/syntax/hello.sm");
    // test_case!("../tests/syntax/list.sm");
    // test_case!("../tests/syntax/prime.sm");
    // test_case!("../tests/syntax/string.sm");
}

// TODO: update format including format_with_codemap_symbols for these integration tests
// in detail, use struct codemap::SourceSession{ symbols: Option<&SymbolCollection>, codemap: Option<&CodeMap>, indention: usize }
// also a `human_friendly_eqer` for line:column compare instead of byte index compare, string compare instead of symid compare
