///! fff-lang
///!
///! syntax/block
///! Block = fLeftBrace [Statement]* fRightBrace

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Seperator;

use super::super::Statement;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Block {
    pub items: Vec<Statement>,
    pub all_span: Span,
}
impl ISyntaxFormat for Block {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("block").space().span(self.all_span);
        if self.items.is_empty() { 
            f.endl().indent1().lit("no-item").finish()
        } else {
            for item in &self.items { f = f.endl().apply1(item); }
            f.finish()
        }
    }
}
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl Block {

    pub fn new(all_span: Span, statements: Vec<Statement>) -> Block { Block{ all_span, items: statements } }
}
impl ISyntaxItemGrammar for Block {
    fn is_first_final(sess: &ParseSession) -> bool { sess.current_tokens()[0] == &Token::Sep(Seperator::LeftBrace) }
}
impl ISyntaxItemParse for Block {
    type Target = Block;

    fn parse(sess: &mut ParseSession) -> ParseResult<Block> {

        let starting_span = sess.expect_sep(Seperator::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = sess.try_expect_sep(Seperator::RightBrace) {
                return Ok(Block::new(starting_span.merge(&ending_span), items));
            }
            items.push(Statement::parse(sess)?);
        }
    }
}

#[cfg(test)] #[test]
fn block_parse() {
    use super::super::WithTestInput;
    
    assert_eq!{ Block::with_test_str("{}"),
        Block::new(make_span!(0, 1), vec![])
    }
}