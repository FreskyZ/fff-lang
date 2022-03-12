///! fff-lang
///!
///! syntax/block
///! block = '{' { statement } '}'

use std::fmt;
use crate::source::{FileSystem, Span};
use crate::lexical::Token;
use crate::lexical::Separator;
use super::super::Statement;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

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
impl ISyntaxGrammar for Block {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Sep(Separator::LeftBrace)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for Block where F: FileSystem {
    type Output = Block;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Block> {

        let starting_span = sess.expect_sep(Separator::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = sess.try_expect_sep(Separator::RightBrace) {
                return Ok(Block::new(starting_span + ending_span, items));
            }
            items.push(Statement::parse(sess)?);
        }
    }
}

#[cfg(test)] #[test]
fn block_parse() {
    use super::super::WithTestInput;
    
    assert_eq!{ make_node!("{}"),
        Block::new(Span::new(0, 1), vec![])
    }
}