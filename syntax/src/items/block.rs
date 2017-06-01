///! fff-lang
///!
///! syntax/block
///! Block = fLeftBrace [Statement]* fRightBrace

use std::fmt;

use codemap::StringPosition;
use lexical::Token;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Statement;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Block {
    items: Vec<Statement>,
    all_strpos: StringPosition,
}
impl ISyntaxItemFormat for Block {
    fn format(&self, indent: u32) -> String {
        format!("{}Block {}<{:?}>{}",
            Block::indent_str(indent), if self.items.is_empty() { "(empty) " } else { "" }, self.all_strpos,
            self.items.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 1)); buf })
        )
    }
}
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl Block {

    pub fn new(all_strpos: StringPosition, statements: Vec<Statement>) -> Block { Block{ all_strpos: all_strpos, items: statements } }

    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
    pub fn get_statements(&self) -> &Vec<Statement> { &self.items }
}
impl ISyntaxItemGrammar for Block {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftBrace) }
}
impl ISyntaxItemParse for Block {

    fn parse(sess: &mut ParseSession) -> ParseResult<Block> {

        let starting_strpos = sess.expect_sep(SeperatorKind::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let (&Token::Sep(SeperatorKind::RightBrace), ref right_brace_strpos) = (sess.tk, sess.pos) {
                sess.move_next();
                return Ok(Block::new(StringPosition::merge(starting_strpos, *right_brace_strpos), items));
            }
            items.push(Statement::parse(sess)?); 
        }
    }
}

#[cfg(test)] #[test]
fn block_parse() {
    use super::super::ISyntaxItemWithStr;
    use message::MessageCollection;
    
    assert_eq!{ Block::with_test_str_ret_size_messages("{}"), (
        Some(Block::new(make_strpos!(1, 1, 1, 2), vec![])), 
        2,
        make_messages![],
    )}
}