///! fff-lang
///!
///! syntax/block
///! Block = fLeftBrace [Statement]* fRightBrace

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
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
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index) == &Token::Sep(SeperatorKind::LeftBrace) 
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for Block {
    fn is_first_finalx(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftBrace) }
}
impl ISyntaxItemParse for Block {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<Block>, usize) {
        assert!(tokens.nth(index) == &Token::Sep(SeperatorKind::LeftBrace));

        let mut items = Vec::new();
        let mut current_length = 1;
        loop {
            if tokens.nth(index + current_length) == &Token::Sep(SeperatorKind::RightBrace) {
                return (Some(Block::new(StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length)), items)), current_length + 1);
            }
            match Statement::parse(tokens, messages, index + current_length) {
                (Some(stmt), stmt_len) => {
                    current_length += stmt_len;
                    items.push(stmt);
                    continue;
                }
                (None, length) => return (None, current_length + length),
            }
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for Block {

    fn parsex(sess: &mut ParseSession) -> ParseResult<Block> {

        let starting_strpos = sess.expect_sep(SeperatorKind::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let (&Token::Sep(SeperatorKind::RightBrace), ref right_brace_strpos) = (sess.tk, sess.pos) {
                sess.move_next();
                return Ok(Block::new(StringPosition::merge(starting_strpos, *right_brace_strpos), items));
            }
            items.push(Statement::parsex(sess)?); 
        }
    }
}

#[cfg(test)] #[test]
fn block_parse() {
    use super::super::ISyntaxItemWithStr;
    
    assert_eq!{ Block::with_test_str_ret_size_messages("{}"), (
        Some(Block::new(make_strpos!(1, 1, 1, 2), vec![])), 
        2,
        make_messages![],
    )}
}