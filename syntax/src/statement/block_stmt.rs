///! fff-lang
///!
///! syntax/block_stmt
///! BlockStatement for explicit block definition in block and allow block label

// BlockStatement = [fLabel fColon] Block

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
#[cfg(feature = "parse_sess")] use lexical::SeperatorKind; // Remove this cfg after parse sess is applied

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BlockStatement {
    m_label: Option<LabelDef>,
    m_body: Block,
}
impl ISyntaxItemFormat for BlockStatement {
    fn format(&self, indent: u32) -> String {
        match self.m_label {
            Some(ref label_def) => format!("BlockStmt <{:?}>\n{}\n{}", 
                StringPosition::merge(label_def.get_all_strpos(), self.m_body.get_all_strpos()),
                label_def.format(indent + 1),
                self.m_body.format(indent + 1),
            ),
            None => format!("BlockStmt <{:?}>\n{}", 
                self.m_body.get_all_strpos(),
                self.m_body.format(indent + 1)
            )
        }
    }
}
impl fmt::Debug for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl BlockStatement {
    
    pub fn new_no_label(body: Block) -> BlockStatement { BlockStatement{ m_body: body, m_label: None } }
    pub fn new_with_label(label: LabelDef, body: Block) -> BlockStatement { BlockStatement { m_body: body, m_label: Some(label) } }
    #[cfg(feature = "parse_sess")] fn new_some_label(label: Option<LabelDef>, body: Block) -> BlockStatement { BlockStatement{ m_body: body, m_label: label } }

    pub fn has_label(&self) -> bool { self.m_label.is_some() }
    pub fn get_label(&self) -> Option<&LabelDef> { self.m_label.as_ref() }
    pub fn get_body(&self) -> &Block { &self.m_body }

    pub fn get_all_strpos(&self) -> StringPosition {
        match self.m_label {
            Some(ref label_def) => StringPosition::merge(label_def.get_all_strpos(), self.m_body.get_all_strpos()),
            None => self.m_body.get_all_strpos(),
        }
    }
}
impl ISyntaxItemGrammar for BlockStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        if let &Token::Label(_) = tokens.nth(index) { Block::is_first_final(tokens, index + 2) } else { Block::is_first_final(tokens, index) }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for BlockStatement {
    fn is_first_finalx(sess: &ParseSession) -> bool { 
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Sep(SeperatorKind::LeftBrace)) 
            | (&Token::Sep(SeperatorKind::LeftBrace), _) => true,
            _ => false,
        }
    }
}
impl ISyntaxItemParse for BlockStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BlockStatement>, usize) {

        match tokens.nth(index) {
            &Token::Label(_) => match LabelDef::parse(tokens, messages, index) {
                (None, length) => (None, length),
                (Some(label_def), _label_def_which_is_definitely_2) => match Block::parse(tokens, messages, index + 2) {
                    (None, length) => (None, length + 2),
                    (Some(block), block_length) => 
                        (Some(BlockStatement::new_with_label(label_def, block)), 2 + block_length),
                },
            },
            _ => match Block::parse(tokens, messages, index) {
                (None, length) => (None, length),
                (Some(block), block_length) => 
                    (Some(BlockStatement::new_no_label(block)), block_length),
            },
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for BlockStatement {

    fn parsex(sess: &mut ParseSession) -> ParseResult<BlockStatement> {
    
        let maybe_label = LabelDef::try_parse(sess)?;
        let body = Block::parsex(sess)?;
        return Ok(BlockStatement::new_some_label(maybe_label, body));
    }
}

#[cfg(test)] #[test]
fn block_stmt_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ BlockStatement::with_test_str("{}"), BlockStatement::new_no_label(Block::new(make_strpos!(1, 1, 1, 2), vec![])) }
    assert_eq!{ BlockStatement::with_test_str_ret_messages("@: {}"), (
        Some(BlockStatement::new_with_label(
            LabelDef::new("".to_owned(), make_strpos!(1, 1, 1, 2)),
            Block::new(make_strpos!(1, 4, 1, 5), vec![])
        )),
        make_messages![],
    )}
}