///! fff-lang
///!
///! syntax/block_stmt
///! BlockStatement for explicit block definition in block and allow block label
///! BlockStatement = [LabelDef] Block

use std::fmt;

use codemap::StringPosition;
use lexical::Token;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
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
    fn new_some_label(label: Option<LabelDef>, body: Block) -> BlockStatement { BlockStatement{ m_body: body, m_label: label } }

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
    fn is_first_final(sess: &ParseSession) -> bool { 
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Sep(SeperatorKind::LeftBrace)) 
            | (&Token::Sep(SeperatorKind::LeftBrace), _) => true,
            _ => false,
        }
    }
}
impl ISyntaxItemParse for BlockStatement {

    fn parse(sess: &mut ParseSession) -> ParseResult<BlockStatement> {
    
        let maybe_label = LabelDef::try_parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(BlockStatement::new_some_label(maybe_label, body));
    }
}

#[cfg(test)] #[test]
fn block_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use message::MessageCollection;

    assert_eq!{ BlockStatement::with_test_str("{}"), BlockStatement::new_no_label(Block::new(make_strpos!(1, 1, 1, 2), vec![])) }
    assert_eq!{ BlockStatement::with_test_str_ret_messages("@: {}"), (
        Some(BlockStatement::new_with_label(
            LabelDef::new("".to_owned(), make_strpos!(1, 1, 1, 2)),
            Block::new(make_strpos!(1, 4, 1, 5), vec![])
        )),
        make_messages![],
    )}
}