///! fff-lang
///!
///! syntax/block_stmt
///! BlockStatement for explicit block definition in block and allow block label

// BlockStatement = [fLabel fColon] Block

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::TokenStream;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::Block;
use super::super::LabelDef;

#[derive(Eq, PartialEq)]
pub struct BlockStatement {
    m_label: Option<LabelDef>,
    m_body: Block,
}
impl ISyntaxItemFormat for BlockStatement {
    fn format(&self, indent: u32) -> String {
        match self.m_label {
            Some(ref label_def) => format!("BlockStmt <{:?}>\n{}\n{:?}", 
                StringPosition::merge(label_def.get_all_strpos(), self.m_body.pos_all()),
                label_def.format(indent + 1),
                self.m_body,
            ),
            None => format!("BlockStmt <{:?}>\n{:?}", 
                self.m_body.pos_all(),
                self.m_body
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

    pub fn has_label(&self) -> bool { self.m_label.is_some() }
    pub fn get_label(&self) -> Option<&LabelDef> { self.m_label.as_ref() }
    pub fn get_body(&self) -> &Block { &self.m_body }

    pub fn get_all_strpos(&self) -> StringPosition {
        match self.m_label {
            Some(ref label_def) => StringPosition::merge(label_def.get_all_strpos(), self.m_body.pos_all()),
            None => self.m_body.pos_all(),
        }
    }
}
impl ISyntaxItem for BlockStatement {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() }
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        (tokens.nth(index).is_label() && Block::is_first_final(tokens, index + 2) ) || Block::is_first_final(tokens, index)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BlockStatement>, usize) {

        match tokens.nth(index).get_label() {
            None => match Block::parse(tokens, messages, index) {
                (None, length) => (None, length),
                (Some(block), block_length) => 
                    (Some(BlockStatement::new_no_label(block)), block_length),
            },
            Some(_) => match LabelDef::parse(tokens, messages, index) {
                (None, length) => (None, length),
                (Some(label_def), _label_def_which_is_definitely_2) => match Block::parse(tokens, messages, index + 2) {
                    (None, length) => (None, length + 2),
                    (Some(block), block_length) => 
                        (Some(BlockStatement::new_with_label(label_def, block)), 2 + block_length),
                },
            },
        }
    }
}