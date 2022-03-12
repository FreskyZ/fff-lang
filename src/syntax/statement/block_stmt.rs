///! fff-lang
///!
///! syntax/block_stmt
///! block-stmt = [ label-def ] block
///! block-stmt for explicit block definition in block and allow block label

use std::fmt;
use crate::source::{FileSystem, Span};
use crate::lexical::{Token, Separator};
use super::super::Block;
use super::super::LabelDef;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
}
impl ISyntaxFormat for BlockStatement {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("block-stmt").space().span(self.all_span).endl();
        match self.name { Some(ref name) => f = f.apply1(name).endl(), None => () }
        f.set_header_text("body").apply1(&self.body).finish()
    }
}
impl fmt::Debug for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl BlockStatement {
    
    pub fn new_no_label(body: Block) -> BlockStatement { BlockStatement{ all_span: body.all_span, body, name: None } }
    pub fn new_with_label(name: LabelDef, body: Block) -> BlockStatement { 
        BlockStatement { all_span: name.all_span + body.all_span, body, name: Some(name) } 
    }

    fn new(name: Option<LabelDef>, body: Block) -> BlockStatement { 
        BlockStatement{ 
            all_span: match name { Some(ref name) => name.all_span + body.all_span, None => body.all_span },
            body, name
        } 
    }
}
impl ISyntaxGrammar for BlockStatement {
    fn matches_first(tokens: [&Token; 3]) -> bool { 
        match (tokens[0], tokens[2]) {
            (&Token::Label(_), &Token::Sep(Separator::LeftBrace)) 
            | (&Token::Sep(Separator::LeftBrace), _) => true,
            _ => false,
        }
    }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for BlockStatement where F: FileSystem {
    type Output = BlockStatement;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<BlockStatement> {
    
        let maybe_name = LabelDef::try_parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(BlockStatement::new(maybe_name, body));
    }
}

#[cfg(test)] #[test]
fn block_stmt_parse() {
    use super::super::WithTestInput;

    assert_eq!{ make_node!("{}"), BlockStatement::new_no_label(Block::new(Span::new(0, 1), vec![])) }
    assert_eq!{ make_node!("@: {}"), 
        BlockStatement::new_with_label(
            LabelDef::new(1, Span::new(0, 1)),
            Block::new(Span::new(3, 4), vec![])
        )
    }
}