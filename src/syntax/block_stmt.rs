///! fff-lang
///!
///! syntax/block_stmt
///! block-stmt = [ label-def ] block
///! block-stmt for explicit block definition in block and allow block label

use super::prelude::*;
use super::{Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
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
impl Node for BlockStatement {
    type ParseOutput = BlockStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool { 
        matches!((current, peek2), (Token::Label(_), Token::Sep(Separator::LeftBrace)) | (Token::Sep(Separator::LeftBrace), _))
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<BlockStatement> {
    
        let maybe_name = LabelDef::try_parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(BlockStatement::new(maybe_name, body));
    }
}

#[cfg(test)] #[test]
fn block_stmt_parse() {
    use super::make_node;

    assert_eq!{ make_node!("{}" as BlockStatement), BlockStatement::new_no_label(Block::new(Span::new(0, 1), vec![])) }
    assert_eq!{ make_node!("@: {}" as BlockStatement), 
        BlockStatement::new_with_label(
            LabelDef::new(1, Span::new(0, 1)),
            Block::new(Span::new(3, 4), vec![])
        )
    }
}