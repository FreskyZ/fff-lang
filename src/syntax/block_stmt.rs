///! fff-lang
///!
///! syntax/block_stmt
///! block-stmt = [ label-def ] block
///! block-stmt for explicit block definition in block and allow block label

use super::prelude::*;
use super::{Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct BlockStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub all_span: Span,
}

impl Parser for BlockStatement {
    type Output = BlockStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool { 
        matches!((current, peek2), (Token::Label(_), Token::Sep(Separator::LeftBrace)) | (Token::Sep(Separator::LeftBrace), _))
    }

    fn parse(cx: &mut ParseContext) -> Result<BlockStatement, Unexpected> {
    
        let name = cx.try_expect::<LabelDef>()?;
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(body.all_span) + body.all_span;
        Ok(BlockStatement{ all_span, name, body })
    }
}

impl Node for BlockStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_block_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.name {
            v.visit_label_def(name)?;
        }
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn block_stmt_parse() {

    case!{ "{}" as BlockStatement, 
        BlockStatement{ name: None, all_span: Span::new(0, 1), 
            body: Block::new(Span::new(0, 1), vec![]) }
    }

    case!{ "@: {}" as BlockStatement,
        BlockStatement{ all_span: Span::new(0, 4),
            name: Some(LabelDef::new(1, Span::new(0, 1))),
            body: Block::new(Span::new(3, 4), vec![]) }
    }
}
