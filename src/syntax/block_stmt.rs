///! fff-lang
///!
///! syntax/block_stmt
///! block-stmt = [ label-def ] block
///! block-stmt for explicit block definition in block and allow block label

use super::prelude::*;

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
