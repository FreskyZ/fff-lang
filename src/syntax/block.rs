///! fff-lang
///!
///! syntax/block
///! block = '{' { statement } '}'

use super::prelude::*;

impl Parser for Block {
    type Output = Block;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBrace)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Block, Unexpected> {

        let starting_span = cx.expect_sep(Separator::LeftBrace)?;
        let mut items = Vec::new();
        loop {
            if let Some(ending_span) = cx.try_expect_sep(Separator::RightBrace) {
                return Ok(Block::new(starting_span + ending_span, items));
            }
            items.push(cx.expect::<Statement>()?);
        }
    }
}

impl Node for Block {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_block(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for item in &self.items {
            v.visit_stmt(item)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn block_parse() {
    
    case!{ "{}" as Block,
        Block::new(Span::new(0, 1), vec![])
    }
}
