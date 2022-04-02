
#[cfg(test)]
use super::prelude::*;

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
