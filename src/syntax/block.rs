
#[cfg(test)]
use super::prelude::*;

#[cfg(test)] #[test]
fn block_parse() {
    
    case!{ "{}" as Block,
        Block::new(Span::new(0, 1), vec![])
    }
}
