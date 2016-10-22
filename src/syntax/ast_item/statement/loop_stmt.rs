
// LoopStatement = fLoop Block

use std::fmt;

use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::Block;

#[derive(Eq, PartialEq)]
pub struct LoopStatement {
    pub id: usize,
    pub body: Block,
    pub pos: StringPosition,   // position for 'loop'
}

impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>loop @ {:?} {:?}", self.id, self.pos, self.body)
    }
}
impl fmt::Display for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>loop {}", self.id, self.body)
    }
}

impl IASTItem for LoopStatement {

    fn pos_all(&self) -> StringPosition { self.pos }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<LoopStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::Loop) {
            unreachable!()
        }

        match Block::parse(lexer, index + 1) {
            (Some(block), block_len) => (Some(LoopStatement{ id: 0, body: block, pos: lexer.pos(index), }), block_len + 1),
            (None, length) => (None, length + 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LoopStatement;
    use lexical::Lexer;
    use syntax::ast_item::IASTItem;

    #[test]
    fn ast_stmt_loop() {

        let (result, len) = LoopStatement::parse(&mut Lexer::new_test2("loop { writeln(\"love zmj\"); }"), 0);
        perrorln!("Debug: {:?}", result);
        perrorln!("Display: {}, {}", result.unwrap(), len); 
    }
}