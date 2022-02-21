
// WhileStatement = fWhile Expression Block

use std::fmt;

use crate::common::From2;
use crate::common::StringPosition;

use crate::lexical::Lexer;
use crate::lexical::KeywordKind;

use crate::syntax::ast_item::IASTItem;
use crate::syntax::Expression;
use crate::syntax::Block;

#[derive(Eq, PartialEq)]
pub struct WhileStatement {
    pub expr: Expression,
    pub body: Block,
    pub pos: StringPosition, // while position
}

impl fmt::Debug for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while @ {:?} {:?} {:?}", self.pos, self.expr, self.body)
    }
}
impl fmt::Display for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} {}", self.expr, self.body)
    }
}

impl IASTItem for WhileStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos.start_pos, self.body.pos_all().end_pos) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::While)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<WhileStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::While) {
            unreachable!()
        }
        let pos = lexer.pos(index);
        let mut current_len = 1;

        let expr = match Expression::parse(lexer, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        let body = match Block::parse(lexer, index + current_len) {
            (Some(block), block_len) => { current_len += block_len; block }
            (None, length) => return (None, current_len + length),
        };

        (Some(WhileStatement{ expr: expr, body: body, pos: pos}), current_len)
    }
}

#[cfg(test)]
mod tests {
    use super::WhileStatement;
    use crate::syntax::ast_item::IASTItem;
    use crate::lexical::Lexer;

    #[test]
    fn ast_stmt_while () {

        let (result, length) = WhileStatement::parse(&mut Lexer::new("while true { writeln(\"fresky loves zmj\"); }"), 0);
        perrorln!("Debug: {:?}", result);
        perrorln!("Display: {}, {}", result.unwrap(), length);
    }
}