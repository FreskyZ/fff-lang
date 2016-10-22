
// WhileStatement = fWhile Expression Block

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::Block;

#[derive(Eq, PartialEq)]
pub struct WhileStatement {
    pub id: usize,
    pub expr: Expression,
    pub body: Block,
    pub pos: StringPosition, // while position
}

impl fmt::Debug for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}> while @ {:?} {:?} {:?}", self.id, self.pos, self.expr, self.body)
    }
}
impl fmt::Display for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}> while {} {}", self.id, self.expr, self.body)
    }
}

impl IASTItem for WhileStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos.start_pos, self.body.pos_all().end_pos) }

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

        (Some(WhileStatement{ id: 0, expr: expr, body: body, pos: pos}), current_len)
    }
}

#[cfg(test)]
mod tests {
    use super::WhileStatement;
    use syntax::ast_item::IASTItem;
    use lexical::Lexer;

    #[test]
    fn ast_stmt_while () {

        let (result, length) = WhileStatement::parse(&mut Lexer::new_test2("while true { writeln(\"fresky loves zmj\"); }"), 0);
        perrorln!("Debug: {:?}", result);
        perrorln!("Display: {}, {}", result.unwrap(), length);
    }
}