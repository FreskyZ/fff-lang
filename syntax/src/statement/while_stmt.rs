
// WhileStatement = fWhile Expression Block

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::Lexer;
use lexical::KeywordKind;

use super::super::ast_item::ISyntaxItem;
use super::super::Expression;
use super::super::Block;

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

impl ISyntaxItem for WhileStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.pos, self.body.pos_all()) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::While)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<WhileStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::While) {
            unreachable!()
        }
        let pos = lexer.pos(index);
        let mut current_len = 1;

        let expr = match Expression::parse(lexer, messages, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        let body = match Block::parse(lexer, messages, index + current_len) {
            (Some(block), block_len) => { current_len += block_len; block }
            (None, length) => return (None, current_len + length),
        };

        (Some(WhileStatement{ expr: expr, body: body, pos: pos}), current_len)
    }
}

#[cfg(test)]
mod tests {
    use super::WhileStatement;
    use super::super::super::ast_item::ISyntaxItem;

    #[test]
    fn ast_stmt_while () {

        let result = WhileStatement::with_test_str("while true { writeln(\"fresky loves zmj\"); }");
        perrorln!("Debug: {:?}", result);
    }
}