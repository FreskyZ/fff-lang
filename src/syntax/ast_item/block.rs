
// Block = fLeftBrace [Statement]* fRightBrace

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Statement;

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub stmts: Vec<Statement>, 
    pub pos: StringPosition,
}

impl Block {

    pub fn pos_all(&self) -> StringPosition { self.pos }
}

impl IASTItem for Block {

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Block>, usize) {

        if !lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) {
            return lexer.push_expect("left brace", index, 0);
        }

        let mut stmts = Vec::<Statement>::new();
        let mut stmts_sym_len = 0_usize;
        loop {
            match Statement::parse(lexer, index + stmts_sym_len) {
                (Some(expr), expr_len) => {
                    stmts_sym_len += expr_len;
                    stmts.push(expr);
                }
                (None, _) => break,
            }
        }

        if !lexer.nth(index + stmts_sym_len + 1).is_seperator(SeperatorKind::RightBrace) {
            lexer.push_expect("right brace", index + stmts_sym_len + 1, stmts_sym_len)
        } else {
            (Some(Block{ stmts: stmts, pos: StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + stmts_sym_len + 1).end_pos) }), stmts_sym_len + 2)
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_block_parse() {
        use super::Block;
        use syntax::ast_item::IASTItem;
        use lexical::Lexer;
        use message::MessageEmitter;
        use common::StringPosition;
        
        assert_eq!(Block::parse(&mut Lexer::new_test("{}", MessageEmitter::new()), 0), (Some(Block{ stmts: Vec::new(), pos: StringPosition::new() }), 2)); 
    }
}