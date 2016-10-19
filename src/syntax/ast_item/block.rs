
// Block = fLeftBrace [Statement]* fRightBrace

use lexical::Lexer;
use lexical::IToken;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Statement;

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub stmts: Vec<Statement>, 
}

impl IASTItem for Block {

    fn symbol_len(&self) -> usize {
        self.stmts.iter().fold(0, |counter, ref stmt| counter + stmt.symbol_len())
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Block> {

        if !lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) {
            return lexer.push_expect_symbol("left brace", index);
        }

        let mut stmts = Vec::<Statement>::new();
        let mut stmts_sym_len = 0_usize;
        loop {
            match Statement::parse(lexer, index + stmts_sym_len) {
                Some(expr) => {
                    stmts_sym_len += expr.symbol_len();
                    stmts.push(expr);
                }
                None => break,
            }
        }

        if !lexer.nth(index + stmts_sym_len + 1).is_seperator(SeperatorKind::RightBrace) {
            lexer.push_expect_symbol("right brace", index + stmts_sym_len + 1)
        } else {
            Some(Block{ stmts: stmts })
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
        
        assert_eq!(Block::parse(&mut Lexer::new_test("{}", MessageEmitter::new()), 0), Some(Block{ stmts: Vec::new() })); 
    }
}