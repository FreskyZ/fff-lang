
// Block = fLeftBrace [Statement]* fRightBrace

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;

use super::ISyntaxItem;
use super::Statement;

#[derive(Eq, PartialEq)]
pub struct Block {
    pub stmts: Vec<Statement>, 
    pub pos: StringPosition,   // pos for { and }
}
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\n{}\n}} @ {:?}", format_vector_debug(&self.stmts, "\n"), self.pos)
    }
}
impl ISyntaxItem for Block {
    
    fn pos_all(&self) -> StringPosition { self.pos }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) 
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<Block>, usize) {

        if !lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) {
            return push_unexpect!(lexer, messages, "left brace", index, 0);
        }

        let mut stmts = Vec::new();
        let mut current_len = 1;
        loop {
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightBrace) {
                return (Some(Block{
                    stmts: stmts,
                    pos: StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len))
                }), current_len + 1);
            }
            match Statement::parse(lexer, messages, index + current_len) {
                (Some(stmt), stmt_len) => {
                    current_len += stmt_len;
                    stmts.push(stmt);
                    continue;
                }
                (None, length) => return (None, current_len + length),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_block_parse() {
        use super::Block;
        use super::super::ISyntaxItemWithStr;
        use codepos::StringPosition;
        
        assert_eq!(
            Block::with_test_str_ret_size("{}"), 
            (Some(Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 2) }), 2)
        );

        perrorln!("{:?}", Block::with_test_str("{ 1; 1 + 1; while true { writeln(\"fresky loves zmj\"); } loop { writeln(\"zmj loves fresky\"); } }")); 
    }
}