
// Block = fLeftBrace [Statement]* fRightBrace

use std::fmt;

use common::From2;
use common::StringPosition;
use common::format_vector_display;
use common::format_vector_debug;

use lexical::Lexer;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Statement;

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
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\n{}\n}}", format_vector_display(&self.stmts, "\n"))
    }
}

impl Block {

    #[cfg(test)]
    pub fn from_str(program: &str, index: usize) -> Block {
        let lexer = &mut Lexer::new(program);
        Block::parse(lexer, index).0.unwrap()
    }
}

impl IASTItem for Block {
    
    fn pos_all(&self) -> StringPosition { self.pos }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) 
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Block>, usize) {

        if !lexer.nth(index).is_seperator(SeperatorKind::LeftBrace) {
            return lexer.push_expect("left brace", index, 0);
        }

        let mut stmts = Vec::new();
        let mut current_len = 1;
        loop {
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightBrace) {
                return (Some(Block{
                    stmts: stmts,
                    pos: StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + current_len).end_pos)
                }), current_len + 1);
            }
            match Statement::parse(lexer, index + current_len) {
                (Some(mut stmt), stmt_len) => {
                    current_len += stmt_len;
                    stmt.set_id(stmts.len());
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
        use syntax::ast_item::IASTItem;
        use lexical::Lexer;
        use common::StringPosition;
        
        assert_eq!(
            Block::parse(&mut Lexer::new("{}", ), 0), 
            (Some(Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 2) }), 2)
        );

        perrorln!("{}", Block::parse(&mut Lexer::new("{ 1; 1 + 1; while true { writeln(\"fresky loves zmj\"); } loop { writeln(\"zmj loves fresky\"); } }"), 0).0.unwrap()); 
    }
}