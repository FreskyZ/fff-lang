
// ForStatement = fFor fIdentifier fIn Expression fRange Expression Block 

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::Block;

#[derive(Eq, PartialEq)]
pub struct ForStatement {
    pub id: usize,
    pub iter_name: String, 
    pub expr_low: Expression,
    pub expr_high: Expression,
    pub body: Block,
    pub pos: [StringPosition; 4]  // position for 'for', iter_name, 'in', range operator, 
}

impl fmt::Debug for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}> for @ {:?} {} @ {:?} in @ {:?} {:?}.. @ {:?} {:?} {:?}", 
            self.id, self.pos[0], self.iter_name, self.pos[1], self.pos[2], self.expr_low, self.pos[3], self.expr_high, self.body    
        )
    }
}
impl fmt::Display for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}> for {} in {}..{} {}", 
            self.id, self.iter_name, self.expr_low, self.expr_high, self.body    
        )
    }
}

impl IASTItem for ForStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.body.pos_all().end_pos) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::For)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<ForStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::For) {
            unreachable!()
        }
        let mut current_len = 1;
        let mut pos = [StringPosition::new(); 4];
        pos[0] = lexer.pos(index);

        let iter_name = match lexer.nth(index + current_len).get_identifier() {
            Some(ident) => ident.clone(),
            None => return lexer.push_expect("identifier", index + current_len, current_len),
        };
        pos[1] = lexer.pos(index + current_len);
        current_len += 1;

        if !lexer.nth(index + current_len).is_keyword(KeywordKind::In) {
            return lexer.push_expect("keyword in", index + current_len, current_len);
        }
        pos[2] = lexer.pos(index + current_len);
        current_len += 1;

        let left_expr = match Expression::parse(lexer, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        if !lexer.nth(index + current_len).is_seperator(SeperatorKind::Range) {
            return lexer.push_expect("range operator", index + current_len, current_len);
        }
        pos[3] = lexer.pos(index + current_len);
        current_len += 1;

        let right_expr = match Expression::parse(lexer, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        let body = match Block::parse(lexer, index + current_len) {
            (Some(block), block_len) => { current_len += block_len; block }
            (None, length) => return (None, current_len + length),
        };

        return (Some(ForStatement{
            id: 0, 
            iter_name: iter_name, 
            expr_low: left_expr, 
            expr_high: right_expr, 
            body: body, 
            pos: pos
        }), current_len);
    }
}

#[cfg(test)]
mod tests {
    use super::ForStatement;
    use syntax::ast_item::IASTItem;
    use lexical::Lexer;

    #[test]
    fn ast_stmt_for() {

        perrorln!("{}", ForStatement::parse(&mut Lexer::new("for i in 1 + 1[2]..infinite(true) { fresky.loves(zmj); }"), 0).0.unwrap());
    }
}