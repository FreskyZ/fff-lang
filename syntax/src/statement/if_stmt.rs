
// IfStatement = fIf Expression Block [fElse fIf Expression Block]* [ fElse Block ]

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;
use message::MessageCollection;

use lexical::Lexer;
use lexical::KeywordKind;

use super::super::ast_item::ISyntaxItem;
use super::super::Expression;
use super::super::Block;

#[derive(Eq, PartialEq)]
pub struct ElseIfBranch {
    pub expr: Expression,
    pub body: Block,
    pub pos: [StringPosition; 2],   // position for if and else
}

impl fmt::Debug for ElseIfBranch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "else @ {:?} if @ {:?} {:?} {:?}", self.pos[0], self.pos[1], self.expr, self.body)
    }
}

impl ISyntaxItem for ElseIfBranch {

    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.pos[0], self.body.pos_all()) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::Else)
    }

    /// given index should be index of else and nth(index) = else, nth(index + 1) = if are confirmed
    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<ElseIfBranch>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::Else)
            || !lexer.nth(index + 1).is_keyword(KeywordKind::If) {
            unreachable!()
        }
        let mut current_len = 2;
        let pos = [lexer.pos(index), lexer.pos(index + 1)];

        let expr = match Expression::parse(lexer, messages, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        let body = match Block::parse(lexer, messages, index + current_len) {
            (Some(block), block_len) => { current_len += block_len; block }
            (None, length) => return (None, current_len + length),
        };

        (Some(ElseIfBranch{ expr: expr, body: body, pos: pos }), current_len)
    }
}

#[derive(Eq, PartialEq)]
pub struct IfStatement {
    pub if_expr: Expression,
    pub if_body: Block,
    pub elseifs: Vec<ElseIfBranch>,
    pub else_body: Option<Block>,
    pub pos: [StringPosition; 2],  // position for if and else
}
impl fmt::Debug for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if @ {:?} {:?} {:?}{}{}", 
            self.pos[0], self.if_expr, self.if_body,
            format_vector_debug(&self.elseifs, "\n"), 
            match self.else_body { 
                Some(ref else_body) => format!("else @ {:?} {:?}", self.pos[1], else_body),
                None => String::new(),
            }
        )
    }
}

impl ISyntaxItem for IfStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::new() }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::If)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<IfStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::If) {
            unreachable!()
        }
        let mut current_len = 1;
        let mut pos = [lexer.pos(index), StringPosition::new()];
        
        let expr = match Expression::parse(lexer, messages, index + current_len) {
            (Some(expr), expr_len) => { current_len += expr_len; expr }
            (None, length) => return (None, current_len + length),
        };

        let body = match Block::parse(lexer, messages, index + current_len) {
            (Some(block), block_len) => { current_len += block_len; block }
            (None, length) => return (None, current_len + length),
        };

        let mut elseifs = Vec::new();
        let mut else_body = None;
        loop {
            match (lexer.nth(index + current_len).is_keyword(KeywordKind::Else), lexer.nth(index + current_len + 1).is_keyword(KeywordKind::If)) {
                (true, true) => match ElseIfBranch::parse(lexer, messages, index + current_len) {  // else if
                    (Some(elseif), elseif_len) => { 
                        current_len += elseif_len;
                        elseifs.push(elseif);  
                    }
                    (None, length) => return (None, current_len + length),
                },
                (true, false) => match Block::parse(lexer, messages, index + current_len + 1) { // normal else
                    (Some(block), block_len) => {
                        pos[1] = lexer.pos(index + current_len + 1);
                        else_body = Some(block);
                        current_len += block_len + 1;
                    }  // 16/12/1, we lost TWO `+1`s for current_len here ... fixed
                    (None, length) => return (None, current_len + 1 + length),
                },
                (false, _) => {
                    break;
                }
            }
        }

        return (Some(IfStatement{
            if_expr: expr,
            if_body: body,
            elseifs: elseifs,
            else_body: else_body,
            pos: pos
        }), current_len);
    }
}

#[cfg(test)]
mod tests {
    use super::IfStatement;
    use super::super::super::ast_item::ISyntaxItem;

    #[test]
    fn ast_stmt_if() {

        perrorln!("{:?}", IfStatement::with_test_str("if 1 { fresky.love(zmj); zmj.love(fresky); }"));

        perrorln!("{:?}", IfStatement::with_test_str("if 1 { fresky.love(zmj); zmj.love(fresky); } else { writeln(\"hellworld\"); }"));

        perrorln!("{:?}", IfStatement::with_test_str(
r#"
            if 1 { 
                fresky.love(zmj); 
                zmj.love(fresky); 
            } else if false {
                1 + 1 = 2;
            } else if abc * defg == hij {
                keywords.remove("def");
                use_in_test("def");
            } else { 
                writeln("hellworld"); 
            }"#
        ));
    }
}