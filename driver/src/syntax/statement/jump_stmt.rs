
// JumpStatement = ReturnStatement | BreakStatement | ContinueStatement
// ReturnStatemt = fReturn [Expression] fSemiColon
// BreakStatement = fBreak [fStringLiteral] fSemiColon        
// ContinueStatement = fContinue [fStringLiteral] fSemiColon

// Recoverable:
// fBreak|fContinue OtherExpression fSemiColon => loop name specifier must be string literal

use std::fmt;

use codemap::StringPosition;
use message::SyntaxMessage;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;

#[derive(Eq, PartialEq)]
pub struct ReturnStatement {
    pub expr: Option<Expression>,
    pub pos: [StringPosition; 2], // position for return and semicolon
}

impl fmt::Debug for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return @ {:?}{}; @ {:?}", 
            self.pos[0], 
            match self.expr { Some(ref expr) => format!(" {:?} ", expr), None => String::new() }, 
            self.pos[1]
        )
    }
}
impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return{};",
            match self.expr { Some(ref expr) => format!(" {} ", expr), None => String::new() }, 
        )
    }
}
impl ReturnStatement {
    pub fn pub_pos_all(&self) -> StringPosition { self.pos_all() }
}
impl IASTItem for ReturnStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.pos[1].end_pos) } 

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::Return)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<ReturnStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::Return) {
            unreachable!()
        }

        if lexer.nth(index + 1).is_seperator(SeperatorKind::SemiColon) {
            return (Some(ReturnStatement{
                expr: None,
                pos: [lexer.pos(index), lexer.pos(index + 1)],
            }), 2);
        }

        match Expression::parse(lexer, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::SemiColon) {
                    return (Some(ReturnStatement{
                        expr: Some(expr),
                        pos: [lexer.pos(index), lexer.pos(index + 1 + expr_len)],
                    }), 2 + expr_len);
                } else {
                    return lexer.push_expect("semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct ContinueStatement {
    pub name: Option<String>,
    pub pos: [StringPosition; 3], // position for continue, name and semicolon
}
#[derive(Eq, PartialEq)]
pub struct BreakStatement {
    pub name: Option<String>,
    pub pos: [StringPosition; 3], // position for break, name and semicolon
}

impl fmt::Debug for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue @ {:?}{}; @ {:?}", 
            self.pos[0],
            match self.name { Some(ref name) => format!(" {} @ {:?}", name, self.pos[1]), None => format!(" \"\" @ {:?}", self.pos[1]), },
            self.pos[2]
        )
    }
}
impl fmt::Display for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue{};",
            match self.name { Some(ref name) => format!(" {}", name), None => String::new(), }
        )
    }
}
impl fmt::Debug for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break @ {:?}{}; @ {:?}", 
            self.pos[0],
            match self.name { Some(ref name) => format!(" {} @ {:?}", name, self.pos[1]), None => format!(" \"\" @ {:?}", self.pos[1]), },
            self.pos[2]
        )
    }
}
impl fmt::Display for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break{};",
            match self.name { Some(ref name) => format!(" {}", name), None => String::new(), }
        )
    }
}

impl IASTItem for ContinueStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.pos[2].end_pos) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::Continue)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<ContinueStatement>, usize) {
        
        if !lexer.nth(index).is_keyword(KeywordKind::Continue) {
            unreachable!()
        }

        if lexer.nth(index + 1).is_seperator(SeperatorKind::SemiColon) {
            return (Some(ContinueStatement{
                name: None,
                pos: [lexer.pos(index), StringPosition::new(), lexer.pos(index + 1)],
            }), 2);
        }

        match Expression::parse(lexer, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::SemiColon) {
                    let mut name_pos = expr.pos_all();
                    if !expr.is_pure_str_lit() {
                        lexer.push(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: expr.pos_all() });
                        name_pos = StringPosition::new();
                    }
                    let name = expr.into_pure_str_lit();
                    return (Some(ContinueStatement{
                        name: name,
                        pos: [lexer.pos(index), name_pos, lexer.pos(index + 1 + expr_len)],
                    }), 2 + expr_len);
                } else {
                    return lexer.push_expect("semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}
impl IASTItem for BreakStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.pos[2].end_pos) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::Break)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<BreakStatement>, usize) {
        
        if !lexer.nth(index).is_keyword(KeywordKind::Break) {
            unreachable!()
        }

        if lexer.nth(index + 1).is_seperator(SeperatorKind::SemiColon) {
            return (Some(BreakStatement{
                name: None,
                pos: [lexer.pos(index), StringPosition::new(), lexer.pos(index + 1)],
            }), 2);
        }

        match Expression::parse(lexer, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if lexer.nth(index + 1 + expr_len).is_seperator(SeperatorKind::SemiColon) {
                    let mut name_pos = expr.pos_all();
                    if !expr.is_pure_str_lit() {
                        lexer.push(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: expr.pos_all() });
                        name_pos = StringPosition::new();
                    }

                    let name = expr.into_pure_str_lit();
                    return (Some(BreakStatement{
                        name: name,
                        pos: [lexer.pos(index), name_pos, lexer.pos(index + 1 + expr_len)],
                    }), 2 + expr_len);
                } else {
                    return lexer.push_expect("semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ReturnStatement;
    use super::BreakStatement;
    use super::ContinueStatement;
    use codemap::StringPosition;
    use message::SyntaxMessage;
    use message::Message;
    use syntax::Expression;
    use syntax::ast_item::TestCase;

    #[test]
    fn ast_stmt_jump_parse() {

        // Return statement
        //               1234567
        ast_test_case!{ "return;", 2, make_str_pos!(1, 1, 1, 7),
            ReturnStatement{ 
                expr: None, 
                pos: [make_str_pos!(1, 1, 1, 6), make_str_pos!(1, 7, 1, 7)] 
            }
        }            //  1234567890123
        ast_test_case!{ "return 1 + 1;", 5, make_str_pos!(1, 1, 1, 13),
            ReturnStatement{ 
                expr: Some(Expression::from_str("return 1 + 1", 1)),
                pos: [make_str_pos!(1, 1, 1, 6), make_str_pos!(1, 13, 1, 13)] 
            }
        }
        
        // Continue statement
        //               1234567890
        ast_test_case!{ "continue;", 2, make_str_pos!(1, 1, 1, 9),
            ContinueStatement{ 
                name: None, 
                pos: [make_str_pos!(1, 1, 1, 8), StringPosition::new(), make_str_pos!(1, 9, 1, 9)] 
            }
        }            //  123456789 012345 67
        ast_test_case!{ "continue \"inner\";", 3, make_str_pos!(1, 1, 1, 17),
            ContinueStatement{ 
                name: Some("inner".to_owned()), 
                pos: [make_str_pos!(1, 1, 1, 8), make_str_pos!(1, 10, 1, 16), make_str_pos!(1, 17, 1, 17)] 
            }
        }            //  1234567890123
        ast_test_case!{ "continue 123;", 3, make_str_pos!(1, 1, 1, 13),
            ContinueStatement{
                name: None, 
                pos: [make_str_pos!(1, 1, 1, 8), StringPosition::new(), make_str_pos!(1, 13, 1, 13)]
            },
            [
                Message::Syntax(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: make_str_pos!(1, 10, 1, 12) })
            ]
        }

        // Break statement
        ast_test_case!{ "break   ;", 2, make_str_pos!(1, 1, 1, 9),
            BreakStatement{ 
                name: None, 
                pos: [make_str_pos!(1, 1, 1, 5), StringPosition::new(), make_str_pos!(1, 9, 1, 9)] 
            }
        }            // 123456 7890123 45
        ast_test_case!{ "break \"outter\";", 3, make_str_pos!(1, 1, 1, 15),
            BreakStatement{ 
                name: Some("outter".to_owned()), 
                pos: [make_str_pos!(1, 1, 1, 5), make_str_pos!(1, 7, 1, 14), make_str_pos!(1, 15, 1, 15)] 
            }
        }            //  1234567890
        ast_test_case!{ "break 123;", 3, make_str_pos!(1, 1, 1, 10),
            BreakStatement{
                name: None,
                pos: [make_str_pos!(1, 1, 1, 5), StringPosition::new(), make_str_pos!(1, 10, 1, 10)],
            },
            [
                Message::Syntax(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: make_str_pos!(1, 7, 1, 9) })
            ]
        }
    }
}