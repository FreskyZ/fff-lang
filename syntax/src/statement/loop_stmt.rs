
// LoopStatement = fLoop fStringLiteral Block

use std::fmt;

use codepos::StringPosition;
use message::SyntaxMessage;
use message::MessageCollection;

use lexical::Lexer;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::Block;
use super::super::Expression;

#[derive(Eq, PartialEq)]
pub struct LoopStatement {
    pub name: Option<String>,
    pub body: Block,
    pub pos: [StringPosition; 2],   // position for 'loop' and loop name
}

impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "loop @ {:?}{} @ {:?} {:?}", 
            self.pos[0],
            match self.name { Some(ref name) => format!("{}", name), None => format!("\"\""), },
            self.pos[1], 
            self.body
        )
    }
}

impl ISyntaxItem for LoopStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.pos[0], self.body.pos_all()) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::Loop)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<LoopStatement>, usize) {

        if !lexer.nth(index).is_keyword(KeywordKind::Loop) {
            unreachable!()
        }

        let (name, name_pos, current_len) = if Expression::is_first_final(lexer, index + 1) {
            match Expression::parse(lexer, messages, index + 1) {
                (Some(expr), expr_len) => {
                    let expr_pos = expr.pos_all();
                    if expr.is_pure_str_lit() {
                        (expr.into_pure_str_lit(), expr_pos, expr_len + 1)
                    } else {
                        messages.push(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: expr_pos });
                        (None, StringPosition::new(), expr_len + 1)
                    }
                }
                (None, length) => return (None, length + 1),
            }
        } else {
            (None, StringPosition::new(), 1)
        };

        match Block::parse(lexer, messages, index + current_len) {
            (Some(block), block_len) => 
                (Some(LoopStatement{ 
                    name: name, 
                    body: block, 
                    pos: [lexer.pos(index), name_pos] 
                }), current_len + block_len),
            (None, length) => (None, current_len + length),
        }
    }
}

#[cfg(test)] #[test]
fn ast_stmt_loop_parse() {
    use message::LegacyMessage as Message;
    use message::SyntaxMessage;
    use super::super::TestCase;
    use super::super::ISyntaxItemWithStr;

    //               0        1          2          3
    //               123456789012345 678901234 56789
    ast_test_case!{ "loop { writeln(\"love zmj\"); }", 8, make_str_pos!(1, 1, 1, 29),
        LoopStatement{
            name: None, 
            body: Block::with_test_str("     { writeln(\"love zmj\"); }"),
            pos: [make_str_pos!(1, 1, 1, 4), StringPosition::new()]
        }
    }            //  12345 678901 2345
    ast_test_case!{ "loop \"innnn\" {}", 4, make_str_pos!(1, 1, 1, 15),
        LoopStatement{
            name: Some("innnn".to_owned()),
            body: Block::with_test_str("               {}"),
            pos: [make_str_pos!(1, 1, 1, 4), make_str_pos!(1, 6, 1, 12)],
        }                                              
    }            //  12345678901
    ast_test_case!{ "loop abc {}", 4, make_str_pos!(1, 1, 1, 11),
        LoopStatement{
            name: None,                          
            body: Block::with_test_str("         {}"),
            pos: [make_str_pos!(1, 1, 1, 4), StringPosition::new()],
        },
        [
            Message::Syntax(SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ pos: make_str_pos!(1, 6, 1, 8) })
        ]
    }
}