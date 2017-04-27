
// LoopStatement = [fLabel fColon] fLoop Block

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use super::super::Block;
use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;

#[derive(Eq, PartialEq)]
pub struct LoopStatement {
    m_name: Option<String>,
    m_name_strpos: StringPosition,
    m_loop_strpos: StringPosition,
    m_body: Block,
}
impl ISyntaxItemFormat for LoopStatement {
    fn format(&self, indent: u32) -> String {
        match self.m_name {
            Some(ref loop_name) => format!("{}Loop <{:?}>\n{}Name {} <{:?}>\n{:?}", 
                LoopStatement::indent_str(indent), StringPosition::merge(self.m_name_strpos, self.m_body.pos_all()),
                LoopStatement::indent_str(indent + 1), loop_name, self.m_name_strpos,
                self.m_body),
            None => format!("{}Loop <{:?}>\n{:?}", 
                LoopStatement::indent_str(indent), StringPosition::merge(self.m_loop_strpos, self.m_body.pos_all()),
                self.m_body),
        }
    }
}
impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}
impl LoopStatement { // New
    
    pub fn new(loop_strpos: StringPosition, body: Block) -> LoopStatement {
        LoopStatement {
            m_name: None,
            m_name_strpos: StringPosition::new(),
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }
    pub fn new_by_name(name: String, name_strpos: StringPosition, loop_strpos: StringPosition, body: Block) -> LoopStatement {
        LoopStatement {
            m_name: Some(name), 
            m_name_strpos: name_strpos,
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }
}
impl LoopStatement { // Get

    pub fn has_name(&self) -> bool {
        self.m_name.is_some()
    }

    pub fn get_name(&self) -> Option<&String> {
        match self.m_name {
            Some(ref name) => Some(name),
            None => None
        }
    }
    pub fn get_name_strpos(&self) -> Option<StringPosition> {
        match self.m_name {
            Some(_) => Some(self.m_name_strpos),
            None => None
        }
    }
    pub fn get_loop_strpos(&self) -> StringPosition {
        self.m_loop_strpos
    }
    pub fn get_body(&self) -> &Block {
        &self.m_body
    }

    pub fn get_all_strpos(&self) -> StringPosition {
        match self.m_name {
            Some(_) => StringPosition::merge(self.m_name_strpos, self.m_body.pos_all()),
            None => StringPosition::merge(self.m_loop_strpos, self.m_body.pos_all()),
        }
    }
}

impl ISyntaxItem for LoopStatement {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() }

    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_label() || tokens.nth(index).is_keyword(KeywordKind::Loop)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<LoopStatement>, usize) {

        match tokens.nth(index).get_label() {
            Some(loop_name) => { 
                if !tokens.nth(index + 1).is_seperator(SeperatorKind::Colon) {
                    return push_unexpect!(tokens, messages, "colon", index, 0);
                }
                if !tokens.nth(index + 2).is_keyword(KeywordKind::Loop) {
                    return push_unexpect!(tokens, messages, "keyword loop", index, 0);
                }
                match Block::parse(tokens, messages, index + 3) {
                    (None, length) => (None, 3 + length),
                    (Some(body), body_length) => 
                        (Some(LoopStatement::new_by_name(loop_name, tokens.pos(index), tokens.pos(index + 2), body)), 3 + body_length),
                }
            }
            None => {
                if !tokens.nth(index).is_keyword(KeywordKind::Loop) {
                    return push_unexpect!(tokens, messages, "keyword loop", index, 0);
                }
                match Block::parse(tokens, messages, index + 1) {
                    (None, length) => (None, 1 + length),
                    (Some(body), body_length) => (Some(LoopStatement::new(tokens.pos(index), body)), 1 + body_length),
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn loop_stmt_format() {
    unimplemented!()
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ LoopStatement::with_test_str("loop {}"),
        LoopStatement::new(make_strpos!(1, 1, 1, 4), Block{ pos: make_strpos!(1, 5, 1, 6), stmts: Vec::new() })
    }
}

#[cfg(test)] #[test]
fn ast_stmt_loop_parse() {
    // use super::super::TestCase;
    // use super::super::ISyntaxItemWithStr;

    // //               0        1          2          3
    // //               123456789012345 678901234 56789
    // ast_test_case!{ "loop { writeln(\"love zmj\"); }", 8, make_str_pos!(1, 1, 1, 29),
    //     LoopStatement{
    //         name: None, 
    //         body: Block::with_test_str("     { writeln(\"love zmj\"); }"),
    //         pos: [make_str_pos!(1, 1, 1, 4), StringPosition::new()]
    //     }
    // }            //  12345 678901 2345
    // ast_test_case!{ "loop \"innnn\" {}", 4, make_str_pos!(1, 1, 1, 15),
    //     LoopStatement{
    //         name: Some("innnn".to_owned()),
    //         body: Block::with_test_str("               {}"),
    //         pos: [make_str_pos!(1, 1, 1, 4), make_str_pos!(1, 6, 1, 12)],
    //     }                                              
    // }            //  12345678901
    // ast_test_case!{ "loop abc {}", 4, make_str_pos!(1, 1, 1, 11),
    //     LoopStatement{
    //         name: None,                          
    //         body: Block::with_test_str("         {}"),
    //         pos: [make_str_pos!(1, 1, 1, 4), StringPosition::new()],
    //     },
    //     [
    //         // loop_name_not_str_lit_message!(make_str_pos!(1, 6, 1, 8))
    //     ]
    // }
}