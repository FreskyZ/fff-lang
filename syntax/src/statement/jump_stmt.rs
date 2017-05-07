///! fff-lang
///! 
///! syntax/jump_stmt for BreakStatement and ContinueStatement
///! BreakStatement = fBreak [fLabel] fSemiColon        
///! ContinueStatement = fContinue [fLabel] fSemiColon

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
struct JumpStatement {
    m_target: Option<String>,
    m_target_strpos: StringPosition,
    m_all_strpos: StringPosition,
}
impl JumpStatement {

    fn new_no_target(all_strpos: StringPosition) -> JumpStatement {
        JumpStatement{ m_target: None, m_target_strpos: StringPosition::new(), m_all_strpos: all_strpos }
    }
    fn new_target(all_strpos: StringPosition, target: String, target_strpos: StringPosition) -> JumpStatement {
        JumpStatement{ m_target: Some(target), m_target_strpos: target_strpos, m_all_strpos: all_strpos }
    }

    fn format(&self, indent: u32, stmt_name: &str) -> String {
        match self.m_target {
            Some(ref target_name) => format!("{}{} <{:?}>\n{}To '@{}' <{:?}>", 
                ContinueStatement::indent_str(indent), stmt_name, self.m_all_strpos,
                ContinueStatement::indent_str(indent + 1), target_name, self.m_target_strpos
            ),
            None => format!("{}{} <{:?}>",
                ContinueStatement::indent_str(indent), stmt_name, self.m_all_strpos
            )
        }
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize, expect_first_kw: KeywordKind) -> (Option<JumpStatement>, usize) {
        assert!(tokens.nth(index) == &Token::Keyword(expect_first_kw));

        match (tokens.nth(index + 1), tokens.nth(index + 2)) {
            (&Token::Label(ref target), &Token::Sep(SeperatorKind::SemiColon)) => 
                (Some(JumpStatement::new_target(StringPosition::merge(tokens.pos(index), tokens.pos(index + 2)), target.clone(), tokens.pos(index + 1))), 3),
            (&Token::Label(_), _) => 
                push_unexpect!(tokens, messages, "semicolon", index + 2, 2),
            (&Token::Sep(SeperatorKind::SemiColon), _) => 
                (Some(JumpStatement::new_no_target(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2),
            _ =>
                push_unexpect!(tokens, messages, "label or semicolon", index + 1, 1),
        }
    }

    #[cfg(feature = "parse_sess")]
    fn parsex(sess: &mut ParseSession, expect_first_kw: KeywordKind) -> ParseResult<JumpStatement> {
        assert!(sess.tk == &Token::Keyword(expect_first_kw));

        let starting_strpos = sess.pos;
        sess.move_next();
        match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
            (&Token::Label(ref target), target_strpos, 
                &Token::Sep(SeperatorKind::SemiColon), ref semi_colon_strpos) => 
                Ok(JumpStatement::new_target(StringPosition::merge(starting_strpos, *semi_colon_strpos), target.clone(), target_strpos)),
            (&Token::Sep(SeperatorKind::SemiColon), ref semi_colon_strpos, _, _) => 
                Ok(JumpStatement::new_no_target(StringPosition::merge(starting_strpos, *semi_colon_strpos))),
            (&Token::Label(_), _, _, _) =>
                sess.push_unexpect("semicolon"),
            _ => 
                sess.push_unexpect("label, semicolo"),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ContinueStatement(JumpStatement);
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BreakStatement(JumpStatement);

impl ISyntaxItemFormat for ContinueStatement {
    fn format(&self, indent: u32) -> String { self.0.format(indent, "ContinueStmt") }
}
impl ISyntaxItemFormat for BreakStatement {
    fn format(&self, indent: u32) -> String { self.0.format(indent, "BreakStmt") }
}
impl fmt::Debug for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl fmt::Debug for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}

impl ContinueStatement {

    pub fn new_no_target(all_strpos: StringPosition) -> ContinueStatement { ContinueStatement(JumpStatement::new_no_target(all_strpos)) }
    pub fn new_with_target(all_strpos: StringPosition, target: String, target_strpos: StringPosition) -> ContinueStatement {
        ContinueStatement(JumpStatement::new_target(all_strpos, target, target_strpos))
    }

    pub fn has_target(&self) -> bool { match self.0.m_target { Some(_) => true, None => false } }
    pub fn get_all_strpos(&self) -> StringPosition { self.0.m_all_strpos }
    pub fn get_target(&self) -> Option<&String> { match self.0.m_target { Some(ref target) => Some(target), None => None } }
    pub fn get_target_strpos(&self) -> Option<StringPosition> { match self.0.m_target { Some(_) => Some(self.0.m_target_strpos), None => None } }
}
impl BreakStatement {

    pub fn new_no_target(all_strpos: StringPosition) -> BreakStatement { BreakStatement(JumpStatement::new_no_target(all_strpos)) }
    pub fn new_with_target(all_strpos: StringPosition, target: String, target_strpos: StringPosition) -> BreakStatement {
        BreakStatement(JumpStatement::new_target(all_strpos, target, target_strpos))
    }

    pub fn has_target(&self) -> bool { match self.0.m_target { Some(_) => true, None => false } }
    pub fn get_all_strpos(&self) -> StringPosition { self.0.m_all_strpos }
    pub fn get_target(&self) -> Option<&String> { match self.0.m_target { Some(ref target) => Some(target), None => None } }
    pub fn get_target_strpos(&self) -> Option<StringPosition> { match self.0.m_target { Some(_) => Some(self.0.m_target_strpos), None => None } }
}

impl ISyntaxItemGrammar for ContinueStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { tokens.nth(index) == &Token::Keyword(KeywordKind::Continue) }
}
impl ISyntaxItemGrammar for BreakStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { tokens.nth(index) == &Token::Keyword(KeywordKind::Break) }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for ContinueStatement {
    fn is_first_finalx(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Continue) }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for BreakStatement {
    fn is_first_finalx(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Break) }
}

impl ISyntaxItemParse for ContinueStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ContinueStatement>, usize) {
        
        match JumpStatement::parse(tokens, messages, index, KeywordKind::Continue) {
            (None, length) => (None, length),
            (Some(jump_stmt), length) => (Some(ContinueStatement(jump_stmt)), length),
        }
    }
}
impl ISyntaxItemParse for BreakStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BreakStatement>, usize) {
        
        match JumpStatement::parse(tokens, messages, index, KeywordKind::Break) {
            (None, length) => (None, length),
            (Some(jump_stmt), length) => (Some(BreakStatement(jump_stmt)), length),
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for ContinueStatement {

    fn parsex(sess: &mut ParseSession) -> ParseResult<ContinueStatement> { 
        Ok(ContinueStatement(JumpStatement::parsex(sess, KeywordKind::Continue)?))
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for BreakStatement {

    fn parsex(sess: &mut ParseSession) -> ParseResult<BreakStatement> {
        Ok(BreakStatement(JumpStatement::parsex(sess, KeywordKind::Break)?))
    }
}

#[cfg(test)] #[test]
fn jump_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    
    assert_eq!{ ContinueStatement::with_test_str("continue;"), ContinueStatement::new_no_target(make_strpos!(1, 1, 1, 9)) }
    assert_eq!{ ContinueStatement::with_test_str("continue @1;"), 
        ContinueStatement::new_with_target(make_strpos!(1, 1, 1, 12), "1".to_owned(), make_strpos!(1, 10, 1, 11))
    }
    
    assert_eq!{ BreakStatement::with_test_str("break;"), BreakStatement::new_no_target(make_strpos!(1, 1, 1, 6)) }
    assert_eq!{ BreakStatement::with_test_str("break @1;"), 
        BreakStatement::new_with_target(make_strpos!(1, 1, 1, 9), "1".to_owned(), make_strpos!(1, 7, 1, 8))
    }
}