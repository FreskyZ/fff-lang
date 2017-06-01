///! fff-lang
///! 
///! syntax/jump_stmt for BreakStatement and ContinueStatement
///! BreakStatement = fBreak [fLabel] fSemiColon        
///! ContinueStatement = fContinue [fLabel] fSemiColon

use std::fmt;

use codemap::StringPosition;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
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

    fn parse(sess: &mut ParseSession, expect_first_kw: KeywordKind) -> ParseResult<JumpStatement> {
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
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Continue) }
}
impl ISyntaxItemGrammar for BreakStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Break) }
}

impl ISyntaxItemParse for ContinueStatement {
    fn parse(sess: &mut ParseSession) -> ParseResult<ContinueStatement> { 
        Ok(ContinueStatement(JumpStatement::parse(sess, KeywordKind::Continue)?))
    }
}
impl ISyntaxItemParse for BreakStatement {
    fn parse(sess: &mut ParseSession) -> ParseResult<BreakStatement> {
        Ok(BreakStatement(JumpStatement::parse(sess, KeywordKind::Break)?))
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