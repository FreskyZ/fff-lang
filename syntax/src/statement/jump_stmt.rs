///! fff-lang
///! 
///! syntax/jump_stmt for BreakStatement and ContinueStatement
///! BreakStatement = fBreak [fLabel] fSemiColon        
///! ContinueStatement = fContinue [fLabel] fSemiColon

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct JumpStatement {
    pub target: Option<SymbolID>,
    pub target_span: Span,
    pub all_span: Span,
}
impl JumpStatement {

    fn new_no_target(all_span: Span) -> JumpStatement {
        JumpStatement{ all_span, target: None, target_span: Span::default() }
    }
    fn new_target(all_span: Span, target: SymbolID, target_span: Span) -> JumpStatement {
        JumpStatement{ all_span, target_span, target: Some(target) }
    }

    fn format(&self, indent: u32, stmt_name: &str) -> String {
        match self.target {
            Some(ref target_name) => format!("{}{} <{:?}>\n{}To @{:?} <{:?}>", 
                ContinueStatement::indent_str(indent), stmt_name, self.all_span,
                ContinueStatement::indent_str(indent + 1), target_name, self.target_span
            ),
            None => format!("{}{} <{:?}>",
                ContinueStatement::indent_str(indent), stmt_name, self.all_span
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
                Ok(JumpStatement::new_target(starting_strpos.merge(semi_colon_strpos), *target, target_strpos)),
            (&Token::Sep(SeperatorKind::SemiColon), ref semi_colon_strpos, _, _) => 
                Ok(JumpStatement::new_no_target(starting_strpos.merge(semi_colon_strpos))),
            (&Token::Label(_), _, _, _) =>
                sess.push_unexpect("semicolon"),
            _ => 
                sess.push_unexpect("label, semicolo"),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ContinueStatement(pub JumpStatement);
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BreakStatement(pub JumpStatement);

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

    pub fn new_no_target(all_span: Span) -> ContinueStatement { ContinueStatement(JumpStatement::new_no_target(all_span)) }
    pub fn new_with_target(all_span: Span, target: SymbolID, target_span: Span) -> ContinueStatement {
        ContinueStatement(JumpStatement::new_target(all_span, target, target_span))
    }
}
impl BreakStatement {

    pub fn new_no_target(all_span: Span) -> BreakStatement { BreakStatement(JumpStatement::new_no_target(all_span)) }
    pub fn new_with_target(all_span: Span, target: SymbolID, target_span: Span) -> BreakStatement {
        BreakStatement(JumpStatement::new_target(all_span, target, target_span))
    }
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
    
    assert_eq!{ ContinueStatement::with_test_str("continue;"), ContinueStatement::new_no_target(make_span!(0, 8)) }
    assert_eq!{ ContinueStatement::with_test_str("continue @1;"), 
        ContinueStatement::new_with_target(make_span!(0, 11), make_id!(1), make_span!(9, 10))
    }
    
    assert_eq!{ BreakStatement::with_test_str("break;"), BreakStatement::new_no_target(make_span!(0, 5)) }
    assert_eq!{ BreakStatement::with_test_str("break @1;"), 
        BreakStatement::new_with_target(make_span!(0, 8), make_id!(1), make_span!(6, 7))
    }
}