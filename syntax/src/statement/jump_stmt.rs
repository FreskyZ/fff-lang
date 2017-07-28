///! fff-lang
///! 
///! syntax/jump_stmt for break statement and continue statement
///! break_stmt = 'break' [ label ] ';'
///! continue_stmt = 'continue' [ label ] ';'

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::Seperator;
use lexical::Keyword;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

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

    fn format(&self, f: Formatter, stmt_name: &'static str) -> String {
        let f = f.indent().header_text_or(stmt_name).space().span(self.all_span).endl();
        match self.target {
            Some(ref target_name) => f.indent1().lit("to").space().lit("@").sym(*target_name).space().span(self.target_span).finish(),
            None => f.finish(),
        }
    }

    fn parse(sess: &mut ParseSession, expect_first_kw: Keyword) -> ParseResult<JumpStatement> {

        let starting_span = sess.expect_keyword(expect_first_kw)?;

        if let Some((label_id, label_span)) = sess.try_expect_label() {
            let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
            Ok(JumpStatement::new_target(starting_span.merge(&semicolon_span), label_id, label_span))
        } else { 
            let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
            Ok(JumpStatement::new_no_target(starting_span.merge(&semicolon_span)))
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ContinueStatement(pub JumpStatement);
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct BreakStatement(pub JumpStatement);

impl ISyntaxFormat for ContinueStatement {
    fn format(&self, f: Formatter) -> String { self.0.format(f, "continue-stmt") }
}
impl ISyntaxFormat for BreakStatement {
    fn format(&self, f: Formatter) -> String { self.0.format(f, "break-stmt") }
}
impl fmt::Debug for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl fmt::Debug for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
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

impl ISyntaxGrammar for ContinueStatement {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::Continue) }
}
impl ISyntaxGrammar for BreakStatement {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::Break) }
}

impl ISyntaxParse for ContinueStatement {
    type Output = ContinueStatement;
    fn parse(sess: &mut ParseSession) -> ParseResult<ContinueStatement> { 
        Ok(ContinueStatement(JumpStatement::parse(sess, Keyword::Continue)?))
    }
}
impl ISyntaxParse for BreakStatement {
    type Output = BreakStatement;
    fn parse(sess: &mut ParseSession) -> ParseResult<BreakStatement> {
        Ok(BreakStatement(JumpStatement::parse(sess, Keyword::Break)?))
    }
}

#[cfg(test)] #[test]
fn jump_stmt_parse() {
    use super::super::WithTestInput;
    
    assert_eq!{ ContinueStatement::with_test_str("continue;"), ContinueStatement::new_no_target(make_span!(0, 8)) }
    assert_eq!{ ContinueStatement::with_test_str("continue @1;"), 
        ContinueStatement::new_with_target(make_span!(0, 11), make_id!(1), make_span!(9, 10))
    }
    
    assert_eq!{ BreakStatement::with_test_str("break;"), BreakStatement::new_no_target(make_span!(0, 5)) }
    assert_eq!{ BreakStatement::with_test_str("break @1;"), 
        BreakStatement::new_with_target(make_span!(0, 8), make_id!(1), make_span!(6, 7))
    }
}