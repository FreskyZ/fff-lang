///! fff-lang
///! 
///! syntax/jump_stmt for break statement and continue statement
///! break_stmt = 'break' [ label ] ';'
///! continue_stmt = 'continue' [ label ] ';'

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct JumpStatement {
    pub target: Option<IsId>,
    pub target_span: Span,
    pub all_span: Span,
}
impl JumpStatement {

    fn new_no_target(all_span: Span) -> JumpStatement {
        JumpStatement{ all_span, target: None, target_span: Span::new(0, 0) }
    }
    fn new_target(all_span: Span, target: impl Into<IsId>, target_span: Span) -> JumpStatement {
        JumpStatement{ all_span, target_span, target: Some(target.into()) }
    }

    fn format(&self, f: Formatter, stmt_name: &'static str) -> String {
        let f = f.indent().header_text_or(stmt_name).space().span(self.all_span).endl();
        match self.target {
            Some(ref target_name) => f.indent1().lit("to").space().lit("@").isid(*target_name).space().span(self.target_span).finish(),
            None => f.finish(),
        }
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>, expect_first_kw: Keyword) -> ParseResult<JumpStatement> {

        let starting_span = sess.expect_keyword(expect_first_kw)?;

        if let Some((label_id, label_span)) = sess.try_expect_label() {
            let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement::new_target(starting_span + semicolon_span, label_id, label_span))
        } else { 
            let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement::new_no_target(starting_span + semicolon_span))
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ContinueStatement(pub JumpStatement);
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct BreakStatement(pub JumpStatement);

impl ISyntaxFormat for ContinueStatement {
    fn format(&self, f: Formatter) -> String { self.0.format(f, "continue-stmt") }
}
impl ISyntaxFormat for BreakStatement {
    fn format(&self, f: Formatter) -> String { self.0.format(f, "break-stmt") }
}

impl ContinueStatement {

    pub fn new_no_target(all_span: Span) -> ContinueStatement { ContinueStatement(JumpStatement::new_no_target(all_span)) }
    pub fn new_with_target(all_span: Span, target: impl Into<IsId>, target_span: Span) -> ContinueStatement {
        ContinueStatement(JumpStatement::new_target(all_span, target.into(), target_span))
    }
}
impl BreakStatement {

    pub fn new_no_target(all_span: Span) -> BreakStatement { BreakStatement(JumpStatement::new_no_target(all_span)) }
    pub fn new_with_target(all_span: Span, target: impl Into<IsId>, target_span: Span) -> BreakStatement {
        BreakStatement(JumpStatement::new_target(all_span, target.into(), target_span))
    }
}

impl Node for ContinueStatement {
    type ParseOutput = ContinueStatement;
    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Continue)) 
    }
    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<ContinueStatement> { 
        Ok(ContinueStatement(JumpStatement::parse(sess, Keyword::Continue)?))
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_continue_stmt(self)
    }
}
impl Node for BreakStatement {
    type ParseOutput = BreakStatement;
    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Break)) 
    }
    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<BreakStatement> {
        Ok(BreakStatement(JumpStatement::parse(sess, Keyword::Break)?))
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_break_stmt(self)
    }
}

#[cfg(test)] #[test]
fn jump_stmt_parse() {
    use super::make_node;
    
    assert_eq!{ make_node!("continue;" as ContinueStatement), ContinueStatement::new_no_target(Span::new(0, 8)) }
    assert_eq!{ make_node!("continue @1;" as ContinueStatement), 
        ContinueStatement::new_with_target(Span::new(0, 11), 1, Span::new(9, 10))
    }
    
    assert_eq!{ make_node!("break;" as BreakStatement), BreakStatement::new_no_target(Span::new(0, 5)) }
    assert_eq!{ make_node!("break @1;" as BreakStatement), 
        BreakStatement::new_with_target(Span::new(0, 8), 1, Span::new(6, 7))
    }
}
