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

    fn parse(cx: &mut ParseContext, expect_first_kw: Keyword) -> Result<JumpStatement, Unexpected> {

        let starting_span = cx.expect_keyword(expect_first_kw)?;

        if let Some((label_id, label_span)) = cx.try_expect_label() {
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement::new_target(starting_span + semicolon_span, label_id, label_span))
        } else { 
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
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

impl Parser for ContinueStatement {
    type Output = ContinueStatement;
    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Continue)) 
    }
    fn parse(cx: &mut ParseContext) -> Result<ContinueStatement, Unexpected> { 
        Ok(ContinueStatement(JumpStatement::parse(cx, Keyword::Continue)?))
    }
}

impl Node for ContinueStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_continue_stmt(self)
    }
}
impl Parser for BreakStatement {
    type Output = BreakStatement;
    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Break)) 
    }
    fn parse(cx: &mut ParseContext) -> Result<BreakStatement, Unexpected> {
        Ok(BreakStatement(JumpStatement::parse(cx, Keyword::Break)?))
    }
}

impl Node for BreakStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_break_stmt(self)
    }
}

#[cfg(test)] #[test]
fn jump_stmt_parse() {
    
    case!{ "continue;" as ContinueStatement, ContinueStatement::new_no_target(Span::new(0, 8)) }
    case!{ "continue @1;" as ContinueStatement, 
        ContinueStatement::new_with_target(Span::new(0, 11), 2, Span::new(9, 10))
    }
    
    case!{ "break;" as BreakStatement, BreakStatement::new_no_target(Span::new(0, 5)) }
    case!{ "break @1;" as BreakStatement, 
        BreakStatement::new_with_target(Span::new(0, 8), 2, Span::new(6, 7))
    }
}
