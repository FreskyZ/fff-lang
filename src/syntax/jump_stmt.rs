///! fff-lang
///! 
///! syntax/jump_stmt for break statement and continue statement
///! break_stmt = 'break' [ label ] ';'
///! continue_stmt = 'continue' [ label ] ';'

use super::prelude::*;

impl JumpStatement {

    fn parse(cx: &mut ParseContext, expect_first_kw: Keyword) -> Result<JumpStatement, Unexpected> {

        let starting_span = cx.expect_keyword(expect_first_kw)?;

        if let Some(label) = cx.try_expect_label() {
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement{ all_span: starting_span + semicolon_span, target: Some(label) })
        } else { 
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(JumpStatement{ all_span: starting_span + semicolon_span, target: None })
        }
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

impl Parser for BreakStatement {
    type Output = BreakStatement;
    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Break)) 
    }
    fn parse(cx: &mut ParseContext) -> Result<BreakStatement, Unexpected> {
        Ok(BreakStatement(JumpStatement::parse(cx, Keyword::Break)?))
    }
}

#[cfg(test)] #[test]
fn jump_stmt_parse() {
    
    case!{ "continue;" as ContinueStatement, 
        make_stmt!(continue 0:8)
    }
    case!{ "continue @1;" as ContinueStatement,
        make_stmt!(continue 0:11 label #2 9:10)
    }
    
    case!{ "break;" as BreakStatement,
        make_stmt!(break 0:5)
    }
    case!{ "break @1;" as BreakStatement,
        make_stmt!(break 0:8 label #2 6:7)
    }
}
