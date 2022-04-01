///! fff-lang
///!
///! syntax/ret_stmt
///! ret_stmt = 'return' [ expr ] ';'

use super::prelude::*;

impl Parser for ReturnStatement {
    type Output = ReturnStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Return))
    }

    fn parse(cx: &mut ParseContext) -> Result<ReturnStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Return)?;

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_cx and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span + semicolon_span))
        } else {
            let expr = cx.expect::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span + semicolon_span, expr))
        }
    }
}


#[cfg(test)] #[test]
fn ret_stmt_parse() {
    case!{ "return;" as ReturnStatement, ReturnStatement::new_unit(Span::new(0, 6)) }
    case!{ "return 1 + 1;" as ReturnStatement, 
        ReturnStatement::new_expr(
            Span::new(0, 12),
            make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11))
        )
    }
}
