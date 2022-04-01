////! fff-lang
///!
///! syntax/use_stmt
///! use_stmt = 'use' name [ 'as' identifier ] ';'

use super::prelude::*;

impl Parser for UseStatement {
    type Output = UseStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Use)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<UseStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Use)?;
        let name = cx.expect::<Name>()?;

        let alias = cx.try_expect_keyword(Keyword::As).map(|_| cx.expect_ident()).transpose()?;
        let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(UseStatement{ all_span, name, alias })
    }
}

impl Node for UseStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_use_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_name(&self.name)?;
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn use_stmt_parse() {

    case!{ "use a;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 5), alias: None, 
            name: make_name!(simple bare 4:4 #2) },
    }
    //                   0123456789012345678901234567890
    case!{ "use std::fmt::Debug as Display;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 30), alias: Some((IsId::new(5), Span::new(23, 29))),
            name: make_name!(bare 4:18 false, None,
                make_name!(segment 4:6 #2),
                make_name!(segment 9:11 #3),
                make_name!(segment 14:18 #4),
            ) },
    }
}
