///! syntax::loop_stmt:
///! loop_stmt = [ label_def ] 'loop' block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use super::prelude::*;

impl Parser for LoopStatement {
    type Output = LoopStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    fn parse(cx: &mut ParseContext) -> Result<LoopStatement, Unexpected> {

        let name = cx.try_expect::<LabelDef>()?;
        let loop_span = cx.expect_keyword(Keyword::Loop)?;
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(loop_span) + body.all_span;
        Ok(LoopStatement{ all_span, name, loop_span, body })
    }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::{ast::Statement, ast::SimpleExprStatement};

    case!{ "loop {}" as LoopStatement,
        make_stmt!(loop 0:6 loop 0:3
            Block::new(Span::new(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    case!{ "@@: loop { println(\"233\"); }" as LoopStatement,
        make_stmt!(loop 0:27 label #2 0:2 loop 4:7
            Block::new(Span::new(9, 27), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(11, 25), 
                    make_expr!(fn 11:24 paren 18:24
                        make_name!(simple 11:17 #3),
                        make_expr!(str #4 19:23))
                ))
            ])
        ), strings ["@", "println", "233"]
    }
}
