///! syntax::loop_stmt:
///! loop_stmt = [ label_def ] 'loop' block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use super::prelude::*;
use super::{Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
    pub all_span: Span,
}

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

impl Node for LoopStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_loop_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.name {
            v.visit_label_def(name)?;
        }
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::{Statement, SimpleExprStatement};

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
