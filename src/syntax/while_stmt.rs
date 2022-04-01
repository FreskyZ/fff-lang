///! fff-lang
///!
///! syntax/while_stmt
///! while-stmt = [ label-def ] 'while' expr block
// TODO: add else for break, like python

use super::prelude::*;
use super::{Expr, Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
    pub all_span: Span,
}

impl Parser for WhileStatement {
    type Output = WhileStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }

    fn parse(cx: &mut ParseContext) -> Result<WhileStatement, Unexpected> {
        
        let name = cx.try_expect::<LabelDef>()?;
        let while_span = cx.expect_keyword(Keyword::While)?;
        cx.no_object_literals.push(true);
        let expr = cx.expect::<Expr>()?;
        cx.no_object_literals.pop();
        let body = cx.expect::<Block>()?;
        let all_span = name.as_ref().map(|n| n.all_span).unwrap_or(while_span) + body.all_span;
        Ok(WhileStatement{ name, while_span, loop_expr: expr, body, all_span })
    }
}

impl Node for WhileStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_while_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.name {
            v.visit_label_def(name)?;
        }
        v.visit_expr(&self.loop_expr)?;
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use super::{Statement, SimpleExprStatement};
    //      0        1         2         3         4        
    //      01234567890123456789012345 67890123456789012 3456
    case!{ "@2: while true { writeln(\"fresky hellooooo\"); }" as WhileStatement,
        make_stmt!(while 0:46 label #2 0:2 while 4:8
            make_expr!(true 10:13),
            Block::new(Span::new(15, 46), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(17, 44),
                    make_expr!(fn 17:43 paren 24:43
                        make_name!(simple 17:23 #3),
                        make_expr!(str #4 25:42))
                ))
            ])
        ), strings ["2", "writeln", "fresky hellooooo"]
    }
}
