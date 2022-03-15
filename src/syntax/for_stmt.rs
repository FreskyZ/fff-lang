///! fff-lang
///!
///! syntax/for_stmt
///! for_stmt = [ label_def ] 'for' identifier 'in' expr block

// TODO: add else for break, like python

use super::prelude::*;
use super::{Expr, Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub for_span: Span,
    pub iter_name: IsId,
    pub iter_span: Span,
    pub iter_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}
impl ForStatement {

    pub fn new_with_label<T: Into<Expr>>(
            all_span: Span, loop_name: LabelDef, for_span: Span, 
            iter_name: impl Into<IsId>, iter_span: Span, iter_expr: T, 
            body: Block) -> ForStatement {
        ForStatement { 
            loop_name: Some(loop_name),
            for_span,
            iter_name: iter_name.into(), iter_span, 
            iter_expr: iter_expr.into(),
            body, all_span
        }
    }

    fn new(loop_name: Option<LabelDef>, for_span: Span,
        iter_name: IsId, iter_span: Span, iter_expr: Expr,
        body: Block) -> ForStatement {
        ForStatement{
            all_span: loop_name.as_ref().map(|n| n.all_span).unwrap_or(for_span) + body.all_span,
            loop_name, for_span, iter_name, iter_expr, iter_span, body,
        }
    }
}

impl Node for ForStatement {
    type ParseOutput = ForStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::For)) | (Token::Keyword(Keyword::For), _))
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<ForStatement> {

        let maybe_label = cx.try_expect_node::<LabelDef>()?;
        let for_span = cx.expect_keyword(Keyword::For)?;
        let (iter_name, iter_span) = cx.expect_ident_or(&[Keyword::Underscore])?; // Accept _ as iter_name, _ do not declare iter var
        let _in_span = cx.expect_keyword(Keyword::In)?;
        let iter_expr = cx.expect_node::<Expr>()?;
        let body = cx.expect_node::<Block>()?;
        return Ok(ForStatement::new(maybe_label, for_span, iter_name, iter_span, iter_expr, body));
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_for_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.loop_name {
            v.visit_label_def(name)?;
        }
        v.visit_expr(&self.iter_expr)?;
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use super::{make_node, make_exprs, make_lit, SimpleName, SimpleExprStatement, Statement, MemberAccessExpr, FnCallExpr, ExprList};

    //                      0123456789012345678
    assert_node_eq!{ make_node!("@2: for i in 42 {}" as ForStatement, [Span::new(1, 1), Span::new(8, 8)]),
        ForStatement::new_with_label(Span::new(0, 17),
            LabelDef::new(2, Span::new(0, 2)),
            Span::new(4, 6),
            3, Span::new(8, 8),
            Expr::Lit(make_lit!(42, 13, 14)),
            Block::new(Span::new(16, 17), vec![])
        )
    }

    //              0         1         2         3         4         5         6         7         
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    assert_node_eq!{ make_node!("@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }" as ForStatement, 
        [], ["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"]),
        ForStatement::new_with_label(Span::new(0, 77),
            LabelDef::new(2, Span::new(0, 6)),
            Span::new(8, 10),
            3, Span::new(12, 12),
            FnCallExpr::new(
                MemberAccessExpr::new(
                    FnCallExpr::new(
                        MemberAccessExpr::new(
                            FnCallExpr::new(
                                SimpleName::new(4, Span::new(17, 21)),
                                Span::new(22, 28), make_exprs![
                                    make_lit!(0, 23, 23),
                                    make_lit!(10, 26, 27),
                                ]
                            ),
                            Span::new(29, 29),
                            SimpleName::new(5, Span::new(30, 38))
                        ),
                        Span::new(39, 40), ExprList::new(vec![])
                    ), 
                    Span::new(41, 41),
                    SimpleName::new(6, Span::new(42, 48))
                ),
                Span::new(49, 50), ExprList::new(vec![])
            ),
            Block::new(Span::new(52, 77), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(54, 75),
                    FnCallExpr::new(
                        SimpleName::new(7, Span::new(54, 60)),
                        Span::new(61, 74), make_exprs![
                            make_lit!(8: str, 62, 73)
                        ]
                    )
                ))
            ])
        )
    }
}
