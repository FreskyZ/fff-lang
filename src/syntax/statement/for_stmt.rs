///! fff-lang
///!
///! syntax/for_stmt
///! for_stmt = [ label_def ] 'for' identifier 'in' expr block

// TODO: add else for break, like python

use crate::syntax::prelude::*;
use super::super::{Expr, Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub for_span: Span,
    pub iter_name: IsId,
    pub iter_span: Span,
    pub iter_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}
impl ISyntaxFormat for ForStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("for-stmt").space().span(self.all_span).endl();
        let f = match self.loop_name { 
            Some(ref name) => f.set_header_text("loop-name").apply1(name).unset_header_text().endl(), 
            None => f.indent1().lit("no-loop-name").endl(),
        };
        f.indent1().lit("\"for\"").space().span(self.for_span).endl()
            .indent1().lit("iter-var").space().isid(self.iter_name).space().span(self.iter_span).endl()
            .set_prefix_text("iter-expr-is").apply1(&self.iter_expr).unset_prefix_text().endl()
            .set_header_text("body").apply1(&self.body)
            .finish()
    }
}
impl fmt::Debug for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl ForStatement {

    pub fn new_no_label<T: Into<Expr>>(
            all_span: Span, for_span: Span, 
            iter_name: IsId, iter_span: Span, iter_expr: T, 
            body: Block) -> ForStatement {
        ForStatement { 
            loop_name: None,
            for_span,
            iter_name, iter_span, 
            iter_expr: iter_expr.into(),
            body, all_span
        }
    }
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

impl<'ecx, 'scx, F> Node<'ecx, 'scx, F> for ForStatement where F: FileSystem {
    type ParseOutput = ForStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::For)) | (Token::Keyword(Keyword::For), _))
    }

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<ForStatement> {

        let maybe_label = LabelDef::try_parse(sess)?;
        let for_span = sess.expect_keyword(Keyword::For)?;
        let (iter_name, iter_span) = sess.expect_ident_or(&[Keyword::Underscore])?; // Accept _ as iter_name, _ do not declare iter var
        let _in_span = sess.expect_keyword(Keyword::In)?;
        let iter_expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(ForStatement::new(maybe_label, for_span, iter_name, iter_span, iter_expr, body));
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use super::super::{make_node, SimpleName, LitExpr, LitValue, SimpleExprStatement, Statement, MemberAccessExpr, FnCallExpr, ExprList};

    //                      0123456789012345678
    assert_eq!{ make_node!("@2: for i in 42 {}" as ForStatement, [Span::new(1, 1), Span::new(8, 8)]),
        ForStatement::new_with_label(Span::new(0, 17),
            LabelDef::new(1, Span::new(0, 2)),
            Span::new(4, 6),
            2, Span::new(8, 8),
            Expr::Lit(LitExpr::new(LitValue::from(42i32), Span::new(13, 14))),
            Block::new(Span::new(16, 17), vec![])
        )
    }

    //              0         1         2         3         4         5         6         7         
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    assert_eq!{ make_node!("@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }" as ForStatement, 
        [], ["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"]),
        ForStatement::new_with_label(Span::new(0, 77),
            LabelDef::new(1, Span::new(0, 6)),
            Span::new(8, 10),
            2, Span::new(12, 12),
            FnCallExpr::new(
                MemberAccessExpr::new(
                    FnCallExpr::new(
                        MemberAccessExpr::new(
                            FnCallExpr::new(
                                SimpleName::new(3, Span::new(17, 21)),
                                Span::new(22, 28), make_exprs![
                                    LitExpr::new(LitValue::from(0i32), Span::new(23, 23)),
                                    LitExpr::new(LitValue::from(10i32), Span::new(26, 27)),
                                ]
                            ),
                            Span::new(29, 29),
                            SimpleName::new(4, Span::new(30, 38))
                        ),
                        Span::new(39, 40), ExprList::new(vec![])
                    ), 
                    Span::new(41, 41),
                    SimpleName::new(5, Span::new(42, 48))
                ),
                Span::new(49, 50), ExprList::new(vec![])
            ),
            Block::new(Span::new(52, 77), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(54, 75),
                    FnCallExpr::new(
                        SimpleName::new(6, Span::new(54, 60)),
                        Span::new(61, 74), make_exprs![
                            LitExpr::new(7u32, Span::new(62, 73))
                        ]
                    )
                ))
            ])
        )
    }
}
