///! fff-lang
///!
///! syntax/if_stmt
///! if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]

use super::prelude::*;
use super::{Expr, Block};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct IfClause {
    pub cond_expr: Expr,
    pub body: Block,
    pub all_span: Span, // if_span = all_span.slice(0..1)
}
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ElseIfClause {
    pub elseif_span: Span, // else_span = elseif_span.slice(0..3), if_span = elseif_span.slice(-2..0)  // TODO: that slice(-2..0)
    pub cond_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ElseClause {
    pub body: Block,
    pub all_span: Span, // else_span = all_span.slice(0..3)
}
impl IfClause {
    pub fn new<T: Into<Expr>>(if_span: Span, cond_expr: T, body: Block) -> IfClause { 
        IfClause{ all_span: if_span + body.all_span, cond_expr: cond_expr.into(), body } 
    }
}
impl ElseIfClause {
    pub fn new<T: Into<Expr>>(elseif_span: Span, cond_expr: T, body: Block) -> ElseIfClause {
        ElseIfClause{ 
            all_span: body.all_span + elseif_span,
            cond_expr: cond_expr.into(),
            elseif_span,
            body
        }
    }
}
impl ElseClause {
    pub fn new(else_span: Span, body: Block) -> ElseClause {
        ElseClause{ all_span: else_span + body.all_span, body }
    }
}

// TODO: try change if stmt to vec<(if part, option<else part>)> to prevent 4 types
impl Node for IfClause {
    type ParseOutput = IfStatement;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.cond_expr)?;
        v.visit_block(&self.body)
    }
}
impl Node for ElseIfClause {
    type ParseOutput = IfStatement;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_else_if_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.cond_expr)?;
        v.visit_block(&self.body)
    }
}
impl Node for ElseClause {
    type ParseOutput = IfStatement;
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_else_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_block(&self.body)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct IfStatement {
    pub if_clause: IfClause,
    pub elseif_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
    pub all_span: Span,
}
impl ISyntaxFormat for IfStatement {
    fn format(&self, f: Formatter) -> String {

        let f = f.indent().header_text_or("if-stmt").space().span(self.all_span).endl();
        let IfClause{ all_span: ref if_all_span, cond_expr: ref if_cond_expr, body: ref if_body } = self.if_clause;
        let mut f = f.indent1().lit("if-clause").space().span(*if_all_span).endl()
            .set_prefix_text("cond-expr-is").apply2(if_cond_expr).unset_prefix_text().endl()
            .set_header_text("body").apply2(if_body).unset_header_text();
        for &ElseIfClause{ elseif_span: _, 
                cond_expr: ref elseif_cond_expr, body: ref elseif_body, all_span: ref elseif_all_span } in &self.elseif_clauses {
            f = f.endl()
                .indent1().lit("else-if-clause").space().span(*elseif_all_span).endl()
                .set_prefix_text("cond-expr-is").apply2(elseif_cond_expr).unset_prefix_text().endl()
                .set_header_text("body").apply2(elseif_body).unset_header_text();
        }
        if let Some(ElseClause{ body: ref else_body, all_span: ref else_all_span }) = self.else_clause { 
            f = f.endl()
                .indent1().lit("else-clause").space().span(*else_all_span).endl()
                .set_header_text("body").apply2(else_body);
        }
        f.finish()
    }
}
impl IfStatement {

    pub fn new_if(if_clause: IfClause, elseif_clauses: Vec<ElseIfClause>) -> IfStatement {
        match elseif_clauses.len() {
            0 => IfStatement{
                all_span: if_clause.all_span,
                if_clause, 
                elseif_clauses,
                else_clause: None,
            },
            n => IfStatement{
                all_span: if_clause.all_span + elseif_clauses[n - 1].all_span,
                if_clause,
                elseif_clauses,
                else_clause: None,
            }
        }
    }
    pub fn new_ifelse(if_clause: IfClause, elseif_clauses: Vec<ElseIfClause>, else_clause: ElseClause) -> IfStatement {
        IfStatement{
            all_span: if_clause.all_span + else_clause.all_span,
            if_clause, 
            elseif_clauses,
            else_clause: Some(else_clause),
        }
    }

    fn new(if_clause: IfClause, elseif_clauses: Vec<ElseIfClause>, else_clause: Option<ElseClause>) -> IfStatement {
        match (else_clause, elseif_clauses.len()) {
            (Some(else_clause), _) => IfStatement{
                all_span: if_clause.all_span + else_clause.all_span,
                if_clause, 
                elseif_clauses,
                else_clause: Some(else_clause),
            },
            (None, 0) => IfStatement{
                all_span: if_clause.all_span,
                if_clause, 
                elseif_clauses,
                else_clause: None,
            },
            (None, n) => IfStatement{
                all_span: if_clause.all_span + elseif_clauses[n - 1].all_span,
                if_clause, 
                elseif_clauses,
                else_clause: None,
            }
        }
    }
}
impl Node for IfStatement {
    type ParseOutput = IfStatement;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::If)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<IfStatement> {

        let if_span = sess.expect_keyword(Keyword::If)?;
        let if_expr = Expr::parse(sess)?;
        let if_body = Block::parse(sess)?;
        let if_clause = IfClause::new(if_span, if_expr, if_body);

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        loop {
            if let Some(else_span) = sess.try_expect_keyword(Keyword::Else) {
                if let Some(if_span) = sess.try_expect_keyword(Keyword::If) {
                    let elseif_span = else_span + if_span;
                    let elseif_expr = Expr::parse(sess)?;
                    let elseif_body = Block::parse(sess)?;
                    elseif_clauses.push(ElseIfClause::new(elseif_span, elseif_expr, elseif_body));
                } else {
                    // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                    // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                    // There was a bug fix here, now no more current_length handling!
                    // 17/6/21: a new physical structure update makes it much more simple
                    // 17/7/28: a new small update of parse_sess makes things even more simple
                    let else_body = Block::parse(sess)?;
                    else_clause = Some(ElseClause::new(else_span, else_body));
                }
            } else {
                break;
            }
        }

        Ok(IfStatement::new(if_clause, elseif_clauses, else_clause))
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_clause(&self.if_clause)?;
        for elseif in &self.elseif_clauses {
            v.visit_else_if_clause(elseif)?;
        }
        if let Some(r#else) = &self.else_clause {
            v.visit_else_clause(r#else)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn if_stmt_parse() {
    use super::{make_node, make_exprs, make_lit, FnCallExpr, MemberAccessExpr, SimpleName, ArrayDef, SimpleExprStatement, Statement};

    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    assert_eq!{ make_node!("if true { } else if false { } else {}" as IfStatement),
        IfStatement::new_ifelse(
            IfClause::new(Span::new(0, 1), 
                make_lit!(false, 3, 6),
                Block::new(Span::new(8, 10), vec![])
            ), vec![
                ElseIfClause::new(Span::new(12, 18), 
                    make_lit!(false, 20, 24),
                    Block::new(Span::new(26, 28), vec![])
                ),
            ],
            ElseClause::new(Span::new(30, 33),
                Block::new(Span::new(35, 36), vec![])
            )
        )
    }

    //              0         1         2         3         4         5         6         7
    //              012345678901234567890123456789012345678901234567890123456789012345678901
    assert_eq!{ make_node!("if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}" as IfStatement, [], ["sth", "do_sth", "a", "other", "do_other", "b", "writeln", "map"]),
        IfStatement::new_ifelse(
            IfClause::new(Span::new(0, 1), 
                make_lit!(1, 3, 3),
                Block::new(Span::new(5, 41), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(7, 20),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                SimpleName::new(1, Span::new(7, 9)),
                                Span::new(10, 10),
                                SimpleName::new(2, Span::new(11, 16))
                            ),
                            Span::new(17, 19), make_exprs![
                                SimpleName::new(3, Span::new(18, 18))
                            ]
                        ),
                    )),
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(22, 39),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                SimpleName::new(4, Span::new(22, 26)),
                                Span::new(27, 27),
                                SimpleName::new(5, Span::new(28, 35))
                            ),
                            Span::new(36, 38), make_exprs![
                                SimpleName::new(6, Span::new(37, 37))
                            ]
                        ),
                    ))
                ])
            ),
            vec![],
            ElseClause::new(
                Span::new(43, 46),
                Block::new(Span::new(48, 71), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(50, 70),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                ArrayDef::new(Span::new(50, 56), make_exprs![
                                    make_lit!(1, 51, 51),
                                    make_lit!(2, 53, 53),
                                    make_lit!(3, 55, 55),
                                ]),
                                Span::new(57, 57),
                                SimpleName::new(8, Span::new(58, 60))
                            ),
                            Span::new(61, 69), make_exprs![
                                SimpleName::new(7, Span::new(62, 68))
                            ]
                        )
                    ))
                ])
            )
        )
    }
}
