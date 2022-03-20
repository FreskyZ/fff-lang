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
            all_span: elseif_span + body.all_span,
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
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.cond_expr)?;
        v.visit_block(&self.body)
    }
}
impl Node for ElseIfClause {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_else_if_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.cond_expr)?;
        v.visit_block(&self.body)
    }
}
impl Node for ElseClause {
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
impl IfStatement {

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
impl Parser for IfStatement {
    type Output = IfStatement;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::If)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<IfStatement, Unexpected> {

        let if_span = cx.expect_keyword(Keyword::If)?;
        let if_expr = cx.expect::<Expr>()?;
        let if_body = cx.expect::<Block>()?;
        let if_clause = IfClause::new(if_span, if_expr, if_body);

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        loop {
            if let Some(else_span) = cx.try_expect_keyword(Keyword::Else) {
                if let Some(if_span) = cx.try_expect_keyword(Keyword::If) {
                    let elseif_span = else_span + if_span;
                    let elseif_expr = cx.expect::<Expr>()?;
                    let elseif_body = cx.expect::<Block>()?;
                    elseif_clauses.push(ElseIfClause::new(elseif_span, elseif_expr, elseif_body));
                } else {
                    // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                    // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                    // There was a bug fix here, now no more current_length handling!
                    // 17/6/21: a new physical structure update makes it much more simple
                    // 17/7/28: a new small update of parse_cx makes things even more simple
                    let else_body = cx.expect::<Block>()?;
                    else_clause = Some(ElseClause::new(else_span, else_body));
                }
            } else {
                break;
            }
        }

        Ok(IfStatement::new(if_clause, elseif_clauses, else_clause))
    }
}

impl Node for IfStatement {
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
    use super::{FnCallExpr, ArrayDef, SimpleExprStatement, Statement};

    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ "if true { } else if false { } else {}" as IfStatement,
        IfStatement::new_ifelse(
            IfClause::new(Span::new(0, 1), 
                make_lit!(true, 3, 6),
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
    case!{ "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}" as IfStatement,
        IfStatement::new_ifelse(
            IfClause::new(Span::new(0, 1), 
                make_lit!(1, 3, 3),
                Block::new(Span::new(5, 41), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(7, 20),
                        FnCallExpr::new(
                            make_expr!(member 7:16 dot 10:10
                                make_name!(simple 7:9 #2),
                                make_name!(simple bare 11:16 #3)),
                            Span::new(17, 19), make_exprs![
                                make_name!(simple 18:18 #4)
                            ]
                        ),
                    )),
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(22, 39),
                        FnCallExpr::new(
                            make_expr!(member 22:35 dot 27:27
                                make_name!(simple 22:26 #5),
                                make_name!(simple bare 28:35 #6)),
                            Span::new(36, 38), make_exprs![
                                make_name!(simple 37:37 #7)
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
                            make_expr!(member 50:60 dot 57:57
                                ArrayDef::new(Span::new(50, 56), make_exprs![
                                    make_lit!(1, 51, 51),
                                    make_lit!(2, 53, 53),
                                    make_lit!(3, 55, 55),
                                ]),
                                make_name!(simple bare 58:60 #8)),
                            Span::new(61, 69), make_exprs![
                                make_name!(simple 62:68 #9)
                            ]
                        )
                    ))
                ])
            )
        ), strings ["sth", "do_sth", "a", "other", "do_other", "b", "map", "writeln"]
    }
}
