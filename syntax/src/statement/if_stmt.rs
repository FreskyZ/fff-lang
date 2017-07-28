///! fff-lang
///!
///! syntax/if_stmt
///! if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Keyword;

use super::super::Expr;
use super::super::Block;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfClause {
    pub cond_expr: Expr,
    pub body: Block,
    pub all_span: Span, // if_span = all_span.slice(0..1)
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ElseIfClause {
    pub elseif_span: Span, // else_span = elseif_span.slice(0..3), if_span = elseif_span.slice(-2..0)  // TODO: that slice(-2..0)
    pub cond_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ElseClause {
    pub body: Block,
    pub all_span: Span, // else_span = all_span.slice(0..3)
}
impl IfClause {
    pub fn new<T: Into<Expr>>(if_span: Span, cond_expr: T, body: Block) -> IfClause { 
        IfClause{ all_span: if_span.merge(&body.all_span), cond_expr: cond_expr.into(), body } 
    }
}
impl ElseIfClause {
    pub fn new<T: Into<Expr>>(elseif_span: Span, cond_expr: T, body: Block) -> ElseIfClause {
        ElseIfClause{ 
            all_span: body.all_span.merge(&elseif_span),
            cond_expr: cond_expr.into(),
            elseif_span,
            body
        }
    }
}
impl ElseClause {
    pub fn new(else_span: Span, body: Block) -> ElseClause {
        ElseClause{ all_span: else_span.merge(&body.all_span), body }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
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
impl fmt::Debug for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
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
                all_span: if_clause.all_span.merge(&elseif_clauses[n - 1].all_span),
                if_clause,
                elseif_clauses,
                else_clause: None,
            }
        }
    }
    pub fn new_ifelse(if_clause: IfClause, elseif_clauses: Vec<ElseIfClause>, else_clause: ElseClause) -> IfStatement {
        IfStatement{
            all_span: if_clause.all_span.merge(&else_clause.all_span),
            if_clause, 
            elseif_clauses,
            else_clause: Some(else_clause),
        }
    }

    fn new(if_clause: IfClause, elseif_clauses: Vec<ElseIfClause>, else_clause: Option<ElseClause>) -> IfStatement {
        match (else_clause, elseif_clauses.len()) {
            (Some(else_clause), _) => IfStatement{
                all_span: if_clause.all_span.merge(&else_clause.all_span),
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
                all_span: if_clause.all_span.merge(&elseif_clauses[n - 1].all_span),
                if_clause, 
                elseif_clauses,
                else_clause: None,
            }
        }
    }
}
impl ISyntaxGrammar for IfStatement {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::If) }
}
impl ISyntaxParse for IfStatement {
    type Output = IfStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<IfStatement> {

        let if_span = sess.expect_keyword(Keyword::If)?;
        let if_expr = Expr::parse(sess)?;
        let if_body = Block::parse(sess)?;
        let if_clause = IfClause::new(if_span, if_expr, if_body);

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        loop {
            if let Some(else_span) = sess.try_expect_keyword(Keyword::Else) {
                if let Some(if_span) = sess.try_expect_keyword(Keyword::If) {
                    let elseif_span = else_span.merge(&if_span);
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
}

#[cfg(test)] #[test]
fn if_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::ExprList;
    use super::super::FnCallExpr;
    use super::super::MemberAccessExpr;
    use super::super::SimpleName;
    use super::super::ArrayDef;
    use super::super::SimpleExprStatement;
    use super::super::LitExpr;
    use super::super::Statement;
    use super::super::TestInput;
    use super::super::WithTestInput;

    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    assert_eq!{ IfStatement::with_test_str("if true { } else if false { } else {}"),
        IfStatement::new_ifelse(
            IfClause::new(make_span!(0, 1), 
                LitExpr::new(LitValue::from(true), make_span!(3, 6)),
                Block::new(make_span!(8, 10), vec![])
            ), vec![
                ElseIfClause::new(make_span!(12, 18), 
                    LitExpr::new(LitValue::from(false), make_span!(20, 24)),
                    Block::new(make_span!(26, 28), vec![])
                ),
            ],
            ElseClause::new(make_span!(30, 33),
                Block::new(make_span!(35, 36), vec![])
            )
        )
    }

    //              0         1         2         3         4         5         6         7
    //              012345678901234567890123456789012345678901234567890123456789012345678901
    TestInput::new("if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}")
        .set_syms(make_symbols!["sth", "do_sth", "a", "other", "do_other", "b", "writeln", "map"])
        .apply::<IfStatement, _>()
        .expect_no_message()
        .expect_result(IfStatement::new_ifelse(
            IfClause::new(make_span!(0, 1), 
                LitExpr::new(LitValue::from(1), make_span!(3, 3)),
                Block::new(make_span!(5, 41), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(make_span!(7, 20),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                SimpleName::new(make_id!(1), make_span!(7, 9)),
                                make_span!(10, 10),
                                SimpleName::new(make_id!(2), make_span!(11, 16))
                            ),
                            make_span!(17, 19), make_exprs![
                                SimpleName::new(make_id!(3), make_span!(18, 18))
                            ]
                        ),
                    )),
                    Statement::SimpleExpr(SimpleExprStatement::new(make_span!(22, 39),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                SimpleName::new(make_id!(4), make_span!(22, 26)),
                                make_span!(27, 27),
                                SimpleName::new(make_id!(5), make_span!(28, 35))
                            ),
                            make_span!(36, 38), make_exprs![
                                SimpleName::new(make_id!(6), make_span!(37, 37))
                            ]
                        ),
                    ))
                ])
            ),
            vec![],
            ElseClause::new(
                make_span!(43, 46),
                Block::new(make_span!(48, 71), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(make_span!(50, 70),
                        FnCallExpr::new(
                            MemberAccessExpr::new(
                                ArrayDef::new(make_span!(50, 56), make_exprs![
                                    LitExpr::new(LitValue::from(1), make_span!(51, 51)),
                                    LitExpr::new(LitValue::from(2), make_span!(53, 53)),
                                    LitExpr::new(LitValue::from(3), make_span!(55, 55)),
                                ]),
                                make_span!(57, 57),
                                SimpleName::new(make_id!(8), make_span!(58, 60))
                            ),
                            make_span!(61, 69), make_exprs![
                                SimpleName::new(make_id!(7), make_span!(62, 68))
                            ]
                        )
                    ))
                ])
            )
        ))
    .finish();
}