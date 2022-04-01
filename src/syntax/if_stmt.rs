///! syntax::if_stmt:
///! if_stmt = 'if' expr block { 'else' 'if' expr block } [ 'else' block ]

use super::prelude::*;
use super::{Expr, Block};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct IfClause {
    pub condition: Expr,
    pub body: Block,
    pub all_span: Span, // start from `if` or `else`
}

impl Node for IfClause {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_clause(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.condition)?;
        v.visit_block(&self.body)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ElseClause {
    pub body: Block,
    pub all_span: Span, // else_span = all_span.slice(0..3)
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
    pub elseif_clauses: Vec<IfClause>,
    pub else_clause: Option<ElseClause>,
    pub all_span: Span,
}

impl Parser for IfStatement {
    type Output = IfStatement;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::If)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<IfStatement, Unexpected> {

        let mut all_span = cx.expect_keyword(Keyword::If)?;

        cx.no_object_literals.push(true);
        let if_expr = cx.expect::<Expr>()?;
        cx.no_object_literals.pop();
        let if_body = cx.expect::<Block>()?;
        all_span += if_body.all_span;
        let if_clause = IfClause{ all_span, condition: if_expr, body: if_body };

        let mut elseif_clauses = Vec::new();
        let mut else_clause = None;
        while let Some(else_span) = cx.try_expect_keyword(Keyword::Else) {
            if let Some(if_span) = cx.try_expect_keyword(Keyword::If) {
                let elseif_span = else_span + if_span;
                cx.no_object_literals.push(true);
                let elseif_expr = cx.expect::<Expr>()?;
                cx.no_object_literals.pop();
                let elseif_body = cx.expect::<Block>()?;
                all_span += elseif_body.all_span;
                elseif_clauses.push(IfClause{ all_span: elseif_span + elseif_body.all_span, condition: elseif_expr, body: elseif_body });
            } else {
                // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                // There was a bug fix here, now no more current_length handling!
                // 17/6/21: a new physical structure update makes it much more simple
                // 17/7/28: a new small update of parse_cx makes things even more simple
                let else_body = cx.expect::<Block>()?;
                all_span += else_body.all_span;
                else_clause = Some(ElseClause{ all_span: else_span + else_body.all_span, body: else_body });
            }
        }

        Ok(IfStatement{ all_span, if_clause, elseif_clauses, else_clause })
    }
}

impl Node for IfStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_if_clause(&self.if_clause)?;
        for elseif in &self.elseif_clauses {
            v.visit_if_clause(elseif)?;
        }
        if let Some(r#else) = &self.else_clause {
            v.visit_else_clause(r#else)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn if_stmt_parse() {
    use super::{SimpleExprStatement, Statement};

    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ "if true { } else if false { } else {}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 36),
            if_clause: IfClause{ all_span: Span::new(0, 10), 
                condition: make_expr!(true 3:6),
                body: Block::new(Span::new(8, 10), vec![]) }, 
            elseif_clauses: vec![IfClause{ all_span: Span::new(12, 28), 
                condition: make_expr!(false 20:24),
                body: Block::new(Span::new(26, 28), vec![]) }],
            else_clause: Some(ElseClause{ all_span: Span::new(30, 36),
                body: Block::new(Span::new(35, 36), vec![]) }) }
    }

    //              0         1         2         3         4         5         6         7
    //              012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 71),
            if_clause: IfClause{ all_span: Span::new(0, 41), 
                condition: make_expr!(i32 1 3:3),
                body: Block::new(Span::new(5, 41), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(7, 20),
                        make_expr!(fn 7:19 paren 17:19
                            make_expr!(member 7:16 dot 10:10
                                make_name!(simple 7:9 #2),
                                make_name!(simple bare 11:16 #3)),
                            make_name!(simple 18:18 #4)))),
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(22, 39),
                        make_expr!(fn 22:38 paren 36:38
                            make_expr!(member 22:35 dot 27:27
                                make_name!(simple 22:26 #5),
                                make_name!(simple bare 28:35 #6)),
                            make_name!(simple 37:37 #7))))]) },
            elseif_clauses: vec![],
            else_clause: Some(ElseClause{ all_span: Span::new(43, 71),
                body: Block::new(Span::new(48, 71), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(50, 70),
                        make_expr!(fn 50:69 paren 61:69
                            make_expr!(member 50:60 dot 57:57
                                make_expr!(array 50:56
                                    make_expr!(i32 1 51:51),
                                    make_expr!(i32 2 53:53),
                                    make_expr!(i32 3 55:55)),
                                make_name!(simple bare 58:60 #8)),
                            make_name!(simple 62:68 #9))))]) }),
        }, strings ["sth", "do_sth", "a", "other", "do_other", "b", "map", "writeln"]
    }

    // if condition does not expect object literal, unless parened
    //      0         1         2
    //      012345678901234567890123
    case!{ "if a {} else if (b{}) {}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 23), 
            if_clause: IfClause{ all_span: Span::new(0, 6),
                condition: make_name!(simple 3:3 #2),
                body: Block::new(Span::new(5, 6), vec![]) },
            elseif_clauses: vec![IfClause{ all_span: Span::new(8, 23),
                condition: make_expr!(paren 16:20
                    make_expr!(object 17:19 quote 18:19
                        make_name!(simple 17:17 #3),)),
                body: Block::new(Span::new(22, 23), vec![]) }],
            else_clause: None,
        }
    }
}
