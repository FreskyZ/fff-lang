///! fff-lang
///!
///! syntax/for_stmt
///! for_stmt = [ label_def ] 'for' identifier 'in' expr block

// TODO: add else for break, like python

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::Keyword;

use super::super::Expr;
use super::super::Block;
use super::super::LabelDef;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ForStatement {
    pub loop_name: Option<LabelDef>,
    pub for_span: Span,
    pub iter_name: SymbolID,
    pub iter_span: Span,
    pub iter_expr: Expr,
    pub body: Block,
    pub all_span: Span,
}
impl ISyntaxItemFormat for ForStatement {
    fn format(&self, f: Formatter) -> String {
        format!("{}ForStmt <{}>{}\n{}'for' <{}>\n{}Ident {:?} <{}>\n{}\n{}", 
            f.indent(), f.span(self.all_span),
            match self.loop_name { Some(ref label_def) => format!("\n{}", f.apply1(label_def)), None => "".to_owned() },
            f.indent1(), f.span(self.for_span),
            f.indent1(), self.iter_name, f.span(self.iter_span),
            f.apply1(&self.iter_expr),
            f.apply1(&self.body),
        )
    }
}
impl fmt::Debug for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::default())) }
}
impl ForStatement {

    pub fn new_no_label<T: Into<Expr>>(
            all_span: Span, for_span: Span, 
            iter_name: SymbolID, iter_span: Span, iter_expr: T, 
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
            iter_name: SymbolID, iter_span: Span, iter_expr: T, 
            body: Block) -> ForStatement {
        ForStatement { 
            loop_name: Some(loop_name),
            for_span,
            iter_name, iter_span, 
            iter_expr: iter_expr.into(),
            body, all_span
        }
    }

    fn new(loop_name: Option<LabelDef>, for_span: Span,
        iter_name: SymbolID, iter_span: Span, iter_expr: Expr,
        body: Block) -> ForStatement {
        ForStatement{
            all_span: (match loop_name { Some(ref label) => label.all_span, None => for_span }).merge(&body.all_span),
            loop_name, for_span, iter_name, iter_expr, iter_span, body,
        }
    }
}
impl ISyntaxItemGrammar for ForStatement {

    fn is_first_final(sess: &ParseSession) -> bool {
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Keyword(Keyword::For)) | (&Token::Keyword(Keyword::For), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for ForStatement {
    type Target = ForStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ForStatement> {

        let maybe_label = LabelDef::try_parse(sess)?;
        let for_strpos = sess.expect_keyword(Keyword::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let (iter_name, iter_strpos) = sess.expect_ident_or(vec![Keyword::Underscore])?;

        let _in_strpos = sess.expect_keyword(Keyword::In)?;
        let iter_expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(ForStatement::new(maybe_label, for_strpos, iter_name, iter_strpos, iter_expr, body));
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::IdentExpr;
    use super::super::LitExpr;
    use super::super::SimpleExprStatement;
    use super::super::Statement;
    use super::super::MemberAccessExpr;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::TestInput;

    //                                       123456789012345678
    TestInput::new("@2: for i in 42 {}")
        .set_syms(make_symbols!["2", "i"])
        .apply::<ForStatement, _>()
        .expect_no_message()
        .expect_result(ForStatement::new_with_label(make_span!(0, 17),
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 6),
            make_id!(2), make_span!(8, 8),
            Expr::Lit(LitExpr::new(LitValue::from(42), make_span!(13, 14))),
            Block::new(make_span!(16, 17), vec![])
        ))
    .finish();

    //              0         1         2         3         4         5         6         7         
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    TestInput::new("@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }")
    //                           1        2    3        4            5          6          7
        .set_syms(make_symbols!["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"])
        .apply::<ForStatement, _>()
        .expect_no_message()
        .expect_result(ForStatement::new_with_label(make_span!(0, 77),
            LabelDef::new(make_id!(1), make_span!(0, 6)),
            make_span!(8, 10),
            make_id!(2), make_span!(12, 12),
            FnCallExpr::new(
                MemberAccessExpr::new(
                    FnCallExpr::new(
                        MemberAccessExpr::new(
                            FnCallExpr::new(
                                IdentExpr::new(make_id!(3), make_span!(17, 21)),
                                make_span!(22, 28), make_exprs![
                                    LitExpr::new(LitValue::from(0), make_span!(23, 23)),
                                    LitExpr::new(LitValue::from(10), make_span!(26, 27)),
                                ]
                            ),
                            make_span!(29, 29),
                            IdentExpr::new(make_id!(4), make_span!(30, 38))
                        ),
                        make_span!(39, 40), ExprList::new(vec![])
                    ), 
                    make_span!(41, 41),
                    IdentExpr::new(make_id!(5), make_span!(42, 48))
                ),
                make_span!(49, 50), ExprList::new(vec![])
            ),
            Block::new(make_span!(52, 77), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(make_span!(54, 75),
                    FnCallExpr::new(
                        Expr::Ident(IdentExpr::new(make_id!(6), make_span!(54, 60))),
                        make_span!(61, 74), ExprList::new(vec![
                            Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(7)), make_span!(62, 73)))
                        ])
                    )
                ))
            ])
        ))
    .finish();
}