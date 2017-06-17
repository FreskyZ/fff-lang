///! fff-lang
///!
///! syntax/for_stmt
///! ForStatement = [LabelDef] fFor fIdentifier fIn Expr Block 
// TODO: add else for break, like python

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Expr;
use super::super::LabelDef;
use super::super::Block;

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
    fn format(&self, indent: u32) -> String {
        format!("{}ForStmt <{:?}>{}\n{}'for' <{:?}>\n{}Ident {:?} <{:?}>\n{}\n{}", 
            ForStatement::indent_str(indent), self.all_span,
            match self.loop_name { Some(ref label_def) => format!("\n{}", label_def.format(indent + 1)), None => "".to_owned() },
            ForStatement::indent_str(indent + 1), self.for_span,
            ForStatement::indent_str(indent + 1), self.iter_name, self.iter_span,
            self.iter_expr.format(indent + 1),
            self.body.format(indent + 1),
        )
    }
}
impl fmt::Debug for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl ForStatement {

    pub fn new_no_label(all_span: Span, for_span: Span, 
        iter_name: SymbolID, iter_span: Span, iter_expr: Expr, 
        body: Block) -> ForStatement {
        ForStatement { 
            loop_name: None,
            for_span,
            iter_name, iter_span, iter_expr,
            body, all_span
        }
    }
    pub fn new_with_label(all_span: Span, loop_name: LabelDef, for_span: Span, 
        iter_name: SymbolID, iter_span: Span, iter_expr: Expr, 
        body: Block) -> ForStatement {
        ForStatement { 
            loop_name: Some(loop_name),
            for_span,
            iter_name, iter_span, iter_expr,
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
            (&Token::Label(_), &Token::Keyword(KeywordKind::For)) | (&Token::Keyword(KeywordKind::For), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for ForStatement {
    type Target = ForStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ForStatement> {

        let maybe_label = LabelDef::try_parse(sess)?;
        let for_strpos = sess.expect_keyword(KeywordKind::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let (iter_name, iter_strpos) = sess.expect_ident_or(vec![KeywordKind::Underscore])?;

        let _in_strpos = sess.expect_keyword(KeywordKind::In)?;
        let iter_expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(ForStatement::new(maybe_label, for_strpos, iter_name, iter_strpos, iter_expr, body));
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use codemap::SymbolCollection;
    use message::MessageCollection;
    use lexical::LitValue;
    use super::super::IdentExpr;
    use super::super::LitExpr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;
    use super::super::ExprStatement;
    use super::super::Statement;
    use super::super::ISyntaxItemWithStr;

    //                                       123456789012345678
    assert_eq!{ ForStatement::with_test_input_ret_messages("@2: for i in 42 {}", &mut make_symbols!["2", "i"]), (
        Some(ForStatement::new_with_label(make_span!(0, 17),
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 6),
            make_id!(2), make_span!(8, 8),
            Expr::new_lit(LitExpr::new(LitValue::from(42), make_span!(13, 14))),
            Block::new(make_span!(16, 17), vec![])
        )),
        make_messages![],
    )}

    assert_eq!{ //                     0         1         2         3         4         5         6         7         
                //                     01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
        ForStatement::with_test_input("@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }", 
            //                  1        2    3        4            5          6          7
            &mut make_symbols!["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"]),
        ForStatement::new_with_label(make_span!(0, 77),
            LabelDef::new(make_id!(1), make_span!(0, 6)),
            make_span!(8, 10),
            make_id!(2), make_span!(12, 12),
            Expr::Postfix(PostfixExpr::FunctionCall(
                Box::new(Expr::Postfix(PostfixExpr::MemberAccess(
                    Box::new(Expr::Postfix(PostfixExpr::FunctionCall(
                        Box::new(Expr::Postfix(PostfixExpr::MemberAccess(
                            Box::new(Expr::Postfix(PostfixExpr::FunctionCall(
                                Box::new(Expr::Primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(3), make_span!(17, 21))))),
                                make_span!(22, 28), vec![
                                    Expr::new_lit(LitExpr::new(LitValue::from(0), make_span!(23, 23))),
                                    Expr::new_lit(LitExpr::new(LitValue::from(10), make_span!(26, 27))),
                                ]
                            ))),
                            make_span!(29, 29),
                            make_id!(4), make_span!(30, 38)
                        ))), 
                        make_span!(39, 40), vec![]
                    ))), 
                    make_span!(41, 41),
                    make_id!(5), make_span!(42, 48)
                ))),
                make_span!(49, 50), vec![]
            )),
            Block::new(make_span!(52, 77), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(54, 75), 
                    Expr::Postfix(PostfixExpr::FunctionCall(
                        Box::new(Expr::Primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(6), make_span!(54, 60))))),
                        make_span!(61, 74), vec![
                            Expr::new_lit(LitExpr::new(LitValue::new_str_lit(make_id!(7)), make_span!(62, 73)))
                    ]))
                ))
            ])
        )
    }
}