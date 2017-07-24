///! fff-lang
///!
///! syntax/while_stmt
///! WhileStatement = LabelDef fWhile Expr Block
// TODO: add else for break, like python

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Keyword;

use super::super::Expr;
use super::super::Block;
use super::super::LabelDef;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
    pub all_span: Span,
}
impl ISyntaxFormat for WhileStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("while-stmt").space().span(self.all_span).endl();
        let f = match self.name { 
            Some(ref name) => f.set_header_text("loop-name").apply1(name).unset_header_text().endl(), 
            None => f.indent1().lit("no-loop-name").endl(),
        };
        f.indent1().lit("\"while\"").space().span(self.while_span).endl()
            .apply1(&self.loop_expr).endl()
            .set_header_text("body").apply1(&self.body)
            .finish()
    }
}
impl fmt::Debug for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl WhileStatement {
    
    pub fn new_no_label(while_span: Span, loop_expr: Expr, body: Block) -> WhileStatement {
        WhileStatement{ 
            all_span: while_span.merge(&body.all_span),
            name: None, loop_expr, body, while_span
        }
    }
    pub fn new_with_label(name: LabelDef, while_span: Span, loop_expr: Expr, body: Block) -> WhileStatement {
        WhileStatement{ 
            all_span: name.all_span.merge(&body.all_span),
            name: Some(name), loop_expr, body, while_span,
        }
    }

    fn new(maybe_name: Option<LabelDef>, while_span: Span, loop_expr: Expr, body: Block) -> WhileStatement {
        WhileStatement{
            all_span: match maybe_name { Some(ref name) => name.all_span.merge(&body.all_span), None => while_span.merge(&body.all_span) },
            name: maybe_name, loop_expr, body, while_span,
        }
    }
}
impl ISyntaxItemGrammar for WhileStatement {
    fn is_first_final(sess: &ParseSession) -> bool {
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Keyword(Keyword::While)) | (&Token::Keyword(Keyword::While), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for WhileStatement {
    type Target = WhileStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<WhileStatement> {
        
        let maybe_name = LabelDef::try_parse(sess)?;
        let while_span = sess.expect_keyword(Keyword::While)?;
        let expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(WhileStatement::new(maybe_name, while_span, expr, body));
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::LitExpr;
    use super::super::IdentExpr;
    use super::super::Statement;
    use super::super::SimpleExprStatement;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::TestInput;
    //                                           0        1         2         3         4        
    //                                           01234567890123456789012345 67890123456789012 3456
    TestInput::new("@2: while true { writeln(\"fresky hellooooo\"); }")
        .set_syms(make_symbols!["2", "writeln", "fresky hellooooo"])
        .apply::<WhileStatement, _>()
        .expect_no_message()
        .expect_result(WhileStatement::new_with_label(
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 8),
            Expr::Lit(LitExpr::new(LitValue::from(true), make_span!(10, 13))),
            Block::new(make_span!(15, 46), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(make_span!(17, 44), 
                    FnCallExpr::new(
                        Expr::Ident(IdentExpr::new(make_id!(2), make_span!(17, 23))),
                        make_span!(24, 43), make_exprs![
                            LitExpr::new(LitValue::new_str_lit(make_id!(3)), make_span!(25, 42))
                        ]
                    )
                ))
            ])
        ))
    .finish();
}
