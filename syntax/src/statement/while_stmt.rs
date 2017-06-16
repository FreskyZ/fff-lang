///! fff-lang
///!
///! syntax/while_stmt
///! WhileStatement = LabelDef fWhile Expr Block
// TODO: add else for break, like python

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Expr;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
    pub all_span: Span,
}
impl ISyntaxItemFormat for WhileStatement {
    fn format(&self, indent: u32) -> String {
        match self.name {
            Some(ref name) => format!("{}WhileStmt <{:?}>\n{}\n{}'while' <{:?}>\n{}\n{}", 
                WhileStatement::indent_str(indent), self.all_span,
                name.format(indent + 1),
                WhileStatement::indent_str(indent + 1), self.while_span,
                self.loop_expr.format(indent + 1),
                self.body.format(indent + 1)),
            None => format!("{}WhileStmt <{:?}>\n{}'while' <{:?}>\n{}\n{}", 
                WhileStatement::indent_str(indent), self.all_span,
                WhileStatement::indent_str(indent + 1), self.while_span,
                self.loop_expr.format(indent + 1),
                self.body.format(indent + 1)),
        }
    }
}
impl fmt::Debug for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
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
            (&Token::Label(_), &Token::Keyword(KeywordKind::While)) | (&Token::Keyword(KeywordKind::While), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for WhileStatement {
    type Target = WhileStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<WhileStatement> {
        
        let maybe_name = LabelDef::try_parse(sess)?;
        let while_span = sess.expect_keyword(KeywordKind::While)?;
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
    use super::super::ISyntaxItemWithStr;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;

    //                                           0        1         2         3         4        
    //                                           01234567890123456789012345 67890123456789012 3456
    assert_eq!{ WhileStatement::with_test_input("@2: while true { writeln(\"fresky hellooooo\"); }", &mut make_symbols!["2", "writeln", "fresky hellooooo"]),
        WhileStatement::new_with_label(
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 8),
            Expr::new_lit(LitExpr::new(LitValue::from(true), make_span!(10, 13))),
            Block::new(make_span!(15, 46), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(17, 44), 
                    Expr::Postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(2), make_span!(17, 23)))),
                        make_span!(24, 43), vec![
                            Expr::new_lit(LitExpr::new(LitValue::new_str_lit(make_id!(3)), make_span!(25, 42)))
                        ]
                    ))
                ))
            ])
        )
    }
}
