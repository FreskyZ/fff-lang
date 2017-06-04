///! fff-lang
///!
///! syntax/while_stmt
///! WhileStatement = LabelDef fWhile BinaryExpr Block
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
use super::super::BinaryExpr;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct WhileStatement {
    label_def: Option<LabelDef>,
    loop_expr: BinaryExpr,
    body: Block,
    while_strpos: Span,
    all_strpos: Span,
}
impl ISyntaxItemFormat for WhileStatement {
    fn format(&self, indent: u32) -> String {
        match self.label_def {
            Some(ref label_def) => format!("{}WhileStmt <{:?}>\n{}\n{}'while' <{:?}>\n{}\n{}", 
                WhileStatement::indent_str(indent), self.all_strpos,
                label_def.format(indent + 1),
                WhileStatement::indent_str(indent + 1), self.while_strpos,
                self.loop_expr.format(indent + 1),
                self.body.format(indent + 1)),
            None => format!("{}WhileStmt <{:?}>\n{}'while' <{:?}>\n{}\n{}", 
                WhileStatement::indent_str(indent), self.all_strpos,
                WhileStatement::indent_str(indent + 1), self.while_strpos,
                self.loop_expr.format(indent + 1),
                self.body.format(indent + 1)),
        }
    }
}
impl fmt::Debug for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl WhileStatement {
    
    pub fn new_no_label(all_strpos: Span, while_strpos: Span, loop_expr: BinaryExpr, body: Block) -> WhileStatement {
        WhileStatement{ label_def: None, loop_expr, body, while_strpos, all_strpos }
    }
    pub fn new_with_label(all_strpos: Span, label_def: LabelDef, while_strpos: Span, loop_expr: BinaryExpr, body: Block) -> WhileStatement {
        WhileStatement{ label_def: Some(label_def), loop_expr, body, while_strpos, all_strpos }
    }

    fn new_some_label(maybe_label_def: Option<LabelDef>, while_strpos: Span, loop_expr: BinaryExpr, body: Block) -> WhileStatement {
        WhileStatement{
            all_strpos: 
                (match maybe_label_def { Some(ref label_def) => label_def.get_all_strpos(), None => while_strpos })
                .merge(&body.get_all_strpos()),
            label_def: maybe_label_def,
            loop_expr, body, while_strpos,
        }
    }

    pub fn get_label(&self) -> Option<&LabelDef> { self.label_def.as_ref() }
    pub fn get_loop_expr(&self) -> &BinaryExpr { &self.loop_expr }
    pub fn get_body(&self) -> &Block { &self.body }
    pub fn get_while_strpos(&self) -> Span { self.while_strpos }

    pub fn get_all_strpos(&self) -> Span { self.all_strpos }
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

    fn parse(sess: &mut ParseSession) -> ParseResult<WhileStatement> {
        
        let maybe_label = LabelDef::try_parse(sess)?;
        let while_strpos = sess.expect_keyword(KeywordKind::While)?;
        let expr = BinaryExpr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(WhileStatement::new_some_label(maybe_label, while_strpos, expr, body));
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;
    use lexical::LitValue;

    //                                         0        1         2         3         4        
    //                                         1234567890123456789012345 67890123456789012 34567
    assert_eq!{ WhileStatement::with_test_str("@2: while true { writeln(\"fresky hellooooo\"); }"),
        WhileStatement::new_with_label(
            make_span!(0, 46),
            LabelDef::new("2".to_owned(), make_span!(0, 2)),
            make_span!(4, 8),
            BinaryExpr::new_lit(LitValue::from(true), make_span!(10, 13)),
            Block::new(make_span!(15, 46), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(17, 44), 
                    BinaryExpr::new_postfix(
                        PostfixExpr::new_function_call(
                            PostfixExpr::new_primary(PrimaryExpr::new_ident("writeln".to_owned(), make_span!(17, 23))),
                            make_span!(24, 43), vec![
                                BinaryExpr::new_lit(LitValue::from("fresky hellooooo"), make_span!(25, 42))
                            ]
                        )
                    )
                ))
            ])
        )
    }
}
