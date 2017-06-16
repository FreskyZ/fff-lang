///! fff-lang
///!
///! syntax/loop_stmt
///! LoopStatement = [LabelDef] fLoop Block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
    pub all_span: Span,
}
impl ISyntaxItemFormat for LoopStatement {
    fn format(&self, indent: u32) -> String {
        match self.name {
            Some(ref label_def) => format!("{}LoopStmt <{:?}>\n{}\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), self.all_span,
                label_def.format(indent + 1),
                LoopStatement::indent_str(indent + 1), self.loop_span,
                self.body.format(indent + 1)),
            None => format!("{}LoopStmt <{:?}>\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), self.all_span,
                LoopStatement::indent_str(indent + 1), self.loop_span,
                self.body.format(indent + 1)),
        }
    }
}
impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl LoopStatement { // New
    
    pub fn new_no_label(loop_span: Span, body: Block) -> LoopStatement {
        LoopStatement {
            all_span: loop_span.merge(&body.all_span),
            name: None, loop_span, body,
        }
    }
    pub fn new_with_label(name: LabelDef, loop_span: Span, body: Block) -> LoopStatement {
        LoopStatement {
            all_span: name.all_span.merge(&body.all_span),
            name: Some(name), loop_span, body,
        }
    }

    fn new(name: Option<LabelDef>, loop_span: Span, body: Block) -> LoopStatement {
        LoopStatement{
            all_span: match name { Some(ref name) => name.all_span.merge(&body.all_span), None => loop_span.merge(&body.all_span) },
            name, loop_span, body 
        }
    }
}
impl ISyntaxItemGrammar for LoopStatement {
    fn is_first_final(sess: &ParseSession) -> bool {
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Keyword(KeywordKind::Loop)) | (&Token::Keyword(KeywordKind::Loop), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for LoopStatement {
    type Target = LoopStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<LoopStatement> {

        let maybe_name = LabelDef::try_parse(sess)?;
        let loop_span = sess.expect_keyword(KeywordKind::Loop)?;
        let body = Block::parse(sess)?;
        return Ok(LoopStatement::new(maybe_name, loop_span, body));
    }
}

#[cfg(test)] #[test]
fn loop_stmt_format() {
    use codemap::SymbolCollection;
    use super::super::ISyntaxItemWithStr;
    
    //                                           1234567890123456789 0123 45678
    let actual = LoopStatement::with_test_input("@@: loop { println(\"233\"); }", &mut make_symbols!["@", "println", "233"]).format(0);
    let expect = r#"LoopStmt <<0>0-27>
  Label #1 <<0>0-2>
  'loop' <<0>4-7>
  Block <<0>9-27>
    ExprStmt <<0>11-25>
      PostfixExpr <<0>11-24>
        Ident #2 <<0>11-17>
        FunctionCall <<0>18-24>
          Literal #3 <<0>19-23>"#;

    if actual != expect { panic!("assertion failed: left: {}, right: {}", actual, expect) }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::LitExpr;
    use super::super::IdentExpr;
    use super::super::ISyntaxItemWithStr;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::Expr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;

    assert_eq!{ LoopStatement::with_test_str("loop {}"),
        LoopStatement::new_no_label(make_span!(0, 3), Block::new(make_span!(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    assert_eq!{ LoopStatement::with_test_input("@@: loop { println(\"233\"); }", &mut make_symbols!["@", "println", "233"]),
        LoopStatement::new_with_label(
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 7),
            Block::new(make_span!(9, 27), vec![
                Statement::Expr(ExprStatement::new_simple(
                    make_span!(11, 25), 
                    Expr::Postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new(make_id!(2), make_span!(11, 17)))),
                        make_span!(18, 24), vec![
                            Expr::new_lit(LitExpr::new(LitValue::new_str_lit(make_id!(3)), make_span!(19, 23)))
                        ]
                    ))
                ))
            ])
        )
    }
}