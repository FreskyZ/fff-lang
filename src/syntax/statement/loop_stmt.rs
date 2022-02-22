///! fff-lang
///!
///! syntax/loop_stmt
///! loop_stmt = [ label_def ] 'loop' block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use std::fmt;
use crate::codemap::Span;
use crate::lexical::Token;
use crate::lexical::Keyword;
use super::super::Block;
use super::super::LabelDef;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
    pub all_span: Span,
}
impl ISyntaxFormat for LoopStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("loop-stmt").space().span(self.all_span).endl();
        let f = match self.name { 
            Some(ref name) => f.set_header_text("loop-name").apply1(name).unset_header_text().endl(), 
            None => f.indent1().lit("no-loop-name").endl(),
        };
        f.indent1().lit("\"loop\"").space().span(self.loop_span).endl()
            .set_header_text("body").apply1(&self.body)
            .finish()
    }
}
impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
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
impl ISyntaxGrammar for LoopStatement {
    fn matches_first(tokens: &[&Token]) -> bool {
        match (tokens[0], tokens[2]) {
            (&Token::Label(_), &Token::Keyword(Keyword::Loop)) | (&Token::Keyword(Keyword::Loop), _) => true,
            _ => false
        }
    }
}
impl ISyntaxParse for LoopStatement {
    type Output = LoopStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<LoopStatement> {

        let maybe_name = LabelDef::try_parse(sess)?;
        let loop_span = sess.expect_keyword(Keyword::Loop)?;
        let body = Block::parse(sess)?;
        return Ok(LoopStatement::new(maybe_name, loop_span, body));
    }
}

#[cfg(test)] #[test]
fn loop_stmt_format() {
    use crate::codemap::SymbolCollection;
    use super::super::TestInput;
    use super::super::WithTestInput;
    
    //                  1234567890123456789 0123 45678
    let actual = LoopStatement::with_test_input(
        TestInput::new("@@: loop { println(\"233\"); }").set_syms(make_symbols!["@", "println", "233"])).0.unwrap().format(Formatter::empty());
    let expect = r#"loop-stmt <<0>0-27>
  loop-name #1 <<0>0-2>
  "loop" <<0>4-7>
  body <<0>9-27>
    expr-stmt simple <<0>11-25>
      fn-call <<0>11-24>
        base-is ident-use #2 <<0>11-17>
        parenthenes <<0>18-24>
        literal #3 <<0>19-23>"#;

    if actual != expect { panic!("assertion failed: left: {}, right: {}", actual, expect) }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use crate::codemap::SymbolCollection;
    use crate::lexical::LitValue;
    use super::super::LitExpr;
    use super::super::SimpleName;
    use super::super::WithTestInput;
    use super::super::Statement;
    use super::super::SimpleExprStatement;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::TestInput;

    assert_eq!{ LoopStatement::with_test_str("loop {}"),
        LoopStatement::new_no_label(make_span!(0, 3), Block::new(make_span!(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    TestInput::new("@@: loop { println(\"233\"); }")
        .set_syms(make_symbols!["@", "println", "233"])
        .apply::<LoopStatement, _>()
        .expect_no_message()
        .expect_result(LoopStatement::new_with_label(
            LabelDef::new(make_id!(1), make_span!(0, 2)),
            make_span!(4, 7),
            Block::new(make_span!(9, 27), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(make_span!(11, 25), 
                    FnCallExpr::new(
                        SimpleName::new(make_id!(2), make_span!(11, 17)),
                        make_span!(18, 24), make_exprs![
                            LitExpr::new(make_lit!(str, 3), make_span!(19, 23))
                        ]
                    )
                ))
            ])
        ))
    .finish();
}