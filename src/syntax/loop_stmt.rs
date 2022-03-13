///! fff-lang
///!
///! syntax/loop_stmt
///! loop_stmt = [ label_def ] 'loop' block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use super::prelude::*;
use super::{Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
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
            all_span: loop_span + body.all_span,
            name: None, loop_span, body,
        }
    }
    pub fn new_with_label(name: LabelDef, loop_span: Span, body: Block) -> LoopStatement {
        LoopStatement {
            all_span: name.all_span + body.all_span,
            name: Some(name), loop_span, body,
        }
    }

    fn new(name: Option<LabelDef>, loop_span: Span, body: Block) -> LoopStatement {
        LoopStatement{
            all_span: match name { Some(ref name) => name.all_span + body.all_span, None => loop_span + body.all_span },
            name, loop_span, body 
        }
    }
}
impl Node for LoopStatement {
    type ParseOutput = LoopStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<LoopStatement> where F: FileSystem {

        let maybe_name = LabelDef::try_parse(sess)?;
        let loop_span = sess.expect_keyword(Keyword::Loop)?;
        let body = Block::parse(sess)?;
        return Ok(LoopStatement::new(maybe_name, loop_span, body));
    }
}

#[cfg(test)] #[test]
fn loop_stmt_format() {
    use super::make_node;
    
    //                  1234567890123456789 0123 45678
    assert_eq!{ make_node!("@@: loop { println(\"233\"); }" as LoopStatement, [], ["@", "println", "233"]).format(Formatter::empty()), r#"loop-stmt <<0>0-27>
  loop-name #1 <<0>0-2>
  "loop" <<0>4-7>
  body <<0>9-27>
    expr-stmt simple <<0>11-25>
      fn-call <<0>11-24>
        base-is ident-use #2 <<0>11-17>
        parenthenes <<0>18-24>
        literal #3 <<0>19-23>"#
    }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::{make_node, make_exprs, LitExpr, SimpleName, Statement, SimpleExprStatement, FnCallExpr};

    assert_eq!{ make_node!("loop {}" as LoopStatement),
        LoopStatement::new_no_label(Span::new(0, 3), Block::new(Span::new(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    assert_eq!{ make_node!("@@: loop { println(\"233\"); }" as LoopStatement, [], ["@", "println", "233"]),
        LoopStatement::new_with_label(
            LabelDef::new(1, Span::new(0, 2)),
            Span::new(4, 7),
            Block::new(Span::new(9, 27), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(11, 25), 
                    FnCallExpr::new(
                        SimpleName::new(2, Span::new(11, 17)),
                        Span::new(18, 24), make_exprs![
                            LitExpr::new(3u32, Span::new(19, 23))
                        ]
                    )
                ))
            ])
        )
    }
}
