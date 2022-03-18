///! fff-lang
///!
///! syntax/loop_stmt
///! loop_stmt = [ label_def ] 'loop' block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use super::prelude::*;
use super::{Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct LoopStatement {
    pub name: Option<LabelDef>,
    pub body: Block,
    pub loop_span: Span,
    pub all_span: Span,
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

impl Parser for LoopStatement {
    type Output = LoopStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::Loop)) | (Token::Keyword(Keyword::Loop), _))
    }

    fn parse(cx: &mut ParseContext) -> Result<LoopStatement, Unexpected> {

        let maybe_name = cx.try_expect::<LabelDef>()?;
        let loop_span = cx.expect_keyword(Keyword::Loop)?;
        let body = cx.expect::<Block>()?;
        return Ok(LoopStatement::new(maybe_name, loop_span, body));
    }
}

impl Node for LoopStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_loop_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.name {
            v.visit_label_def(name)?;
        }
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::{SimpleName, Statement, SimpleExprStatement, FnCallExpr};

    case!{ "loop {}" as LoopStatement,
        LoopStatement::new_no_label(Span::new(0, 3), Block::new(Span::new(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    case!{ "@@: loop { println(\"233\"); }" as LoopStatement,
        LoopStatement::new_with_label(
            LabelDef::new(2, Span::new(0, 2)),
            Span::new(4, 7),
            Block::new(Span::new(9, 27), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(11, 25), 
                    FnCallExpr::new(
                        SimpleName::new(3, Span::new(11, 17)),
                        Span::new(18, 24), make_exprs![
                            make_lit!(4: str, 19, 23),
                        ]
                    )
                ))
            ])
        ), strings ["@", "println", "233"]
    }
}
