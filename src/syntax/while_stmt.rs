///! fff-lang
///!
///! syntax/while_stmt
///! while-stmt = [ label-def ] 'while' expr block
// TODO: add else for break, like python

use super::prelude::*;
use super::{Expr, Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
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
            all_span: while_span + body.all_span,
            name: None, loop_expr, body, while_span
        }
    }
    pub fn new_with_label(name: LabelDef, while_span: Span, loop_expr: Expr, body: Block) -> WhileStatement {
        WhileStatement{ 
            all_span: name.all_span + body.all_span,
            name: Some(name), loop_expr, body, while_span,
        }
    }

    fn new(maybe_name: Option<LabelDef>, while_span: Span, loop_expr: Expr, body: Block) -> WhileStatement {
        WhileStatement{
            all_span: match maybe_name { Some(ref name) => name.all_span + body.all_span, None => while_span + body.all_span },
            name: maybe_name, loop_expr, body, while_span,
        }
    }
}
impl<'ecx, 'scx, F> Node<'ecx, 'scx, F> for WhileStatement where F: FileSystem {
    type ParseOutput = WhileStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<WhileStatement> {
        
        let maybe_name = LabelDef::try_parse(sess)?;
        let while_span = sess.expect_keyword(Keyword::While)?;
        let expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(WhileStatement::new(maybe_name, while_span, expr, body));
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use super::{make_node, make_exprs, LitExpr, SimpleName, Statement, SimpleExprStatement, FnCallExpr};
    //                                           0        1         2         3         4        
    //                                           01234567890123456789012345 67890123456789012 3456
    assert_eq!{ make_node!("@2: while true { writeln(\"fresky hellooooo\"); }" as WhileStatement, [], ["2", "writeln", "fresky hellooooo"]),
        WhileStatement::new_with_label(
            LabelDef::new(1, Span::new(0, 2)),
            Span::new(4, 8),
            Expr::Lit(LitExpr::new(true, Span::new(10, 13))),
            Block::new(Span::new(15, 46), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(17, 44), 
                    FnCallExpr::new(
                        SimpleName::new(2, Span::new(17, 23)),
                        Span::new(24, 43), make_exprs![
                            LitExpr::new(4u32, Span::new(25, 42))
                        ]
                    )
                ))
            ])
        )
    }
}
