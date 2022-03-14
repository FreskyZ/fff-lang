///! fff-lang
///!
///! syntax/while_stmt
///! while-stmt = [ label-def ] 'while' expr block
// TODO: add else for break, like python

use super::prelude::*;
use super::{Expr, Block, LabelDef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct WhileStatement {
    pub name: Option<LabelDef>,
    pub loop_expr: Expr,
    pub body: Block,
    pub while_span: Span,
    pub all_span: Span,
}
impl WhileStatement {
    
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
impl Node for WhileStatement {
    type ParseOutput = WhileStatement;

    fn matches3(current: &Token, _peek: &Token, peek2: &Token) -> bool {
        matches!((current, peek2), (Token::Label(_), Token::Keyword(Keyword::While)) | (Token::Keyword(Keyword::While), _))
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<WhileStatement> {
        
        let maybe_name = LabelDef::try_parse(sess)?;
        let while_span = sess.expect_keyword(Keyword::While)?;
        let expr = Expr::parse(sess)?;
        let body = Block::parse(sess)?;
        return Ok(WhileStatement::new(maybe_name, while_span, expr, body));
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_while_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(name) = &self.name {
            v.visit_label_def(name)?;
        }
        v.visit_expr(&self.loop_expr)?;
        v.visit_block(&self.body)
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use super::{make_node, make_exprs, make_lit, SimpleName, Statement, SimpleExprStatement, FnCallExpr};
    //                                           0        1         2         3         4        
    //                                           01234567890123456789012345 67890123456789012 3456
    assert_node_eq!{ make_node!("@2: while true { writeln(\"fresky hellooooo\"); }" as WhileStatement, [], ["2", "writeln", "fresky hellooooo"]),
        WhileStatement::new_with_label(
            LabelDef::new(2, Span::new(0, 2)),
            Span::new(4, 8),
            Expr::Lit(make_lit!(true, 10, 13)),
            Block::new(Span::new(15, 46), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(17, 44), 
                    FnCallExpr::new(
                        SimpleName::new(3, Span::new(17, 23)),
                        Span::new(24, 43), make_exprs![
                            make_lit!(4: str, 25, 42)
                        ]
                    )
                ))
            ])
        )
    }
}
