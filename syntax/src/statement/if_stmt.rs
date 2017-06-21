///! fff-lang
///!
///! syntax/if_stmt
///! TODO: make it public, update next line grammar
///! IfStatement = fIf Expr Block [fElse fIf Expr Block]* [ fElse Block ]

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

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfConditionBody {
    if_strpos: Span, // or `else if`'s strpos
    cond_expr: Expr,
    body: Block,
}
impl IfConditionBody {
    pub fn new(if_strpos: Span, cond_expr: Expr, body: Block) -> IfConditionBody { IfConditionBody{ if_strpos, cond_expr, body } }
    pub fn get_if_strpos(&self) -> Span { self.if_strpos }
    pub fn get_cond_expr(&self) -> &Expr { &self.cond_expr }
    pub fn get_body(&self) -> &Block { &self.body }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfStatement {
    base: IfConditionBody,
    elseifs: Vec<IfConditionBody>,
    else_body: Option<Block>,
    else_strpos: Span,
    all_strpos: Span,
}
impl ISyntaxItemFormat for IfStatement {
    fn format(&self, indent: u32) -> String {

        let mut retval = String::new();
        retval.push_str(&format!("{}IfStmt <{:?}>\n", IfStatement::indent_str(indent), self.all_strpos));

        let IfConditionBody{ ref if_strpos, ref cond_expr, ref body } = self.base;
        retval.push_str(&format!("{}'if' <{:?}>\n{}\n{}", 
            IfStatement::indent_str(indent + 1), if_strpos, cond_expr.format(indent + 2), body.format(indent + 2)));

        for &IfConditionBody{ if_strpos: ref else_if_strpos, ref cond_expr, ref body } in &self.elseifs {
            retval.push_str(&format!("\n{}'else if' <{:?}>\n{}\n{}", 
                IfStatement::indent_str(indent + 1), else_if_strpos, cond_expr.format(indent + 2), body.format(indent + 2)));
        }
        
        match self.else_body {
            Some(ref else_body) => 
                retval.push_str(&format!("\n{}'else' <{:?}>\n{}", IfStatement::indent_str(indent + 1), self.else_strpos, else_body.format(indent + 2))),
            None => (),
        }

        return retval;
    }
}
impl fmt::Debug for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl IfStatement {

    pub fn new_if(all_strpos: Span, if_strpos: Span, cond_expr: Expr, body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, body),
            elseifs: Vec::new(),
            else_body: None,
            else_strpos: Span::default(),
            all_strpos
        }
    }
    pub fn new_ifelse(all_strpos: Span, 
        if_strpos: Span, cond_expr: Expr, if_body: Block, 
        else_strpos: Span, else_body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs: Vec::new(),
            else_body: Some(else_body),
            else_strpos,
            all_strpos,
        }
    }
    pub fn new_ifelseif(all_strpos: Span,
        if_strpos: Span, cond_expr: Expr, if_body: Block,
        elseifs: Vec<IfConditionBody>) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs,
            else_body: None,
            else_strpos: Span::default(),
            all_strpos,
        }
    }
    pub fn new_ifelseifelse(all_strpos: Span, 
        if_strpos: Span, cond_expr: Expr, if_body: Block, 
        elseifs: Vec<IfConditionBody>,
        else_strpos: Span, else_body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs,
            else_body: Some(else_body),
            else_strpos,
            all_strpos,
        }
    }

    pub fn get_all_strpos(&self) -> Span { self.all_strpos }
    pub fn get_if_strpos(&self) -> Span { self.base.if_strpos }
    pub fn get_if_expr(&self) -> &Expr { &self.base.cond_expr }
    pub fn get_if_body(&self) -> &Block { &self.base.body }
    pub fn get_elseifs(&self) -> &Vec<IfConditionBody> { &self.elseifs }
    pub fn get_else_body(&self) -> Option<&Block> { self.else_body.as_ref() }
    pub fn get_else_strpos(&self) -> Span { self.else_strpos }
}
impl ISyntaxItemGrammar for IfStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::If) }
}
impl ISyntaxItemParse for IfStatement {
    type Target = IfStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<IfStatement> {
        assert!(sess.tk == &Token::Keyword(KeywordKind::If));

        let if_strpos = sess.pos;
        sess.move_next();
        
        let if_expr = Expr::parse(sess)?;
        let if_body = Block::parse(sess)?;

        let mut elseifs = Vec::new();
        let mut ending_strpos = if_body.all_span;
        let mut else_strpos = Span::default();
        let mut else_body = None;
        loop {
            match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                (&Token::Keyword(KeywordKind::Else), ref else_strpos,
                    &Token::Keyword(KeywordKind::If), ref if_strpos) => {
                    sess.move_next2();
                    let elseif_strpos = else_strpos.merge(&if_strpos);
                    let elseif_expr = Expr::parse(sess)?;
                    let elseif_body = Block::parse(sess)?;
                    ending_strpos = elseif_body.all_span;
                    elseifs.push(IfConditionBody::new(elseif_strpos, elseif_expr, elseif_body));
                }
                (&Token::Keyword(KeywordKind::Else), ref this_else_strpos, _, _) => {
                    sess.move_next();
                    // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                    // There is a bug fix here, now no more current_length handling!
                    // // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                    else_strpos = *this_else_strpos;
                    let this_else_body = Block::parse(sess)?;
                    ending_strpos = this_else_body.all_span;
                    else_body = Some(this_else_body);
                }
                _ => break,
            }
        }

        let all_strpos = if_strpos.merge(&ending_strpos);
        match else_body {
            Some(else_body) => Ok(IfStatement::new_ifelseifelse(all_strpos, if_strpos, if_expr, if_body, elseifs, else_strpos, else_body)),
            None => Ok(IfStatement::new_ifelseif(all_strpos, if_strpos, if_expr, if_body, elseifs)),
        }
    }
}

#[cfg(test)] #[test]
fn if_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::ExprList;
    use super::super::FnCallExpr;
    use super::super::MemberAccessExpr;
    use super::super::IdentExpr;
    use super::super::ArrayDef;
    use super::super::ExprStatement;
    use super::super::LitExpr;
    use super::super::Statement;
    use super::super::ISyntaxItemWithStr;

    //                                      0        1         2         3
    //                                      1234567890123456789012345678901234567
    assert_eq!{ IfStatement::with_test_str("if true { } else if false { } else {}"),
        IfStatement::new_ifelseifelse(make_span!(0, 36),
            make_span!(0, 1),
            Expr::Lit(LitExpr::new(LitValue::from(true), make_span!(3, 6))),
            Block::new(make_span!(8, 10), vec![]), vec![
                IfConditionBody::new(
                    make_span!(12, 18),
                    Expr::Lit(LitExpr::new(LitValue::from(false), make_span!(20, 24))),
                    Block::new(make_span!(26, 28), vec![])
                )
            ],
            make_span!(30, 33),
            Block::new(make_span!(35, 36), vec![])
        )
    }

    //                                0         1         2         3         4         5         6         7
    assert_eq!{ //                    012345678901234567890123456789012345678901234567890123456789012345678901
        IfStatement::with_test_input("if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}", 
            &mut make_symbols!["sth", "do_sth", "a", "other", "do_other", "b", "writeln", "map"]),
        IfStatement::new_ifelse(make_span!(0, 71),
            make_span!(0, 1), 
            Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(3, 3))),
            Block::new(make_span!(5, 41), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(7, 20),
                    Expr::FnCall(FnCallExpr::new(
                        MemberAccessExpr::new(
                            IdentExpr::new(make_id!(1), make_span!(7, 9)),
                            make_span!(10, 10),
                            IdentExpr::new(make_id!(2), make_span!(11, 16))
                        ),
                        make_span!(17, 19), make_exprs![
                            IdentExpr::new(make_id!(3), make_span!(18, 18))
                        ]
                    )),
                )),
                Statement::Expr(ExprStatement::new_simple(make_span!(22, 39),
                    Expr::FnCall(FnCallExpr::new(
                        MemberAccessExpr::new(
                            IdentExpr::new(make_id!(4), make_span!(22, 26)),
                            make_span!(27, 27),
                            IdentExpr::new(make_id!(5), make_span!(28, 35))
                        ),
                        make_span!(36, 38), make_exprs![
                            IdentExpr::new(make_id!(6), make_span!(37, 37))
                        ]
                    )),
                ))
            ]),
            make_span!(43, 46),
            Block::new(make_span!(48, 71), vec![
                Statement::Expr(ExprStatement::new_simple(make_span!(50, 70),
                    Expr::FnCall(FnCallExpr::new(
                        MemberAccessExpr::new(
                            ArrayDef::new(make_span!(50, 56), make_exprs![
                                LitExpr::new(LitValue::from(1), make_span!(51, 51)),
                                LitExpr::new(LitValue::from(2), make_span!(53, 53)),
                                LitExpr::new(LitValue::from(3), make_span!(55, 55)),
                            ]),
                            make_span!(57, 57),
                            IdentExpr::new(make_id!(8), make_span!(58, 60))
                        ),
                        make_span!(61, 69), make_exprs![
                            IdentExpr::new(make_id!(7), make_span!(62, 68))
                        ]
                    ))
                ))
            ])
        )
    }
}