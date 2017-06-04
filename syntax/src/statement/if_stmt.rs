///! fff-lang
///!
///! syntax/if_stmt
///! IfStatement = fIf BinaryExpr Block [fElse fIf BinaryExpr Block]* [ fElse Block ]

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

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfConditionBody {
    if_strpos: Span, // or `else if`'s strpos
    cond_expr: BinaryExpr,
    body: Block,
}
impl IfConditionBody {
    pub fn new(if_strpos: Span, cond_expr: BinaryExpr, body: Block) -> IfConditionBody { IfConditionBody{ if_strpos, cond_expr, body } }
    pub fn get_if_strpos(&self) -> Span { self.if_strpos }
    pub fn get_cond_expr(&self) -> &BinaryExpr { &self.cond_expr }
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

    pub fn new_if(all_strpos: Span, if_strpos: Span, cond_expr: BinaryExpr, body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, body),
            elseifs: Vec::new(),
            else_body: None,
            else_strpos: Span::default(),
            all_strpos
        }
    }
    pub fn new_ifelse(all_strpos: Span, 
        if_strpos: Span, cond_expr: BinaryExpr, if_body: Block, 
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
        if_strpos: Span, cond_expr: BinaryExpr, if_body: Block,
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
        if_strpos: Span, cond_expr: BinaryExpr, if_body: Block, 
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
    pub fn get_if_expr(&self) -> &BinaryExpr { &self.base.cond_expr }
    pub fn get_if_body(&self) -> &Block { &self.base.body }
    pub fn get_elseifs(&self) -> &Vec<IfConditionBody> { &self.elseifs }
    pub fn get_else_body(&self) -> Option<&Block> { self.else_body.as_ref() }
    pub fn get_else_strpos(&self) -> Span { self.else_strpos }
}
impl ISyntaxItemGrammar for IfStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::If) }
}
impl ISyntaxItemParse for IfStatement {

    fn parse(sess: &mut ParseSession) -> ParseResult<IfStatement> {
        assert!(sess.tk == &Token::Keyword(KeywordKind::If));

        let if_strpos = sess.pos;
        sess.move_next();
        
        let if_expr = BinaryExpr::parse(sess)?;
        let if_body = Block::parse(sess)?;

        let mut elseifs = Vec::new();
        let mut ending_strpos = if_body.get_all_strpos();
        let mut else_strpos = Span::default();
        let mut else_body = None;
        loop {
            match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                (&Token::Keyword(KeywordKind::Else), ref else_strpos,
                    &Token::Keyword(KeywordKind::If), ref if_strpos) => {
                    sess.move_next2();
                    let elseif_strpos = else_strpos.merge(&if_strpos);
                    let elseif_expr = BinaryExpr::parse(sess)?;
                    let elseif_body = Block::parse(sess)?;
                    ending_strpos = elseif_body.get_all_strpos();
                    elseifs.push(IfConditionBody::new(elseif_strpos, elseif_expr, elseif_body));
                }
                (&Token::Keyword(KeywordKind::Else), ref this_else_strpos, _, _) => {
                    sess.move_next();
                    // 17/5/6: When there is match Block::parse(tokens, messages, index + current_length), etc.
                    // There is a bug fix here, now no more current_length handling!
                    // // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                    else_strpos = *this_else_strpos;
                    let this_else_body = Block::parse(sess)?;
                    ending_strpos = this_else_body.get_all_strpos();
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
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;

    //                                      0        1         2         3
    //                                      1234567890123456789012345678901234567
    assert_eq!{ IfStatement::with_test_str("if true { } else if false { } else {}"),
        IfStatement::new_ifelseifelse(make_span!(0, 36),
            make_span!(0, 1),
            BinaryExpr::new_lit(LitValue::from(true), make_span!(3, 6)),
            Block::new(make_span!(8, 10), vec![]), vec![
                IfConditionBody::new(
                    make_span!(12, 18),
                    BinaryExpr::new_lit(LitValue::from(false), make_span!(20, 24)),
                    Block::new(make_span!(26, 28), vec![])
                )
            ],
            make_span!(30, 33),
            Block::new(make_span!(35, 36), vec![])
        )
    }

//     perrorln!("{:?}", IfStatement::with_test_str("if 1 { fresky.love(zmj); zmj.love(fresky); }"));
//     perrorln!("{:?}", IfStatement::with_test_str("if 1 { fresky.love(zmj); zmj.love(fresky); } else { writeln(\"hellworld\"); }"));
//     perrorln!("{:?}", IfStatement::with_test_str(
// r#"
//         if 1 { 
//             fresky.love(zmj); 
//             zmj.love(fresky); 
//         } else if false {
//             1 + 1 = 2;
//         } else if abc * defg == hij {
//             keywords.remove("def");
//             use_in_test("def");
//         } else { 
//             writeln("hellworld"); 
//         }"#
//     ));
}