
///! fff-lang
///!
///! syntax/if_stmt
///! IfStatement = fIf BinaryExpr Block [fElse fIf BinaryExpr Block]* [ fElse Block ]

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::KeywordKind;

use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::BinaryExpr;
use super::super::Block;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfConditionBody {
    if_strpos: StringPosition, // or `else if`'s strpos
    cond_expr: BinaryExpr,
    body: Block,
}
impl IfConditionBody {
    pub fn new(if_strpos: StringPosition, cond_expr: BinaryExpr, body: Block) -> IfConditionBody { IfConditionBody{ if_strpos, cond_expr, body } }
    pub fn get_if_strpos(&self) -> StringPosition { self.if_strpos }
    pub fn get_cond_expr(&self) -> &BinaryExpr { &self.cond_expr }
    pub fn get_body(&self) -> &Block { &self.body }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct IfStatement {
    base: IfConditionBody,
    elseifs: Vec<IfConditionBody>,
    else_body: Option<Block>,
    else_strpos: StringPosition,
    all_strpos: StringPosition,
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

    pub fn new_if(all_strpos: StringPosition, if_strpos: StringPosition, cond_expr: BinaryExpr, body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, body),
            elseifs: Vec::new(),
            else_body: None,
            else_strpos: StringPosition::new(),
            all_strpos
        }
    }
    pub fn new_ifelse(all_strpos: StringPosition, 
        if_strpos: StringPosition, cond_expr: BinaryExpr, if_body: Block, 
        else_strpos: StringPosition, else_body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs: Vec::new(),
            else_body: Some(else_body),
            else_strpos,
            all_strpos,
        }
    }
    pub fn new_ifelseif(all_strpos: StringPosition,
        if_strpos: StringPosition, cond_expr: BinaryExpr, if_body: Block,
        elseifs: Vec<IfConditionBody>) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs,
            else_body: None,
            else_strpos: StringPosition::new(),
            all_strpos,
        }
    }
    pub fn new_ifelseifelse(all_strpos: StringPosition, 
        if_strpos: StringPosition, cond_expr: BinaryExpr, if_body: Block, 
        elseifs: Vec<IfConditionBody>,
        else_strpos: StringPosition, else_body: Block) -> IfStatement {
        IfStatement {
            base: IfConditionBody::new(if_strpos, cond_expr, if_body),
            elseifs,
            else_body: Some(else_body),
            else_strpos,
            all_strpos,
        }
    }

    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
    pub fn get_if_strpos(&self) -> StringPosition { self.base.if_strpos }
    pub fn get_if_expr(&self) -> &BinaryExpr { &self.base.cond_expr }
    pub fn get_if_body(&self) -> &Block { &self.base.body }
    pub fn get_elseifs(&self) -> &Vec<IfConditionBody> { &self.elseifs }
    pub fn get_else_body(&self) -> Option<&Block> { self.else_body.as_ref() }
    pub fn get_else_strpos(&self) -> StringPosition { self.else_strpos }
}
impl ISyntaxItemGrammar for IfStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_keyword(KeywordKind::If)
    }
}
impl ISyntaxItemParse for IfStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<IfStatement>, usize) {

        if !tokens.nth(index).is_keyword(KeywordKind::If) {
            unreachable!()
        }

        let mut current_length = 1;
        let if_strpos = tokens.pos(index);
        
        let if_expr = match BinaryExpr::parse(tokens, messages, index + current_length) {
            (Some(expr), expr_len) => { current_length += expr_len; expr }
            (None, length) => return (None, current_length + length),
        };
        let if_body = match Block::parse(tokens, messages, index + current_length) {
            (Some(block), block_len) => { current_length += block_len; block }
            (None, length) => return (None, current_length + length),
        };

        let mut elseifs = Vec::new();
        let mut else_strpos = StringPosition::new();
        let mut else_body = None;
        loop {
            match (tokens.nth(index + current_length).is_keyword(KeywordKind::Else), tokens.nth(index + current_length + 1).is_keyword(KeywordKind::If)) {
                (true, true) => {
                    let elseif_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1));
                    current_length += 2;
                    let elseif_expr = match BinaryExpr::parse(tokens, messages, index + current_length) {
                        (Some(expr), expr_len) => { current_length += expr_len; expr }
                        (None, length) => return (None, current_length + length),
                    };
                    let elseif_body = match Block::parse(tokens, messages, index + current_length) {
                        (Some(block), block_len) => { current_length += block_len; block }
                        (None, length) => return (None, current_length + length),
                    };
                    elseifs.push(IfConditionBody::new(elseif_strpos, elseif_expr, elseif_body));
                }
                (true, false) => {
                    else_strpos = tokens.pos(index + current_length);
                    match Block::parse(tokens, messages, index + current_length + 1) { 
                        (Some(block), block_len) => {
                            else_body = Some(block);
                            current_length += block_len + 1;
                        }  // 16/12/1, we lost TWO `+1`s for current_length here ... fixed
                        (None, length) => return (None, current_length + 1 + length),
                    }
                }
                (false, _) => {
                    break;
                }
            }
        }

        let all_strpos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length - 1));
        match else_body {
            Some(else_body) => (Some(IfStatement::new_ifelseifelse(all_strpos, if_strpos, if_expr, if_body, elseifs, else_strpos, else_body)), current_length),
            None => (Some(IfStatement::new_ifelseif(all_strpos, if_strpos, if_expr, if_body, elseifs)), current_length),
        }
    }
}

#[cfg(test)] #[test]
fn ast_stmt_if() {
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;

    //                                      0        1         2         3
    //                                      1234567890123456789012345678901234567
    assert_eq!{ IfStatement::with_test_str("if true { } else if false { } else {}"),
        IfStatement::new_ifelseifelse(make_strpos!(1, 1, 1, 37),
            make_strpos!(1, 1, 1, 2),
            BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 4, 1, 7)),
            Block::new(make_strpos!(1, 9, 1, 11), vec![]), vec![
                IfConditionBody::new(
                    make_strpos!(1, 13, 1, 19),
                    BinaryExpr::new_lit(LitValue::from(false), make_strpos!(1, 21, 1, 25)),
                    Block::new(make_strpos!(1, 27, 1, 29), vec![])
                )
            ],
            make_strpos!(1, 31, 1, 34),
            Block::new(make_strpos!(1, 36, 1, 37), vec![])
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