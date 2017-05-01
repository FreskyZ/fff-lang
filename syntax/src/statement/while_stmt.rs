///! fff-lang
///!
///! syntax/while_stmt
///! WhileStatement = LabelDef fWhile BinaryExpr Block

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::BinaryExpr;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct WhileStatement {
    label_def: Option<LabelDef>,
    loop_expr: BinaryExpr,
    body: Block,
    while_strpos: StringPosition,
    all_strpos: StringPosition,
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl WhileStatement {
    
    pub fn new_no_label(all_strpos: StringPosition, while_strpos: StringPosition, loop_expr: BinaryExpr, body: Block) -> WhileStatement {
        WhileStatement{ label_def: None, loop_expr, body, while_strpos, all_strpos }
    }
    pub fn new_with_label(all_strpos: StringPosition, label_def: LabelDef, while_strpos: StringPosition, loop_expr: BinaryExpr, body: Block) -> WhileStatement {
        WhileStatement{ label_def: Some(label_def), loop_expr, body, while_strpos, all_strpos }
    }

    pub fn get_label(&self) -> Option<&LabelDef> { self.label_def.as_ref() }
    pub fn get_loop_expr(&self) -> &BinaryExpr { &self.loop_expr }
    pub fn get_body(&self) -> &Block { &self.body }
    pub fn get_while_strpos(&self) -> StringPosition { self.while_strpos }

    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}
impl ISyntaxItem for WhileStatement {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() }

    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        (tokens.nth(index).is_label() && tokens.nth(index + 2).is_keyword(KeywordKind::While)) || tokens.nth(index).is_keyword(KeywordKind::While)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<WhileStatement>, usize) {

        let (maybe_label, mut current_length) = match tokens.nth(index).get_label() {
            Some(_) => match LabelDef::parse(tokens, messages, index) {
                (Some(label_def), _label_length_is_2) => (Some(label_def), 2),
                (None, length) => return (None, length),
            },
            None => (None, 0),
        };

        let while_strpos = tokens.pos(index + current_length);
        current_length += 1;

        let expr = match BinaryExpr::parse(tokens, messages, index + current_length) {
            (Some(expr), expr_len) => { current_length += expr_len; expr }
            (None, length) => return (None, current_length + length),
        };

        let body = match Block::parse(tokens, messages, index + current_length) {
            (Some(block), block_len) => { current_length += block_len; block }
            (None, length) => return (None, current_length + length),
        };

        let all_strpos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_length));
        match maybe_label {
            Some(label) => (Some(WhileStatement::new_with_label(all_strpos, label, while_strpos, expr, body)), current_length),
            None => (Some(WhileStatement::new_no_label(all_strpos, while_strpos, expr, body)), current_length),
        }
    }
}

#[cfg(test)] #[test]
fn while_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;

    //                                         0        1         2         3         4        
    //                                         1234567890123456789012345678901234567890123456789
    assert_eq!{ WhileStatement::with_test_str("@2: while true { writeln(\"fresky loves zmj\"); }"),
        WhileStatement::new_with_label(
            make_strpos!(1, 1, 1, 49),
            LabelDef::new("2".to_owned(), make_strpos!(1, 1, 1, 2)),
            make_strpos!(1, 5, 1, 9),
            BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 11, 1, 14)),
            Block::new(make_strpos!(1, 16, 1, 49), vec![
                // TODO: fill it
            ])
        )
    }
}
