///! fff-lang
///!
///! syntax/for_stmt
///! ForStatement = [LabelDef] fFor fIdentifier fIn BinaryExpr Block 

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::BinaryExpr;
use super::super::LabelDef;
use super::super::Block;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ForStatement {
    m_label: Option<LabelDef>,
    m_iter_name: String,
    m_iter_expr: BinaryExpr,
    m_body: Block,
    m_for_strpos: StringPosition,
    m_ident_strpos: StringPosition,
    m_all_strpos: StringPosition,
}
impl ISyntaxItemFormat for ForStatement {
    fn format(&self, indent: u32) -> String {
        format!("{}ForStmt <{:?}>{}\n{}'for' <{:?}>\n{}Ident '{}' <{:?}>\n{}\n{}", 
            ForStatement::indent_str(indent), self.m_all_strpos,
            match self.m_label { Some(ref label_def) => format!("\n{}", label_def.format(indent + 1)), None => "".to_owned() },
            ForStatement::indent_str(indent + 1), self.m_for_strpos,
            ForStatement::indent_str(indent + 1), self.m_iter_name, self.m_ident_strpos,
            self.m_iter_expr.format(indent + 1),
            self.m_body.format(indent + 1),
        )
    }
}
impl fmt::Debug for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl ForStatement {

    pub fn new_no_label(
        all_strpos: StringPosition, 
        for_strpos: StringPosition, 
        iter_name: String, iter_strpos: StringPosition,
        iter_expr: BinaryExpr, 
        body: Block) -> ForStatement {
        ForStatement {
            m_label: None, 
            m_iter_name: iter_name,
            m_iter_expr: iter_expr,
            m_body: body,
            m_for_strpos: for_strpos,
            m_ident_strpos: iter_strpos,
            m_all_strpos: all_strpos
        }
    }
    pub fn new_with_label(
        all_strpos: StringPosition, 
        label: LabelDef,
        for_strpos: StringPosition,
        iter_name: String, iter_strpos: StringPosition,
        iter_expr: BinaryExpr, 
        body: Block) -> ForStatement {
        ForStatement {
            m_label: Some(label), 
            m_iter_name: iter_name,
            m_iter_expr: iter_expr,
            m_body: body,
            m_for_strpos: for_strpos,
            m_ident_strpos: iter_strpos,
            m_all_strpos: all_strpos
        }
    }

    pub fn get_label(&self) -> Option<&LabelDef> { self.m_label.as_ref() }
    pub fn get_iter_name(&self) -> &String { &self.m_iter_name }
    pub fn get_iter_expr(&self) -> &BinaryExpr { &self.m_iter_expr }
    pub fn get_body(&self) -> &Block { &self.m_body }

    pub fn get_iter_strpos(&self) -> StringPosition { self.m_ident_strpos }
    pub fn get_for_strpos(&self) -> StringPosition { self.m_for_strpos }
    pub fn get_all_strpos(&self) ->StringPosition { self.m_all_strpos }
}
impl ISyntaxItem for ForStatement {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() }

    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        (tokens.nth(index).is_label() && tokens.nth(index + 2).is_keyword(KeywordKind::For)) && tokens.nth(index).is_keyword(KeywordKind::For)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ForStatement>, usize) {

        let (maybe_label, mut current_length) = match tokens.nth(index).get_label() {
            Some(_) => match LabelDef::parse(tokens, messages, index) {
                (Some(label_def), _label_length_is_2) => (Some(label_def), 2),
                (None, length) => return (None, length),
            },
            None => (None, 0),
        };

        let for_strpos = tokens.pos(index + current_length);
        current_length += 1;

        let iter_name = match tokens.nth(index + current_length).get_identifier() {
            None => if tokens.nth(index + current_length).is_keyword(KeywordKind::Underscore) {
                "_".to_owned()   // _ do not declare iter var
            } else {
                return push_unexpect!(tokens, messages, "identifier", index + current_length, current_length);
            },
            Some(ident) => ident.clone(),
        };
        let iter_strpos = tokens.pos(index + current_length);
        current_length += 1;

        if !tokens.nth(index + current_length).is_keyword(KeywordKind::In) {
            return push_unexpect!(tokens, messages, "keyword in", index + current_length, current_length);
        }
        current_length += 1;

        let iter_expr = match BinaryExpr::parse(tokens, messages, index + current_length) {
            (Some(expr), expr_len) => { current_length += expr_len; expr }
            (None, length) => return (None, current_length + length),
        };

        let body = match Block::parse(tokens, messages, index + current_length) {
            (Some(block), block_len) => { current_length += block_len; block }
            (None, length) => return (None, current_length + length),
        };

        let all_strpos = StringPosition::merge(tokens.pos(index), body.get_all_strpos());
        match maybe_label {
            Some(label) => (Some(ForStatement::new_with_label(all_strpos, label, for_strpos, iter_name, iter_strpos, iter_expr, body)), current_length),
            None => (Some(ForStatement::new_no_label(all_strpos, for_strpos, iter_name, iter_strpos, iter_expr, body)), current_length)
        }
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;

    //                                       123456789012345678
    assert_eq!{ ForStatement::with_test_str("@2: for i in 42 {}"), 
        ForStatement::new_with_label(make_strpos!(1, 1, 1, 18),
            LabelDef::new("2".to_owned(), make_strpos!(1, 1, 1, 3)),
            make_strpos!(1, 5, 1, 7),
            "i".to_owned(), make_strpos!(1, 9, 1, 9),
            BinaryExpr::new_lit(LitValue::from(42), make_strpos!(1, 14, 1, 15)),
            Block::new(make_strpos!(1, 17, 1, 18), vec![])
        )
    }

    // TODO: finish this
    // assert_eq!{ ForStatement::with_test_str("@hello:  for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }"),
    //     ForStatement::new_with_label(make_strpos!(1, 1, 1, 1), 
    //         LabelDef::new("hello".to_owned(), make_strpos!(1, 1, 1, 1)),
    //         make_strpos!(1, 1, 1, 1),
    //         "_".to_owned(), make_strpos!(1, 1, 1, 1),
    //         BinaryExpr::new_lit(LitValue::from(42), make_strpos!(1, 1, 1, 1)),
    //         Block::new(make_strpos!(1, 1, 1, 1), vec![])
    //     )
    // }
}