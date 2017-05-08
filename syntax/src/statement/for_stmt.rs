///! fff-lang
///!
///! syntax/for_stmt
///! ForStatement = [LabelDef] fFor fIdentifier fIn BinaryExpr Block 

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::KeywordKind;

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
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

    #[cfg(feature = "parse_sess")]
    fn new_some_label(
        label: Option<LabelDef>,
        for_strpos: StringPosition,
        iter_name: String, iter_strpos: StringPosition,
        iter_expr: BinaryExpr,
        body: Block) -> ForStatement {
        ForStatement{
            m_all_strpos: StringPosition::merge(match label { Some(ref label) => label.get_all_strpos(), None => for_strpos }, body.get_all_strpos()),
            m_label: label,
            m_for_strpos: for_strpos,
            m_iter_name: iter_name,
            m_iter_expr: iter_expr,
            m_body: body,
            m_ident_strpos: iter_strpos,
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
impl ISyntaxItemGrammar for ForStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        match (tokens.nth(index), tokens.nth(index + 2)) {
            (&Token::Label(_), &Token::Keyword(KeywordKind::For)) | (&Token::Keyword(KeywordKind::For), _) => true,
            _ => false
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for ForStatement {
    fn is_first_finalx(sess: &ParseSession) -> bool {
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Keyword(KeywordKind::For)) | (&Token::Keyword(KeywordKind::For), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for ForStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ForStatement>, usize) {

        let (maybe_label, mut current_length) = match tokens.nth(index) {
            &Token::Label(_) => match LabelDef::parse(tokens, messages, index) {
                (Some(label_def), _label_length_is_2) => (Some(label_def), 2),
                (None, length) => return (None, length),
            },
            _ => (None, 0),
        };

        let for_strpos = tokens.pos(index + current_length);
        current_length += 1;

        let iter_name = match tokens.nth(index + current_length) {
            &Token::Ident(ref ident) => ident.clone(),
            &Token::Keyword(KeywordKind::Underscore) => "_".to_owned(), // _ do not declare iter var
            _ => return push_unexpect!(tokens, messages, "identifier", index + current_length, current_length),
        };
        let iter_strpos = tokens.pos(index + current_length);
        current_length += 1;

        if tokens.nth(index + current_length) != &Token::Keyword(KeywordKind::In) {
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
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for ForStatement {

    fn parsex(sess: &mut ParseSession) -> ParseResult<ForStatement> {

        let maybe_label = LabelDef::try_parse(sess)?;
        let for_strpos = sess.expect_keyword(KeywordKind::For)?;

        // Accept _ as iter_name, _ do not declare iter var
        let (iter_name, iter_strpos) = sess.expect_ident_or(vec![KeywordKind::Underscore])?;

        let _in_strpos = sess.expect_keyword(KeywordKind::In)?;
        let iter_expr = BinaryExpr::parsex(sess)?;
        let body = Block::parsex(sess)?;
        return Ok(ForStatement::new_some_label(maybe_label, for_strpos, iter_name, iter_strpos, iter_expr, body));
    }
}

#[cfg(test)] #[test]
fn for_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;

    //                                       123456789012345678
    assert_eq!{ ForStatement::with_test_str_ret_messages("@2: for i in 42 {}"), (
        Some(ForStatement::new_with_label(make_strpos!(1, 1, 1, 18),
            LabelDef::new("2".to_owned(), make_strpos!(1, 1, 1, 3)),
            make_strpos!(1, 5, 1, 7),
            "i".to_owned(), make_strpos!(1, 9, 1, 9),
            BinaryExpr::new_lit(LitValue::from(42), make_strpos!(1, 14, 1, 15)),
            Block::new(make_strpos!(1, 17, 1, 18), vec![])
        )),
        make_messages![],
    )}

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