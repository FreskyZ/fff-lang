///! fff-lang
///!
///! syntax/loop_stmt
///! LoopStatement = [LabelDef] fLoop Block

use std::fmt;

use codepos::StringPosition;
use lexical::Token;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Block;
use super::super::LabelDef;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LoopStatement {
    m_label: Option<LabelDef>,
    m_loop_strpos: StringPosition,
    m_body: Block,
}
impl ISyntaxItemFormat for LoopStatement {
    fn format(&self, indent: u32) -> String {
        match self.m_label {
            Some(ref label_def) => format!("{}LoopStmt <{:?}>\n{}\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), StringPosition::merge(label_def.get_all_strpos(), self.m_body.get_all_strpos()),
                label_def.format(indent + 1),
                LoopStatement::indent_str(indent + 1), self.m_loop_strpos,
                self.m_body.format(indent + 1)),
            None => format!("{}LoopStmt <{:?}>\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), StringPosition::merge(self.m_loop_strpos, self.m_body.get_all_strpos()),
                LoopStatement::indent_str(indent + 1), self.m_loop_strpos,
                self.m_body.format(indent + 1)),
        }
    }
}
impl fmt::Debug for LoopStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}
impl LoopStatement { // New
    
    pub fn new_no_label(loop_strpos: StringPosition, body: Block) -> LoopStatement {
        LoopStatement {
            m_label: None,
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }
    pub fn new_with_label(label: LabelDef, loop_strpos: StringPosition, body: Block) -> LoopStatement {
        LoopStatement {
            m_label: Some(label),
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }

    fn new_some_label(label: Option<LabelDef>, loop_strpos: StringPosition, body: Block) -> LoopStatement {
        LoopStatement{  
            m_label: label,
            m_loop_strpos: loop_strpos,
            m_body: body,
        }
    }
}
impl LoopStatement { // Get

    pub fn has_label(&self) -> bool { self.m_label.is_some() }
    pub fn get_label(&self) -> Option<&LabelDef> { self.m_label.as_ref() }

    pub fn get_loop_strpos(&self) -> StringPosition { self.m_loop_strpos }
    pub fn get_body(&self) -> &Block { &self.m_body }

    pub fn get_all_strpos(&self) -> StringPosition {
        match self.m_label {
            Some(ref label_def) => StringPosition::merge(label_def.get_all_strpos(), self.m_body.get_all_strpos()),
            None => StringPosition::merge(self.m_loop_strpos, self.m_body.get_all_strpos()),
        }
    }

    // TODO: remove this temp for make codegen pass compile
    pub fn into_body(self) -> Block {
        self.m_body
    }
}
impl ISyntaxItemGrammar for LoopStatement {
    fn is_first_final(sess: &ParseSession) -> bool {
        match (sess.tk, sess.nextnext_tk) {
            (&Token::Label(_), &Token::Keyword(KeywordKind::Loop)) | (&Token::Keyword(KeywordKind::Loop), _) => true,
            _ => false
        }
    }
}
impl ISyntaxItemParse for LoopStatement {

    fn parse(sess: &mut ParseSession) -> ParseResult<LoopStatement> {

        let maybe_label = LabelDef::try_parse(sess)?;
        let loop_strpos = sess.expect_keyword(KeywordKind::Loop)?;
        let body = Block::parse(sess)?;
        return Ok(LoopStatement::new_some_label(maybe_label, loop_strpos, body));
    }
}

#[cfg(test)] #[test]
fn loop_stmt_format() {
    use super::super::ISyntaxItemWithStr;
    
    //                                         1234567890123456789 0123 45678
    let actual = LoopStatement::with_test_str("@@: loop { println(\"233\"); }").format(0);
    let expect = r#"LoopStmt <<0>1:1-1:28>
  Label '@@' <<0>1:1-1:3>
  'loop' <<0>1:5-1:8>
  Block <<0>1:10-1:28>
    ExprStmt <<0>1:12-1:26>
      PostfixExpr <<0>1:12-1:25>
        Ident 'println' <<0>1:12-1:18>
        FunctionCall <<0>1:19-1:25>
          Literal "233" <<0>1:20-1:24>"#;

    if actual != expect { panic!("assertion failed: left: {}, right: {}", actual, expect) }
}

#[cfg(test)] #[test]
fn loop_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use super::super::Statement;
    use super::super::ExprStatement;
    use super::super::BinaryExpr;
    use super::super::PostfixExpr;
    use super::super::PrimaryExpr;
    use lexical::LitValue;

    assert_eq!{ LoopStatement::with_test_str("loop {}"),
        LoopStatement::new_no_label(make_strpos!(1, 1, 1, 4), Block::new(make_strpos!(1, 6, 1, 7), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    assert_eq!{ LoopStatement::with_test_str("@@: loop { println(\"233\"); }"),
        LoopStatement::new_with_label(
            LabelDef::new("@".to_owned(), make_strpos!(1, 1, 1, 3)),
            make_strpos!(1, 5, 1, 8),
            Block::new(make_strpos!(1, 10, 1, 28), vec![
                Statement::Expr(ExprStatement::new_simple(
                    make_strpos!(1, 12, 1, 26), 
                    BinaryExpr::new_postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::new_ident("println".to_owned(), make_strpos!(1, 12, 1, 18))),
                        make_strpos!(1, 19, 1, 25), vec![
                            BinaryExpr::new_lit(LitValue::from("233".to_owned()), make_strpos!(1, 20, 1, 24))
                        ]
                    ))
                ))
            ])
        )
    }
}