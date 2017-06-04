///! fff-lang
///!
///! syntax/loop_stmt
///! LoopStatement = [LabelDef] fLoop Block
// TODO ATTENTION: no else for break here because if control flow come to else it is always breaked

use std::fmt;

use codemap::Span;
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
    m_loop_strpos: Span,
    m_body: Block,
}
impl ISyntaxItemFormat for LoopStatement {
    fn format(&self, indent: u32) -> String {
        match self.m_label {
            Some(ref label_def) => format!("{}LoopStmt <{:?}>\n{}\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), label_def.get_all_strpos().merge(&self.m_body.get_all_strpos()),
                label_def.format(indent + 1),
                LoopStatement::indent_str(indent + 1), self.m_loop_strpos,
                self.m_body.format(indent + 1)),
            None => format!("{}LoopStmt <{:?}>\n{}'loop' <{:?}>\n{}", 
                LoopStatement::indent_str(indent), self.m_loop_strpos.merge(&self.m_body.get_all_strpos()),
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
    
    pub fn new_no_label(loop_strpos: Span, body: Block) -> LoopStatement {
        LoopStatement {
            m_label: None,
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }
    pub fn new_with_label(label: LabelDef, loop_strpos: Span, body: Block) -> LoopStatement {
        LoopStatement {
            m_label: Some(label),
            m_loop_strpos: loop_strpos,
            m_body: body
        }
    }

    fn new_some_label(label: Option<LabelDef>, loop_strpos: Span, body: Block) -> LoopStatement {
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

    pub fn get_loop_strpos(&self) -> Span { self.m_loop_strpos }
    pub fn get_body(&self) -> &Block { &self.m_body }

    pub fn get_all_strpos(&self) -> Span {
        match self.m_label {
            Some(ref label_def) => label_def.get_all_strpos().merge(&self.m_body.get_all_strpos()),
            None => self.m_loop_strpos.merge(&self.m_body.get_all_strpos()),
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
    let expect = r#"LoopStmt <<0>0-27>
  Label '@@' <<0>0-2>
  'loop' <<0>4-7>
  Block <<0>9-27>
    ExprStmt <<0>11-25>
      PostfixExpr <<0>11-24>
        Ident 'println' <<0>11-17>
        FunctionCall <<0>18-24>
          Literal "233" <<0>19-23>"#;

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
        LoopStatement::new_no_label(make_span!(0, 3), Block::new(make_span!(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    assert_eq!{ LoopStatement::with_test_str("@@: loop { println(\"233\"); }"),
        LoopStatement::new_with_label(
            LabelDef::new("@".to_owned(), make_span!(0, 2)),
            make_span!(4, 7),
            Block::new(make_span!(9, 27), vec![
                Statement::Expr(ExprStatement::new_simple(
                    make_span!(11, 25), 
                    BinaryExpr::new_postfix(PostfixExpr::new_function_call(
                        PostfixExpr::new_primary(PrimaryExpr::new_ident("println".to_owned(), make_span!(11, 17))),
                        make_span!(18, 24), vec![
                            BinaryExpr::new_lit(LitValue::from("233".to_owned()), make_span!(19, 23))
                        ]
                    ))
                ))
            ])
        )
    }
}