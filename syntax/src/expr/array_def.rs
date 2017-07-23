///! fff-lang
///!
///! array_def = '[' [ expr_list ] ']'

use std::fmt;

use codemap::Span;
use message::Message;
use lexical::Token;
use lexical::Seperator;

use super::Expr;
use super::ExprList;
use super::ExprListParseResult;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::error_strings;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ArrayDef {
    pub items: ExprList,
    pub bracket_span: Span,
}
impl ISyntaxItemFormat for ArrayDef {
    fn format(&self, f: Formatter) -> String {
        format!("{}ArrayDef <{}>\n{}", f.indent(), f.span(self.bracket_span),
            if self.items.items.len() == 0 { format!("{}(empty)", f.indent1()) } else { f.apply1(&self.items) }
        )
    }
}
impl fmt::Debug for ArrayDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::default())) }
}
impl From<ArrayDef> for Expr {
    fn from(array_def: ArrayDef) -> Expr { Expr::Array(array_def) }
}
impl ArrayDef {
    pub fn new(bracket_span: Span, items: ExprList) -> ArrayDef { ArrayDef{ bracket_span, items: items } }
}
impl ISyntaxItemGrammar for ArrayDef {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(Seperator::LeftBracket) }
}
impl ISyntaxItemParse for ArrayDef {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => {
                return Ok(Expr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(span, error_strings::ArrayDefHere)]));
                return Ok(Expr::Array(ArrayDef::new(span, ExprList::new(Vec::new()))));
            }
            ExprListParseResult::Normal(span, exprlist) | ExprListParseResult::EndWithComma(span, exprlist) => {
                return Ok(Expr::Array(ArrayDef::new(span, exprlist)));
            }
        }
    }
}

#[cfg(test)] #[test]
fn array_def_format() {
    use lexical::LitValue;
    use super::LitExpr;

    assert_eq!{
        ArrayDef::new(make_span!(0, 42), make_exprs![]).format(Formatter::with_test_indent(1)),
        "  ArrayDef <<0>0-42>\n    (empty)"
    }

    assert_eq!{
        ArrayDef::new(make_span!(0, 42), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 2)),
            LitExpr::new(LitValue::from(2), make_span!(3, 4)),
            LitExpr::new(LitValue::from(48), make_span!(5, 6)),
        ]).format(Formatter::with_test_indent(1)),
        "  ArrayDef <<0>0-42>\n    Literal (i32)1 <<0>1-2>\n    Literal (i32)2 <<0>3-4>\n    Literal (i32)48 <<0>5-6>"
    }
}

#[cfg(test)] #[test]
fn array_def_parse() {
    use lexical::LitValue;
    use super::LitExpr;
    use super::BinaryExpr;
    use super::IdentExpr;
    use super::super::WithTestInput;

    assert_eq!{ ArrayDef::with_test_str("[a]"),
        Expr::Array(ArrayDef::new(make_span!(0, 2), make_exprs![
            IdentExpr::new(make_id!(1), make_span!(1, 1))
        ]))
    }

    //                                   01234567
    assert_eq!{ ArrayDef::with_test_str("[1, '2']"),
        Expr::Array(ArrayDef::new(make_span!(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 1)),
            LitExpr::new(LitValue::from('2'), make_span!(4, 6))
        ]))
    }
    //                                   01234567
    assert_eq!{ ArrayDef::with_test_str("[1 + 1,]"),
        Expr::Array(ArrayDef::new(make_span!(0, 7), make_exprs![
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(1, 1)), 
                Seperator::Add, make_span!(3, 3),
                LitExpr::new(LitValue::from(1), make_span!(5, 5)),
            )
        ]))
    }
}

#[cfg(test)] #[test]
fn array_def_errors() {
    use message::MessageCollection;
    use super::super::TestInput;
    
    TestInput::new("[ , ]")
        .apply::<ArrayDef, _>()
        .expect_result(Expr::Array(ArrayDef::new(make_span!(0, 4), make_exprs![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(0, 4), error_strings::ArrayDefHere)])
        ])
    .finish();
}