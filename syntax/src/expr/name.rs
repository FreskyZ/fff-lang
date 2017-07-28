///! fff-lang
///!
///! syntax/name, currently is
///! name = identifier { '::' identifier }
// future may support something like `to_string::<i32>(a)`

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::Seperator;

use super::Expr;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SimpleName {
    pub value: SymbolID,
    pub span: Span,
}
impl ISyntaxFormat for SimpleName {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("ident-use").space().sym(self.value).space().span(self.span).finish()
    }
}
impl fmt::Debug for SimpleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<SimpleName> for Expr {
    fn from(ident_expr: SimpleName) -> Expr { Expr::SimpleName(ident_expr) }
}
impl SimpleName {
    pub fn new(value: SymbolID, span: Span) -> SimpleName { SimpleName{ value, span } }
}
impl ISyntaxItemParse for SimpleName {
    type Target = SimpleName; // out of expr depdendencies require direct parse and get a simple name

    fn parse(sess: &mut ParseSession) -> ParseResult<SimpleName> {
        let (value, span) = sess.expect_ident()?;
        Ok(SimpleName::new(value, span))
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Name {
    pub segments: Vec<SimpleName>,
    pub all_span: Span,
}
impl ISyntaxFormat for Name {
    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("name").space().span(self.all_span);
        for &SimpleName{ ref value, ref span } in &self.segments {
            f = f.endl().indent1().lit("segment").space().sym(*value).space().span(*span);
        }
        f.finish()
    }
}
impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl From<Name> for Expr {
    fn from(name: Name) -> Expr { Expr::Name(name) }
}
impl Name {
    pub fn new(all_span: Span, segments: Vec<SimpleName>) -> Name { Name{ all_span, segments } }
}
impl ISyntaxItemGrammar for Name {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Ident(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for Name {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        let first_segment = SimpleName::parse(sess)?;
        let mut all_span = first_segment.span;
        let mut segments = vec![first_segment];
        
        loop {
            if let Some(_) = sess.try_expect_sep(Seperator::NamespaceSeperator) {
                let segment = SimpleName::parse(sess)?;
                all_span = all_span.merge(&segment.span);
                segments.push(segment);
            } else {
                break;
            }
        }
        
        if segments.len() > 1 {
            Ok(Expr::Name(Name::new(all_span, segments)))
        } else {
            Ok(Expr::SimpleName(segments.pop().unwrap()))
        }
    }
}

impl Expr {

    /// into name from result of Name::parse, panic when not Name neither SimpleName, crate internal
    pub(crate) fn into_name(self) -> Name {
        match self {
            Expr::Name(name) => name,
            Expr::SimpleName(simple_name) => Name::new(simple_name.span, vec![simple_name]),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)] #[test]
fn name_parse() {
    use codemap::SymbolCollection;
    use super::super::WithTestInput;
    use super::super::TestInput;

    assert_eq!{ Name::with_test_str("hello"), 
        Expr::SimpleName(SimpleName::new(make_id!(1), make_span!(0, 4)))
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    TestInput::new("std::network::wlan::native::GetWLANHandle")
        .set_syms(make_symbols!["std", "network", "wlan", "native", "GetWLANHandle"])
        .apply::<Name, _>()
        .expect_no_message()
        .expect_result(Expr::Name(Name::new(make_span!(0, 40), vec![
            SimpleName::new(make_id!(1), make_span!(0, 2)), 
            SimpleName::new(make_id!(2), make_span!(5, 11)),
            SimpleName::new(make_id!(3), make_span!(14, 17)),
            SimpleName::new(make_id!(4), make_span!(20, 25)),
            SimpleName::new(make_id!(5), make_span!(28, 40)),
        ])))
    .finish();
}