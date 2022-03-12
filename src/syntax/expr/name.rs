///! fff-lang
///!
///! syntax/name, currently is
///! name = identifier { '::' identifier }
// future may support something like `to_string::<i32>(a)`

use crate::syntax::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SimpleName {
    pub value: IsId,
    pub span: Span,
}
impl ISyntaxFormat for SimpleName {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("ident-use").space().isid(self.value).space().span(self.span).finish()
    }
}
impl fmt::Debug for SimpleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl From<SimpleName> for Expr {
    fn from(ident_expr: SimpleName) -> Expr { Expr::SimpleName(ident_expr) }
}
impl SimpleName {
    pub fn new(value: impl Into<IsId>, span: Span) -> SimpleName { SimpleName{ value: value.into(), span } }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for SimpleName where F: FileSystem {
    type Output = SimpleName; // out of expr depdendencies require direct parse and get a simple name

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<SimpleName> {
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
            f = f.endl().indent1().lit("segment").space().isid(*value).space().span(*span);
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
impl ISyntaxGrammar for Name {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Ident(_)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for Name where F: FileSystem {
    type Output = Expr;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Expr> {
        
        let first_segment = SimpleName::parse(sess)?;
        let mut all_span = first_segment.span;
        let mut segments = vec![first_segment];
        
        loop {
            if let Some(_) = sess.try_expect_sep(Separator::ColonColon) {
                let segment = SimpleName::parse(sess)?;
                all_span = all_span + segment.span;
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
    use super::super::make_node;

    assert_eq!{ make_node!("hello"), 
        Expr::SimpleName(SimpleName::new(1, Span::new(0, 4)))
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    TestInput::new("std::network::wlan::native::GetWLANHandle")
        .set_syms(make_symbols!["std", "network", "wlan", "native", "GetWLANHandle"])
        .apply::<Name, _>()
        .expect_no_message()
        .expect_result(Expr::Name(Name::new(Span::new(0, 40), vec![
            SimpleName::new(1, Span::new(0, 2)), 
            SimpleName::new(2, Span::new(5, 11)),
            SimpleName::new(3, Span::new(14, 17)),
            SimpleName::new(4, Span::new(20, 25)),
            SimpleName::new(5, Span::new(28, 40)),
        ])))
    .finish();
}