///! syntax::name:
///! name = identifier { '::' identifier }
///! should be
///! name = name_segment { '::' name_segment }
///! name_segment = identifier | '<' type_ref { ',' type_ref } '>'

use super::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct SimpleName {
    pub value: IsId,
    pub span: Span,
}

impl SimpleName {
    pub fn new(value: impl Into<IsId>, span: Span) -> SimpleName { 
        SimpleName{ value: value.into(), span } 
    }
}

impl Parser for SimpleName {
    type Output = SimpleName; // out of expr depdendencies require direct parse and get a simple name

    fn parse(cx: &mut ParseContext) -> Result<SimpleName, Unexpected> {
        let (value, span) = cx.expect_ident()?;
        Ok(SimpleName::new(value, span))
    }
}

impl Node for SimpleName {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_name(self)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct Name {
    pub segments: Vec<SimpleName>,
    pub all_span: Span,
}

impl Name {
    pub fn new(all_span: Span, segments: Vec<SimpleName>) -> Name { 
        Name{ all_span, segments } 
    }
}

impl Parser for Name {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Ident(_)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        let first_segment = cx.expect::<SimpleName>()?;
        let mut all_span = first_segment.span;
        let mut segments = vec![first_segment];
        
        loop {
            if let Some(_) = cx.try_expect_sep(Separator::ColonColon) {
                let segment = cx.expect::<SimpleName>()?;
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

impl Node for Name {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_name(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for segment in &self.segments {
            v.visit_simple_name(segment)?;
        }
        Ok(Default::default())
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

    case!{ "hello" as Name, 
        Expr::SimpleName(SimpleName::new(2, Span::new(0, 4)))
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    case!{ "std::network::wlan::native::GetWLANHandle" as Name,
        Expr::Name(Name::new(Span::new(0, 40), vec![
            SimpleName::new(2, Span::new(0, 2)), 
            SimpleName::new(3, Span::new(5, 11)),
            SimpleName::new(4, Span::new(14, 17)),
            SimpleName::new(5, Span::new(20, 25)),
            SimpleName::new(6, Span::new(28, 40)),
        ]))
    }
}
