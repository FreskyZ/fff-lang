///! syntax::name:
///! name = identifier { '::' identifier }
///! should be
///! name = name_segment { '::' name_segment }
///! name_segment = identifier | '<' type_ref { ',' type_ref } '>'

use super::prelude::*;

// #[cfg_attr(test, derive(PartialEq))]
// #[derive(Debug)]
// pub enum NameSegment {
//     Normal(IsId, Span),
//     Generic(Vec<TypeRef>, Span),
// }

// impl Node for NameSegment {

//     fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
//         v.visit_name_segment(self)
//     }
//     fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
//         match self {
//             Self::Generic(types, _) => for r#type in types {
//                 v.visit_type_ref(r#type)?;
//             },
//             _ => {},
//         }
//         Ok(Default::default())
//     }
// }

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct SimpleName {
    pub value: IsId,
    pub span: Span,
}

impl Parser for SimpleName {
    type Output = SimpleName; // out of expr depdendencies require direct parse and get a simple name

    fn parse(cx: &mut ParseContext) -> Result<SimpleName, Unexpected> {
        let (value, span) = cx.expect_ident()?;
        Ok(SimpleName{ value, span })
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
    type Output = Self;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Ident(_)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {
        
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
        
        Ok(Name::new(all_span, segments))
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

#[cfg(test)]
macro_rules! make_name {
    (simple $start:literal:$end:literal #$id:literal) => (
        make_name!($start:$end make_name!(segment $start:$end #$id)));
    ($start:literal:$end:literal $($segment:expr),+$(,)?) => (
        crate::syntax::Expr::Name(crate::syntax::Name{ all_span: Span::new($start, $end), segments: vec![$($segment,)+] }));
    (segment $start:literal:$end:literal #$id:literal) => (
        crate::syntax::SimpleName{ value: IsId::new($id), span: Span::new($start, $end) });
    (segment generic $start:literal:$end:literal #$id:literal) => (
        crate::syntax::SimpleName{ value: IsId::new($id), span: Span::new($start, $end) /* TODO */ });
    // bare version for use outside of expr
    (bare $start:literal:$end:literal $($segment:expr),+$(,)?) => (
        crate::syntax::Name{ all_span: Span::new($start, $end), segments: vec![$($segment,)+] });
    (simple bare $start:literal:$end:literal #$id:literal) => (
        make_name!(bare $start:$end make_name!(segment $start:$end #$id)));
}
#[cfg(test)]
pub(crate) use make_name;

#[cfg(test)]
#[test]
fn name_parse() {

    case!{ "hello" as Name, 
        make_name!(bare 0:4 make_name!(segment 0:4 #2)),
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    case!{ "std::network::wlan::native::GetWLANHandle" as Name,
        make_name!(bare 0:40
            make_name!(segment 0:2 #2), 
            make_name!(segment 5:11 #3),
            make_name!(segment 14:17 #4),
            make_name!(segment 20:25 #5),
            make_name!(segment 28:40 #6))
    }
}
