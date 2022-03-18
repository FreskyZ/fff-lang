///! syntax::plain_type
///! plain_type = [ type_as_segment | '::' ] plain_type_segment { '::' plain_type_segment }
///! type_as_segment = '<' type_ref 'as' type_ref '>' '::'
///! plain_type_segment = identifier [ '<' type_ref { ',' type_ref } [ ',' ] '>' ]
///!
///! most common type ref, plain means not special (array/tuple/fn) and not referenced (not directly a reference type)
///! may be namespaced, segment may contain type parameter, does not need namespace separator `::` before type list angle bracket pair
///! may contain a type_as_segment at beginning
///! may contain a namespace separator at beginning, for referencing global items

use super::prelude::*;
use super::{TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TypeAsSegment {
    pub from: Box<TypeRef>,
    pub to: Box<TypeRef>,
    pub span: Span,
}

impl Node for TypeAsSegment {
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_as_segment(self)
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(self.from.as_ref())?;
        v.visit_type_ref(self.to.as_ref())
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TypeSegment {
    pub ident: IsId,
    pub ident_span: Span,
    pub quote_span: Span,
    pub parameters: Vec<TypeRef>,
    pub all_span: Span,
}

impl Node for TypeSegment {
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_segment(self)
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for parameter in &self.parameters {
            v.visit_type_ref(parameter)?;
        }
        Ok(Default::default())
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct PlainType {
    pub type_as_segment: Option<TypeAsSegment>,
    pub segments: Vec<TypeSegment>,
    pub all_span: Span,
}

impl Parser for PlainType {
    type Output = Self;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::Lt | Separator::ColonColon) | Token::Ident(_))
    }

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {

        let type_as_segment = cx.try_expect_sep(Separator::Lt).map(|lt_span| {
            let from = cx.expect::<TypeRef>()?;
            cx.expect_keyword(Keyword::As)?;
            let to = cx.expect::<TypeRef>()?;
            let gt_span = cx.expect_sep(Separator::Gt)?;
            Ok(TypeAsSegment{ from: Box::new(from), to: Box::new(to), span: lt_span + gt_span })
        }).transpose()?;

        let beginning_separator_span = cx.try_expect_sep(Separator::ColonColon);

        let mut segments = Vec::new();
        while let Some((ident, ident_span)) = cx.try_expect_ident() {

            if let Some(lt_span) = cx.try_expect_sep(Separator::Lt) {
                let mut parameters = vec![cx.expect::<TypeRef>()?];
                let quote_span = lt_span + loop {
                    if let Some((_, gt_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::Gt) {
                        break gt_span;
                    } else if let Some(gt_span) = cx.try_expect_sep(Separator::Gt) {
                        break gt_span;
                    }
                    parameters.push(cx.expect::<TypeRef>()?);
                    cx.expect_sep(Separator::Comma)?;
                };
                segments.push(TypeSegment{ ident, ident_span, quote_span, parameters, all_span: ident_span + quote_span });
            } else {
                segments.push(TypeSegment{ ident, ident_span, quote_span: Span::new(0, 0), parameters: Vec::new(), all_span: ident_span });
            }
        }

        let all_span = type_as_segment.as_ref().map(|s| s.span)
            .or_else(|| beginning_separator_span)
            .unwrap_or_else(|| segments.last().unwrap().all_span); // last.unwrap: matches() guarantees segments are not empty
        Ok(PlainType{ type_as_segment, segments, all_span })
    }
}

impl Node for PlainType {

    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_plain_type(self)
    }
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(type_as_segment) = &self.type_as_segment {
            v.visit_type_as_segment(type_as_segment)?;
        }
        for segment in &self.segments {
            v.visit_type_segment(segment)?;
        }
        Ok(Default::default())
    }
}

#[cfg(feature = "todo")]
#[cfg(test)] 
#[test]
fn type_ref_parse() {

    case!{ "[i32; 5]" as ArrayType, ArrayType{
        base: Box::new(TypeRef::new_simple(2, Span::new(1, 3))),
        size: make_lit!(5, 6, 6).into(),
        span: Span::new(0, 7),
    }}
    // case!{ "[[a;1]; 1 + 1 * 1 - 1 / 1]" as ArrayType, ArrayType{
    //     span: Span::new()
    // }}

    case!{ "u8" as TypeRef, TypeRef::new_simple(2, Span::new(0, 1)) }
    case!{ "i32" as TypeRef, TypeRef::new_simple(2, Span::new(0, 2)) }
    case!{ "char" as TypeRef, TypeRef::new_simple(2, Span::new(0, 3)) }
    case!{ "string" as TypeRef, TypeRef::new_simple(2, Span::new(0, 5)) }
    case!{ "helloworld_t" as TypeRef, TypeRef::new_simple(2, Span::new(0, 11)) }

    case!{"()" as TypeRef, TypeRef::new_simple(2, Span::new(0, 1)) };

    case!{ "[u8]" as TypeRef,
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 3), vec![
                TypeRef::new_simple(2, Span::new(1, 2))
        ])
    }

    case!{ "[[he_t]]" as TypeRef,
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 7), vec![
            TypeRef::new_template(3, Span::new(0, 0), Span::new(1, 6), vec![
                TypeRef::new_simple(2, Span::new(2, 5))
            ])
        ]), strings ["he_t", "array"]
    }

    case!{ "(i32,)" as TypeRef, 
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 5), vec![
            TypeRef::new_simple(2, Span::new(1, 3)),
        ])
    }

    case!{ "(i32, string)" as TypeRef,
        TypeRef::new_template(4, Span::new(0, 0), Span::new(0, 12), vec![
            TypeRef::new_simple(2, Span::new(1, 3)),
            TypeRef::new_simple(3, Span::new(6, 11)),
        ])
    }

    //  12345678901234
    case!{ "(char, hw_t, )" as TypeRef,
        TypeRef::new_template(4, Span::new(0, 0), Span::new(0, 13), vec![
            TypeRef::new_simple(3, Span::new(1, 4)),
            TypeRef::new_simple(2, Span::new(7, 10)),
        ]) 
    }

    // 123456789012345678901234567890123456
    case!{ "([char], i32, u17, [((), u8, f129)])" as TypeRef,
        TypeRef::new_template(9, Span::new(0, 0), Span::new(0, 35), vec![
            TypeRef::new_template(3, Span::new(0, 0), Span::new(1, 6), vec![
                TypeRef::new_simple(2, Span::new(2, 5)),
            ]),
            TypeRef::new_simple(5, Span::new(9, 11)),
            TypeRef::new_simple(4, Span::new(14, 16)),
            TypeRef::new_template(3, Span::new(0, 0), Span::new(19, 34), vec![
                TypeRef::new_template(9, Span::new(0, 0), Span::new(20, 33), vec![
                    TypeRef::new_simple(6, Span::new(21, 22)),
                    TypeRef::new_simple(8, Span::new(25, 26)),
                    TypeRef::new_simple(7, Span::new(29, 32)),
                ]),
            ]),
        ]), strings ["char", "array", "u17", "i32", "unit", "f129", "u8", "tuple"]
    }

    //             1234567
    case!{ "(i233,)" as TypeRef,
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 6), vec![
            TypeRef::new_simple(2, Span::new(1, 4))
        ])
    }

    case!{ "(i32)" as TypeRef,
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 4), vec![
            TypeRef::new_simple(2, Span::new(1, 3)),
        ]), errors make_errors!{
            e: e.emit("Single item tuple type use").detail(Span::new(0, 4), "type use here"),
        }
    }

    // Auto generated mixed
    //               0        1         2
    //               12345678901234567890123
    case!{ "((i8, clL, Kopu), f64,)" as TypeRef,
        TypeRef::new_template(5, Span::new(0, 0), Span::new(0, 22), vec![
            TypeRef::new_template(5, Span::new(0, 0), Span::new(1, 15), vec![
                TypeRef::new_simple(3, Span::new(2, 3)),
                TypeRef::new_simple(2, Span::new(6, 8)),
                TypeRef::new_simple(4, Span::new(11, 14))
            ]), 
            TypeRef::new_simple(6, Span::new(18, 20))
        ]), strings ["clL", "i8", "Kopu", "tuple", "f64"]
    } 
    
    //             12345678
    case!{ "[BJlbk4]" as TypeRef,
        TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 7), vec![
            TypeRef::new_simple(2, Span::new(1, 6)),
        ])
    }
            
    //         0        1         2         3         4         5         6         7         8         9
    //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)" as TypeRef,
        TypeRef::new_template(10, Span::new(0, 0), Span::new(0, 93), vec![
            TypeRef::new_template(10, Span::new(0, 0), Span::new(1, 76), vec![
                TypeRef::new_simple(3, Span::new(2, 5)), 
                TypeRef::new_simple(2, Span::new(8, 9)),
                TypeRef::new_template(10, Span::new(0, 0), Span::new(12, 74), vec![
                    TypeRef::new_template(10, Span::new(0, 0), Span::new(13, 59), vec![
                        TypeRef::new_simple(4, Span::new(14, 15)),
                       TypeRef::new_template(7, Span::new(0, 0), Span::new(18, 53), vec![
                            TypeRef::new_template(10, Span::new(0, 0), Span::new(19, 52), vec![
                                TypeRef::new_template(7, Span::new(0, 0), Span::new(20, 26), vec![
                                    TypeRef::new_simple(5, Span::new(21, 25))
                                ]),
                                TypeRef::new_template(7, Span::new(0, 0), Span::new(29, 33), vec![
                                    TypeRef::new_simple(6, Span::new(30, 32))
                                ]),
                                TypeRef::new_template(10, Span::new(0, 0), Span::new(36, 51), vec![
                                    TypeRef::new_template(7, Span::new(0, 0), Span::new(37, 44), vec![
                                        TypeRef::new_simple(8, Span::new(38, 43))
                                    ]), 
                                    TypeRef::new_simple(9, Span::new(47, 49))
                                ])
                            ])
                        ]),
                        TypeRef::new_simple(11, Span::new(56, 58))
                    ]),
                    TypeRef::new_template(7, Span::new(0, 0), Span::new(62, 68), vec![
                        TypeRef::new_simple(12, Span::new(63, 67))
                    ]),
                    TypeRef::new_simple(13, Span::new(71, 73))
                ])
            ]),
            TypeRef::new_template(7, Span::new(0, 0), Span::new(79, 87), vec![
                TypeRef::new_simple(14, Span::new(80, 86))
            ]),
            TypeRef::new_simple(15, Span::new(90, 92)),
            //        2     3       4     5        6      7        8         9      10       11     12       13     14         15
        ]), strings ["jq", "char", "u8", "o2fcd", "CKw", "array", "eCDn2I", "u16", "tuple", "i16", "pxplh", "u32", "vrud2vC", "u64"]
    }
    
    case!{ "sxM4" as TypeRef, TypeRef::new_simple(2, Span::new(0, 3)) }
    
    //               0        1         2
    //               12345678901234567890123
    case!{ "([pwi], [u64], i33, i8)" as TypeRef,
        TypeRef::new_template(7, Span::new(0, 0), Span::new(0, 22), vec![
            TypeRef::new_template(3, Span::new(0, 0), Span::new(1, 5), vec![
                TypeRef::new_simple(2, Span::new(2, 4))
            ]), 
            TypeRef::new_template(3, Span::new(0, 0), Span::new(8, 12), vec![
                TypeRef::new_simple(4, Span::new(9, 11))
            ]),
            TypeRef::new_simple(5, Span::new(15, 17)),
            TypeRef::new_simple(6, Span::new(20, 21))
        ]), strings ["pwi", "array", "u64", "i33", "i8", "tuple"]
    }
}
