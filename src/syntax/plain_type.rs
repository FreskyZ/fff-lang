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
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_as_segment(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
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
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_segment(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
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
    pub global: bool,
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
                    cx.expect_sep(Separator::Comma)?;
                    parameters.push(cx.expect::<TypeRef>()?);
                };
                segments.push(TypeSegment{ ident, ident_span, quote_span, parameters, all_span: ident_span + quote_span });
            } else {
                segments.push(TypeSegment{ ident, ident_span, quote_span: Span::new(0, 0), parameters: Vec::new(), all_span: ident_span });
            }
            if cx.try_expect_sep(Separator::ColonColon).is_none() {
                break;
            }
        }

        let global = type_as_segment.is_none() && beginning_separator_span.is_some();
        let all_span = type_as_segment.as_ref().map(|s| s.span)
            .or_else(|| beginning_separator_span)
            .unwrap_or_else(|| segments[0].all_span) + segments.last().unwrap().all_span; // [0] and last().unwrap(): matches() guarantees segments are not empty
        Ok(PlainType{ type_as_segment, global, segments, all_span })
    }
}

impl Node for PlainType {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_plain_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(type_as_segment) = &self.type_as_segment {
            v.visit_type_as_segment(type_as_segment)?;
        }
        for segment in &self.segments {
            v.visit_type_segment(segment)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)]
macro_rules! make_type {
    (prim $kw:ident, $start:expr, $end:expr) => (
        crate::syntax::TypeRef::Primitive(crate::syntax::PrimitiveType{ name: Keyword::$kw, span: Span::new($start, $end) }));
    (ref $start:literal:$end:literal $inner:expr) => (
        crate::syntax::TypeRef::Ref(crate::syntax::RefType{ span: Span::new($start, $end), base: Box::new($inner) }));
    (array $start:expr, $end:expr, $base:expr, $size:expr) => (
        crate::syntax::TypeRef::Array(crate::syntax::ArrayType{ base: Box::new($base), size: $size, span: Span::new($start, $end) }));
    (tuple $start:expr, $end:expr, [$($item:expr),*$(,)?]) => (
        crate::syntax::TypeRef::Tuple(crate::syntax::TupleType{ items: vec![$($item,)*], span: Span::new($start, $end) }));
    (segment $start:literal:$end:literal $ident:literal) => (crate::syntax::TypeSegment{ 
        ident: IsId::new($ident), 
        ident_span: Span::new($start, $end), 
        quote_span: Span::new(0, 0),
        parameters: Vec::new(),
        all_span: Span::new($start, $end),
    });
    (segment generic $start:literal:$end:literal $ident:literal $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),+$(,)?) => (crate::syntax::TypeSegment{
        ident: IsId::new($ident),
        ident_span: Span::new($ident_start, $ident_end),
        quote_span: Span::new($quote_start, $quote_end),
        parameters: vec![$($parameter,)+],
        all_span: Span::new($start, $end),
    });
    (segment as $start:literal:$end:literal $from:expr, $to:expr) => (Some(crate::syntax::TypeAsSegment{
        from: Box::new($from),
        to: Box::new($to),
        span: Span::new($start, $end),
    }));
    (plain $start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (crate::syntax::TypeRef::Plain(crate::syntax::PlainType{
        type_as_segment: $as,
        global: $global,
        segments: vec![$($segment,)*],
        all_span: Span::new($start, $end),
    }));
    (simple $start:literal:$end:literal $id:literal) => (
        make_type!(plain $start:$end false, None, make_type!(segment $start:$end $id)));
    (fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?]) => (crate::syntax::TypeRef::Fn(crate::syntax::FnType{
        paren_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: None,
        all_span: Span::new($start, $end),
    }));
    (fn ret $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?], $ret:expr) => (crate::syntax::TypeRef::Fn(crate::syntax::FnType{
        paren_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: Some(Box::new($ret)),
        all_span: Span::new($start, $end),
    }));
    (param $start:expr, $end:expr, $ty:expr) => (crate::syntax::FnTypeParam{
        name: None,
        r#type: $ty,
        all_span: Span::new($start, $end),
    });
    (param named $start:literal:$end:literal $name:literal $name_start:literal:$name_end:literal $ty:expr) => (crate::syntax::FnTypeParam{
        name: Some((IsId::new($name), Span::new($name_start, $name_end))),
        r#type: $ty,
        all_span: Span::new($start, $end),
    });
}
#[cfg(test)]
pub(crate) use make_type;

#[cfg(test)] 
#[test]
fn type_ref_parse() {

    case!{ "i32" as TypeRef,
        make_type!(prim I32, 0, 2),
    }

    case!{ "custom_type" as TypeRef,
        make_type!(simple 0:10 2),
    }

    case!{ "[i32; 5]" as TypeRef, 
        make_type!(array 0, 7, 
            make_type!(prim I32, 1, 3), 
            make_lit!(5, 6, 6).into()),
    }

    //      0         1         2
    //      01234567890123456789012345
    case!{ "[[a;1]; 1 + 1 * 1 - 1 / 1]" as TypeRef,
        make_type!(array 0, 25, 
            make_type!(array 1, 5,
                make_type!(simple 2:2 2),
                make_expr!(1: i32, 4, 4)),
            make_expr!(binary 8, 24, Sub, 18, 18, 
                make_expr!(binary 8, 16, Add, 10, 10,
                    make_expr!(1: i32, 8, 8),
                    make_expr!(binary 12, 16, Mul, 14, 14,
                        make_expr!(1: i32, 12, 12),
                        make_expr!(1: i32, 16, 16))),
                make_expr!(binary 20, 24, Div, 22, 22,
                    make_expr!(1: i32, 20, 20),
                    make_expr!(1: i32, 24, 24)))),
    }

    case!{ "()" as TypeRef, 
        make_type!(tuple 0, 1, [])
    }

    case!{ "(i32, i32)" as TypeRef, 
        make_type!(tuple 0, 9, [
            make_type!(prim I32, 1, 3),
            make_type!(prim I32, 6, 8),
        ]),
    }

    case!{ "(abc, def)" as TypeRef, 
        make_type!(tuple 0, 9, [
            make_type!(simple 1:3 2),
            make_type!(simple 6:8 3),
        ]),
    }

    case!{ "(string,)" as TypeRef, 
        make_type!(tuple 0, 8, [
            make_type!(simple 1:6 2),
        ]),
    }

    case!{ "(i32)" as TypeRef,
        make_type!(tuple 0, 4, [
            make_type!(prim I32, 1, 3),
        ]), errors make_errors!(e: e.emit(strings::SingleItemTupleType).detail(Span::new(4, 4), strings::TupleTypeExpectCommaMeetRightParen)),
    }

    case!{ "fn()" as TypeRef,
        make_type!(fn 0:3 paren 2:3 []),
    }

    //      01234567890123456789
    case!{ "fn() -> Result<T, E>" as TypeRef,
        make_type!(fn ret 0:19 paren 2:3 [],
            make_type!(plain 8:19 false, None,
                make_type!(segment generic 8:19 2 8:13 quote 14:19 
                    make_type!(simple 15:15 3),
                    make_type!(simple 18:18 4)))),
    }

    //      0          1          2          3
    //      0123 456789012345 678901234567 89012345678 9
    case!{ "fn(\nthis: This,\nself: Self,\nthat: That,\n)" as TypeRef, 
        make_type!(fn 0:40 paren 2:40 [
            make_type!(param named 4:13 3 4:7 make_type!(simple 10:13 2)),
            make_type!(param named 16:25 5 16:19 make_type!(simple 22:25 4)),
            make_type!(param named 28:37 6 28:31 make_type!(simple 34:37 7)),
        ]), strings ["This", "this", "Self", "self", "that", "That"],
    }

    //      0         1         2         3
    //      01234567890123456789012345678901234
    case!{ "fn(argc: i32, argv: &string) -> i32" as TypeRef, 
        make_type!(fn ret 0:34 paren 2:27 [
            make_type!(param named 3:11 2 3:6 make_type!(prim I32, 9, 11)),
            make_type!(param named 14:26 3 14:17 make_type!(ref 20:26 make_type!(simple 21:26 4)))],
            make_type!(prim I32, 32, 34)),
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ "ffc::syntax::plain_type::type_ref_parse" as TypeRef, 
        make_type!(plain 0:38 false, None,
            make_type!(segment 0:2 2),
            make_type!(segment 5:10 3),
            make_type!(segment 13:22 4),
            make_type!(segment 25:38 5))
    }

    //      0         1         2         3
    //      012345678901234567890123456789012345678
    case!{ "::ffc::syntax::plain_type::type_ref_parse" as TypeRef, 
        make_type!(plain 0:40 true, None,
            make_type!(segment 2:4 2),
            make_type!(segment 7:12 3),
            make_type!(segment 15:24 4),
            make_type!(segment 27:40 5))
    }

    //      0         1         2         3         4         5         6
    //      012345678901234567890123456789012345678901234567890123456789012345678
    case!{ "<::ffc::syntax::PlainType as ffc::syntax::prelude::Parser<F>>::Output" as TypeRef,
        make_type!(plain 0:68 false,
            make_type!(segment as 0:60
                make_type!(plain 1:24 true, None,
                    make_type!(segment 3:5 2),
                    make_type!(segment 8:13 3),
                    make_type!(segment 16:24 4)),
                make_type!(plain 29:59 false, None,
                    make_type!(segment 29:31 2),
                    make_type!(segment 34:39 3),
                    make_type!(segment 42:48 5),
                    make_type!(segment generic 51:59 6 51:56 quote 57:59
                        make_type!(simple 58:58 7)))),
            make_type!(segment 63:68 8)),
    }


    // ffc::syntax::prelude::ast_test_case
    // <T as alloc::string::ToString>::to_string
    // <ffc::syntax::prelude::node_display::NodeDisplay<N,F> as core::fmt::Display>::fmt
    // <ffc::syntax::fn_def::FnDef as ffc::syntax::prelude::Node>::accept
    // ffc::syntax::prelude::node_display::FormatVisitor<F>::invoke_walk
    // <ffc::syntax::prelude::node_display::FormatVisitor<F> as ffc::syntax::prelude::Visitor<(),core::fmt::Error>>::visit_fn_param

    // case!{ "u8" as TypeRef, TypeRef::new_simple(2, Span::new(0, 1)) }
    // case!{ "i32" as TypeRef, TypeRef::new_simple(2, Span::new(0, 2)) }
    // case!{ "char" as TypeRef, TypeRef::new_simple(2, Span::new(0, 3)) }
    // case!{ "string" as TypeRef, TypeRef::new_simple(2, Span::new(0, 5)) }
    // case!{ "helloworld_t" as TypeRef, TypeRef::new_simple(2, Span::new(0, 11)) }

    // case!{"()" as TypeRef, TypeRef::new_simple(2, Span::new(0, 1)) };

    // // 123456789012345678901234567890123456
    // case!{ "([char], i32, u17, [((), u8, f129)])" as TypeRef,
    //     TypeRef::new_template(9, Span::new(0, 0), Span::new(0, 35), vec![
    //         TypeRef::new_template(3, Span::new(0, 0), Span::new(1, 6), vec![
    //             TypeRef::new_simple(2, Span::new(2, 5)),
    //         ]),
    //         TypeRef::new_simple(5, Span::new(9, 11)),
    //         TypeRef::new_simple(4, Span::new(14, 16)),
    //         TypeRef::new_template(3, Span::new(0, 0), Span::new(19, 34), vec![
    //             TypeRef::new_template(9, Span::new(0, 0), Span::new(20, 33), vec![
    //                 TypeRef::new_simple(6, Span::new(21, 22)),
    //                 TypeRef::new_simple(8, Span::new(25, 26)),
    //                 TypeRef::new_simple(7, Span::new(29, 32)),
    //             ]),
    //         ]),
    //     ]), strings ["char", "array", "u17", "i32", "unit", "f129", "u8", "tuple"]
    // }

    // // Auto generated mixed
    // //               0        1         2
    // //               12345678901234567890123
    // case!{ "((i8, clL, Kopu), f64,)" as TypeRef,
    //     TypeRef::new_template(5, Span::new(0, 0), Span::new(0, 22), vec![
    //         TypeRef::new_template(5, Span::new(0, 0), Span::new(1, 15), vec![
    //             TypeRef::new_simple(3, Span::new(2, 3)),
    //             TypeRef::new_simple(2, Span::new(6, 8)),
    //             TypeRef::new_simple(4, Span::new(11, 14))
    //         ]), 
    //         TypeRef::new_simple(6, Span::new(18, 20))
    //     ]), strings ["clL", "i8", "Kopu", "tuple", "f64"]
    // } 
    
    // //             12345678
    // case!{ "[BJlbk4]" as TypeRef,
    //     TypeRef::new_template(3, Span::new(0, 0), Span::new(0, 7), vec![
    //         TypeRef::new_simple(2, Span::new(1, 6)),
    //     ])
    // }
            
    // //         0        1         2         3         4         5         6         7         8         9
    // //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    // case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)" as TypeRef,
    //     TypeRef::new_template(10, Span::new(0, 0), Span::new(0, 93), vec![
    //         TypeRef::new_template(10, Span::new(0, 0), Span::new(1, 76), vec![
    //             TypeRef::new_simple(3, Span::new(2, 5)), 
    //             TypeRef::new_simple(2, Span::new(8, 9)),
    //             TypeRef::new_template(10, Span::new(0, 0), Span::new(12, 74), vec![
    //                 TypeRef::new_template(10, Span::new(0, 0), Span::new(13, 59), vec![
    //                     TypeRef::new_simple(4, Span::new(14, 15)),
    //                    TypeRef::new_template(7, Span::new(0, 0), Span::new(18, 53), vec![
    //                         TypeRef::new_template(10, Span::new(0, 0), Span::new(19, 52), vec![
    //                             TypeRef::new_template(7, Span::new(0, 0), Span::new(20, 26), vec![
    //                                 TypeRef::new_simple(5, Span::new(21, 25))
    //                             ]),
    //                             TypeRef::new_template(7, Span::new(0, 0), Span::new(29, 33), vec![
    //                                 TypeRef::new_simple(6, Span::new(30, 32))
    //                             ]),
    //                             TypeRef::new_template(10, Span::new(0, 0), Span::new(36, 51), vec![
    //                                 TypeRef::new_template(7, Span::new(0, 0), Span::new(37, 44), vec![
    //                                     TypeRef::new_simple(8, Span::new(38, 43))
    //                                 ]), 
    //                                 TypeRef::new_simple(9, Span::new(47, 49))
    //                             ])
    //                         ])
    //                     ]),
    //                     TypeRef::new_simple(11, Span::new(56, 58))
    //                 ]),
    //                 TypeRef::new_template(7, Span::new(0, 0), Span::new(62, 68), vec![
    //                     TypeRef::new_simple(12, Span::new(63, 67))
    //                 ]),
    //                 TypeRef::new_simple(13, Span::new(71, 73))
    //             ])
    //         ]),
    //         TypeRef::new_template(7, Span::new(0, 0), Span::new(79, 87), vec![
    //             TypeRef::new_simple(14, Span::new(80, 86))
    //         ]),
    //         TypeRef::new_simple(15, Span::new(90, 92)),
    //         //        2     3       4     5        6      7        8         9      10       11     12       13     14         15
    //     ]), strings ["jq", "char", "u8", "o2fcd", "CKw", "array", "eCDn2I", "u16", "tuple", "i16", "pxplh", "u32", "vrud2vC", "u64"]
    // }
    
    // case!{ "sxM4" as TypeRef, TypeRef::new_simple(2, Span::new(0, 3)) }
    
    // //               0        1         2
    // //               12345678901234567890123
    // case!{ "([pwi], [u64], i33, i8)" as TypeRef,
    //     TypeRef::new_template(7, Span::new(0, 0), Span::new(0, 22), vec![
    //         TypeRef::new_template(3, Span::new(0, 0), Span::new(1, 5), vec![
    //             TypeRef::new_simple(2, Span::new(2, 4))
    //         ]), 
    //         TypeRef::new_template(3, Span::new(0, 0), Span::new(8, 12), vec![
    //             TypeRef::new_simple(4, Span::new(9, 11))
    //         ]),
    //         TypeRef::new_simple(5, Span::new(15, 17)),
    //         TypeRef::new_simple(6, Span::new(20, 21))
    //     ]), strings ["pwi", "array", "u64", "i33", "i8", "tuple"]
    // }
}
