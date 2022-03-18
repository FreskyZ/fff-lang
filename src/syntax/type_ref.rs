///! syntax::type_ref:
///! type_ref = primitive_type | identifier | '[' type_ref ']' | '(' type_ref ',' { type_ref ',' } [ type_ref ] ')'
///! should be
///! type_ref = 
///!    | primitive_type 
///!    | '[' type_ref ';' expr ']' 
///!    | '(' type_ref ',' { type_ref ',' } [ type_ref ] ')' 
///!    | 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
///!    | type_ref_segment { '::' type_ref_segment }
///!    | '&' type_ref
///! type_ref_segment = identifier | identifier '<' type_parameter { ',' type_parameter } '>'
///! type_parameter = type_ref | expr
///!
///! - this means primitive type, array and tuple special syntax will not be part of path,
///!   only identifier or type-parameterized identifier can be part of double colon separated path
///! - according to current implementation,
///!   - fn syntax and `fn<paramtype1, paramtype2, rettype>` is exactly same
///!   - tuple syntax and `tuple<type1, type2>` is exactly same,
///!     without making `tuple` a keyword, which also mean a custom type cannot be called tuple
///!   - array syntax and `array<itemtype, size>` is exactly same,
///!     without making `array` a keyword, which also mean plain numeric value should be allowed in type list

use super::prelude::*;
// use super::{Expr, LitExpr, LitValue};

// pub struct PrimitiveTypeRef {
//     pub keyword: Keyword,
//     pub span: Span,
// }

// pub struct ArrayTypeRef {
//     pub item: Box<TypeRef>,
//     pub size: Expr,
//     pub span: Span,
// }

// pub struct TupleTypeRef {
//     pub items: Vec<TypeRef>,
//     pub span: Span,
// }

// pub struct FunctionTypeRefParam {
//     pub name: Option<IsId>,
//     pub name_span: Span,
//     pub r#type: TypeRef,
// }

// pub struct FunctionTypeRef {
//     pub parameters: Vec<FunctionTypeRefParam>,
//     pub ret_type: Option<Box<TypeRef>>,
//     pub span: Span,
// }

// pub struct ReferenceTypeRef {
//     pub base: Box<TypeRef>,
//     pub span: Span,
// }

// pub struct TypeRefSegment {
//     pub base: IsId,
//     pub parameters: Vec<TypeRef>,
//     pub span: Span,
// }

// pub struct TypeRefSegments {
//     pub segments: Vec<TypeRefSegment>,
//     pub span: Span,
// }

// pub enum TypeRef {
//     Primitive(PrimitiveTypeRef),
//     Array(ArrayTypeRef),
//     Tuple(TupleTypeRef),
//     Function(FunctionTypeRef),
//     Reference(ReferenceTypeRef),
//     Segments(TypeRefSegments),
// }

// #[cfg_attr(test, derive(PartialEq))]
// #[derive(Debug)]
// pub struct TypeRefSegment {
//     // primitive is interned as isid and no type parameter and one segment
//     pub base: IsId,
//     // all span = base span + quote span, 
//     // so base span for array and tuple is quote span start
//     pub base_span: Span,
//     // and quote span is end of base span if non generic segment
//     pub quote_span: Span,
//     pub parameters: Vec<TypeRef>,
// }

// impl TypeRefSegment {

//     pub fn new_simple(name: impl Into<IsId>, span: Span) -> Self {
//         Self{ base: name.into(), base_span: span, quote_span: span.end.into(), parameters: Vec::new() }
//     }
//     pub fn new_generic(base: impl Into<IsId>, base_span: Span, quote_span: Span, parameters: Vec<TypeRef>) -> Self {
//         Self{ base: base.into(), base_span, quote_span, parameters }
//     }
// }

// impl Node for TypeRefSegment {
//     type ParseOutput = TypeRefSegment;

//     fn matches(current: &Token) -> bool {
//         match current {
//             &Token::Ident(_) 
//             | &Token::Sep(Separator::LeftBracket) 
//             | &Token::Sep(Separator::LeftParen) => true,
//             &Token::Keyword(kw) => kw.kind(KeywordKind::Primitive),
//             _ => false,
//         }
//     }

//     fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        
//         if let Some(left_bracket_span) = cx.try_expect_sep(Separator::LeftBracket) {
//             let inner = cx.expect_node::<TypeRef>()?;
//             let _semicolon_span = cx.expect_sep(Separator::SemiColon)?;
//             let expr = cx.expect_node::<Expr>()?;
//             let size_type = if let Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::I32(size)), span: size_span }) = &expr {
//                 let size_id = cx.intern(&format!("{}", size));
//                 TypeRef{ all_span: *size_span, segments: vec![TypeRefSegment::new_simple(size_id, *size_span)] }
//             } else {
//                 let expr_span = expr.get_all_span();
//                 cx.emit(strings::InvalidArraySize).span(expr_span).help(strings::ArraySizeHelp);
//                 TypeRef{ all_span: expr_span, segments: vec![TypeRefSegment::new_simple(IsId::from(1), expr_span)] }
//             };
//             let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
//             TypeRefSegment::new_generic(cx.intern("array"), left_bracket_span, left_bracket_span, vec![inner, size_type])
//         } else if let Some(left_paren_span) = cx.try_expect_sep(Separator::LeftParen) {
            
//             if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
//                 return Ok(Self::new_simple(cx.intern("unit"), left_paren_span + right_paren_span));
//             }
            
//             let mut tuple_types = vec![cx.expect_node::<TypeRef>()?];
//             let (ending_span, end_by_comma) = loop {
//                 if let Some((_comma_span, right_paren_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
//                     break (right_paren_span, true);
//                 } else if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
//                     break (right_paren_span, false);
//                 }
//                 let _comma_span = cx.expect_sep(Separator::Comma)?;
//                 tuple_types.push(cx.expect_node::<TypeRef>()?);
//             };
                
//             let paren_span = left_paren_span + ending_span;
//             if tuple_types.len() == 1 && !end_by_comma {            // len == 0 already rejected
//                 cx.emit("Single item tuple type use").detail(paren_span, "type use here");
//             }
//             Ok(Self::new_template(cx.intern("tuple"), Span::new(0, 0), paren_span, tuple_types))
//         }

//         Err(())
//     }
// }

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TypeRef {
    pub base: IsId,
    pub base_span: Span,        // maybe default for array and tuple
    pub quote_span: Span,       // span of `()` or `[]` or (unimplemented)`<>`
    pub params: Vec<TypeRef>,
    pub all_span: Span, 
}
impl TypeRef {

    pub fn new_simple(base: impl Into<IsId>, base_span: Span) -> Self {
        Self{ all_span: base_span, params: Vec::new(), quote_span: Span::new(0, 0), base: base.into(), base_span }
    }
    pub fn new_template(base: impl Into<IsId>, base_span: Span, quote_span: Span, params: Vec<Self>) -> Self {
        Self{ all_span: base_span + quote_span, base: base.into(), base_span, quote_span, params }
    }
}

impl Node for TypeRef {
    type ParseOutput = Self;

    fn matches(current: &Token) -> bool {
        match current {
            &Token::Ident(_) | &Token::Sep(Separator::LeftBracket) | &Token::Sep(Separator::LeftParen) => true,
            &Token::Keyword(kw) => kw.kind(KeywordKind::Primitive),
            _ => false,
        }
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {

        if let Some(left_bracket_span) = cx.try_expect_sep(Separator::LeftBracket) {
            let inner = cx.expect_node::<TypeRef>()?;
            let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
            Ok(Self::new_template(cx.intern("array"), Span::new(0, 0), left_bracket_span + right_bracket_span, vec![inner]))
        } else if let Some(left_paren_span) = cx.try_expect_sep(Separator::LeftParen) {
            if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
                return Ok(Self::new_simple(cx.intern("unit"), left_paren_span + right_paren_span));
            }
            
            let mut tuple_types = vec![cx.expect_node::<TypeRef>()?];
            let (ending_span, end_by_comma) = loop {
                if let Some((_comma_span, right_paren_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                    break (right_paren_span, true);
                } else if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
                    break (right_paren_span, false);
                }
                let _comma_span = cx.expect_sep(Separator::Comma)?;
                tuple_types.push(cx.expect_node::<TypeRef>()?);
            };
                
            let paren_span = left_paren_span + ending_span;
            if tuple_types.len() == 1 && !end_by_comma {            // len == 0 already rejected
                cx.emit("Single item tuple type use").detail(paren_span, "type use here");
            }
            Ok(Self::new_template(cx.intern("tuple"), Span::new(0, 0), paren_span, tuple_types))
        } else {
            let (symid, sym_span) = cx.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
            Ok(Self::new_simple(symid, sym_span))
        }
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for child in &self.params {
            v.visit_type_ref(child)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn type_ref_parse() {

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