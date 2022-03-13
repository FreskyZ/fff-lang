///! fff-lang
///! 
///! syntax/typeuse
///! type_use = primitive_type | identifier | '[' type_use ']' | '(' type_use ',' { type_use ',' } [ type_use ] ')'

// Some history, just for complain...
// First, only primitive types and one level of array are supported, 
//     recursive types are not supported, so TypeUseBase are primitive types, TypeUse are enum of them or an array      // TypeBase + Type
// Then recursive array are accepted and boxed value are accepted so there is only TypeUse 
//     and primitive types as enum member, including the recursive array                                                // Type
// Then position info is add and TypeUse is then devided into TypeUseBase and its position                              // TypeBase + Type
// Then more primitive types added and TypeUseBase become large, nearly twenty members                                  // TypeBase + Type
// Then tuple type and identifier as user define types are accepted and TypeUseBase is managed to 6 enum members 
//      and position info are moved back, so there is no more TypeUseBase again                                         // Type
// Then a Primtype enum is added instead of the keywordkind, for unit is not keyword but prim type                      // Type + PrimitiveType
// Then primitive type are removed from sytnax parse                                                                    // Type
// Then rename SMType to TypeUse                                                                                        // TypeUse
// Then enum members are hide, 17/4/10                                                                                  // TypeUse + ActualTypeUse
// Then enum members are public, <unknown-time>                                                                         // TypeUse + ActualTypeUse
// Then array and tuple enum members are removed and use template aware type use, 17/6/12                               // TypeUse

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
pub struct TypeUse {
    pub base: IsId,
    pub base_span: Span,        // maybe default for array and tuple
    pub quote_span: Span,       // span of `()` or `[]` or (unimplemented)`<>`
    pub params: Vec<TypeUse>,
    pub all_span: Span, 
}
impl ISyntaxFormat for TypeUse {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("type-use").space();
        if self.params.len() == 0 { 
            f.isid(self.base).space().span(self.base_span).finish() 
        } else {
            let mut f = f.lit("template").space().span(self.all_span).endl()
                .indent1().lit("base-is").space().isid(self.base).space().span(self.base_span).endl()
                .indent1().lit("quote").space().span(self.quote_span);
            for typeuse in &self.params { f = f.endl().apply2(typeuse); }
            f.finish()
        }
    }
}
impl fmt::Debug for TypeUse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl TypeUse {

    pub fn new_simple(base: impl Into<IsId>, base_span: Span) -> TypeUse {
        TypeUse{ all_span: base_span, params: Vec::new(), quote_span: Span::new(0, 0), base: base.into(), base_span }
    }
    pub fn new_template(base: impl Into<IsId>, base_span: Span, quote_span: Span, params: Vec<TypeUse>) -> TypeUse {
        TypeUse{ all_span: base_span + quote_span, base: base.into(), base_span, quote_span, params }
    }
}

impl Node for TypeUse {
    type ParseOutput = TypeUse;

    fn matches(current: &Token) -> bool {
        match current {
            &Token::Ident(_) | &Token::Sep(Separator::LeftBracket) | &Token::Sep(Separator::LeftParen) => true,
            &Token::Keyword(kw) => kw.kind(KeywordKind::Primitive),
            _ => false,
        }
    }

    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<TypeUse> where F: FileSystem {

        if let Some(left_bracket_span) = sess.try_expect_sep(Separator::LeftBracket) {
            let inner = TypeUse::parse(sess)?;
            let right_bracket_span = sess.expect_sep(Separator::RightBracket)?;
            Ok(TypeUse::new_template(sess.base.chars.intern("array"), Span::new(0, 0), left_bracket_span + right_bracket_span, vec![inner]))
        } else if let Some(left_paren_span) = sess.try_expect_sep(Separator::LeftParen) {
            if let Some(right_paren_span) = sess.try_expect_sep(Separator::RightParen) {
                return Ok(TypeUse::new_simple(sess.base.chars.intern("unit"), left_paren_span + right_paren_span));
            }
            
            let mut tuple_types = vec![TypeUse::parse(sess)?];
            let (ending_span, end_by_comma) = loop {
                if let Some((_comma_span, right_paren_span)) = sess.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                    break (right_paren_span, true);
                } else if let Some(right_paren_span) = sess.try_expect_sep(Separator::RightParen) {
                    break (right_paren_span, false);
                }
                let _comma_span = sess.expect_sep(Separator::Comma)?;
                tuple_types.push(TypeUse::parse(sess)?);
            };
                
            let paren_span = left_paren_span + ending_span;
            if tuple_types.len() == 1 && !end_by_comma {            // len == 0 already rejected
                sess.emit("Single item tuple type use").detail(paren_span, "type use here");
            }
            Ok(TypeUse::new_template(sess.base.chars.intern("tuple"), Span::new(0, 0), paren_span, tuple_types))
        } else {
            let (symid, sym_span) = sess.expect_ident_or_keyword_kind(KeywordKind::Primitive)?;
            Ok(TypeUse::new_simple(symid, sym_span))
        }
    }
}

#[cfg(test)] #[test]
fn type_use_parse() {
    use super::make_node;

    assert_eq!{ make_node!("u8" as TypeUse), TypeUse::new_simple(2, Span::new(0, 1)) }
    assert_eq!{ make_node!("i32" as TypeUse), TypeUse::new_simple(2, Span::new(0, 2)) }
    assert_eq!{ make_node!("char" as TypeUse), TypeUse::new_simple(2, Span::new(0, 3)) }
    assert_eq!{ make_node!("string" as TypeUse), TypeUse::new_simple(2, Span::new(0, 5)) }
    assert_eq!{ make_node!("helloworld_t" as TypeUse), TypeUse::new_simple(2, Span::new(0, 11)) }

    assert_eq!{ make_node!("()" as TypeUse, [], ["unit"]), TypeUse::new_simple(1, Span::new(0, 1)) };

    assert_eq!{ make_node!("[u8]" as TypeUse, [], ["array", "u8"]),
        TypeUse::new_template(1, Span::new(0, 0), Span::new(0, 3), vec![
                TypeUse::new_simple(2, Span::new(1, 2))
        ])
    }

    assert_eq!{ make_node!("[[he_t]]" as TypeUse, [Span::new(2, 5)], ["array"]),
        TypeUse::new_template(1, Span::new(0, 0), Span::new(0, 7), vec![
            TypeUse::new_template(1, Span::new(0, 0),Span::new(1, 6), vec![
                TypeUse::new_simple(2, Span::new(2, 5))
            ])
        ])
    }

    // TODO TODO: finish them
    // // Tuple
    // (i32,)
    // //           1234567890123
    // ast_test_case!{ "(i32, string)", 5, Span::new(0, 12),
    //     TypeUseF::new_tuple(Span::new(0, 12), vec![
    //         simple!("i32", Span::new(1, 3)),
    //         simple!("string", Span::new(6, 11)),
    //     ])
    // }        //  12345678901234
    // ast_test_case!{ "(char, hw_t, )", 6, Span::new(0, 13),
    //     TypeUseF::new_tuple(Span::new(0, 13), vec![
    //         simple!("char", Span::new(1, 4)),
    //         simple!("hw_t", Span::new(7, 10)),
    //     ])   //  0        1         2         3
    // }        //  123456789012345678901234567890123456
    // ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, Span::new(0, 35), 
    //     TypeUseF::new_tuple(Span::new(0, 35), vec![
    //         TypeUseF::new_array(Span::new(1, 6),
    //             simple!("char", Span::new(2, 5))
    //         ),
    //         simple!("i32", Span::new(9, 11)),
    //         simple!("u17", Span::new(14, 16)),
    //         TypeUseF::new_array(Span::new(19, 34),
    //             TypeUseF::new_tuple(Span::new(20, 33), vec![
    //                 TypeUseF::new_unit(Span::new(21, 22)),
    //                 simple!("u8", Span::new(25, 26)),
    //                 simple!("f128", Span::new(29, 32)),
    //             ])
    //         ), 
    //     ])
    // } //             1234567
    // ast_test_case!{ "(i233,)", 4, Span::new(0, 6), 
    //     TypeUseF::new_tuple(Span::new(0, 6), vec![
    //         simple!("i233", Span::new(1, 4))
    //     ])
    // }
    // ast_test_case!{ "(i32)", 3, Span::new(0, 4),
    //     TypeUseF::new_tuple(Span::new(0, 4), vec![
    //         simple!("i32", Span::new(1, 3)),
    //     ]),
    //     [ Message::new_by_str("Single item tuple type use", vec![(Span::new(0, 4), "type use here")]) ]
    // }

    // // Auto generated mixed
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "((i8, clL, Kopu), f64,)", 12, Span::new(0, 22),
    //     TypeUseF::new_tuple(Span::new(0, 22), vec![
    //         TypeUseF::new_tuple(Span::new(1, 15), vec![
    //             simple!("i8", Span::new(2, 3)),
    //             simple!("clL", Span::new(6, 8)),
    //             simple!("Kopu", Span::new(11, 14))
    //         ]), 
    //         simple!("f64", Span::new(18, 20))
    //     ])
    // } //             12345678
    // ast_test_case!{ "[BJlbk4]", 3, Span::new(0, 7),
    //     TypeUseF::new_array(Span::new(0, 7), 
    //         simple!("BJlbk4", Span::new(1, 6)),
    //     ) //         0        1         2         3         4         5         6         7         8         9
    // } //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    // ast_test_case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)", 49, Span::new(0, 93),
    //     TypeUseF::new_tuple(Span::new(0, 93), vec![
    //         TypeUseF::new_tuple(Span::new(1, 76), vec![
    //             simple!("char", Span::new(2, 5)), 
    //             simple!("jq", Span::new(8, 9)),
    //             TypeUseF::new_tuple(Span::new(12, 74), vec![
    //                 TypeUseF::new_tuple(Span::new(13, 59), vec![
    //                     simple!("u8", Span::new(14, 15)),
    //                     TypeUseF::new_array(Span::new(18, 53), 
    //                         TypeUseF::new_tuple(Span::new(19, 52), vec![
    //                             TypeUseF::new_array(Span::new(20, 26), 
    //                                 simple!("o2fcd", Span::new(21, 25))
    //                             ),
    //                             TypeUseF::new_array(Span::new(29, 33), 
    //                                 simple!("CKw", Span::new(30, 32))
    //                             ),
    //                             TypeUseF::new_tuple(Span::new(36, 51), vec![
    //                                 TypeUseF::new_array(Span::new(37, 44), 
    //                                     simple!("eCDn2I", Span::new(38, 43))
    //                                 ), 
    //                                 simple!("u16", Span::new(47, 49))
    //                             ])
    //                         ])
    //                     ),
    //                     simple!("i16", Span::new(56, 58))
    //                 ]),
    //                 TypeUseF::new_array(Span::new(62, 68), 
    //                     simple!("pxplh", Span::new(63, 67))
    //                 ),
    //                 simple!("u32", Span::new(71, 73))
    //             ])
    //         ]),
    //         TypeUseF::new_array(Span::new(79, 87), 
    //             simple!("vrud2vC", Span::new(80, 86))
    //         ),
    //         simple!("u64", Span::new(90, 92))
    //     ])
    // }
    // ast_test_case!{ "sxM4", 1, Span::new(0, 3), simple!("sxM4", Span::new(0, 3)) }
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "([pwi], [u64], i33, i8)", 13, Span::new(0, 22), 
    //     TypeUseF::new_tuple(Span::new(0, 22), vec![
    //         TypeUseF::new_array(Span::new(1, 5), 
    //             simple!("pwi", Span::new(2, 4))
    //         ), 
    //         TypeUseF::new_array(Span::new(8, 12),
    //             simple!("u64", Span::new(9, 11))
    //         ),
    //         simple!("i33", Span::new(15, 17)),
    //         simple!("i8", Span::new(20, 21))
    //     ])
    // }
}