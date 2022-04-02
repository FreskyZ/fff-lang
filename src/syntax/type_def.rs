///! syntax::type_def

#[cfg(test)] #[test]
fn type_def_parse() {use super::prelude::*;
    //                                  01234567890123456
    case!{ "type x { x: i32 }" as TypeDef,
        TypeDef{ all_span: Span::new(0, 16), name: IsId::new(2), name_span: Span::new(5, 5), fields: vec![
            TypeFieldDef{ all_span: Span::new(9, 14), name: IsId::new(2), name_span: Span::new(9, 9), colon_span: Span::new(10, 10),
                r#type: make_type!(prim 12:14 I32) }] }
    }
    case!{ "type x { x: i32,}" as TypeDef,
        TypeDef{ all_span: Span::new(0, 16), name: IsId::new(2), name_span: Span::new(5, 5), fields: vec![
            TypeFieldDef{ all_span:Span::new(9, 15), name: IsId::new(2), name_span: Span::new(9, 9), colon_span: Span::new(10, 10),
                r#type: make_type!(prim 12:14 I32) }] }
    }
    //                                    0         1         2         3         4
    //                                    0123456789012345678901234567890123456789012345
    case!{ "type array { data:  &u8, size: u64, cap: u64 }" as TypeDef,
        TypeDef{ all_span: Span::new(0, 45), name: IsId::new(2), name_span: Span::new(5, 9), fields: vec![
            TypeFieldDef{ all_span: Span::new(13, 23), name: IsId::new(3), name_span: Span::new(13, 16), colon_span: Span::new(17, 17),
                r#type: make_type!(ref 20:22 make_type!(prim 21:22 U8)) },
            TypeFieldDef{ all_span: Span::new(25, 34), name: IsId::new(4), name_span: Span::new(25, 28), colon_span: Span::new(29, 29),
                r#type: make_type!(prim 31:33 U64) },
            TypeFieldDef{ all_span: Span::new(36, 43), name: IsId::new(5), name_span: Span::new(36, 38), colon_span: Span::new(39, 39),
                r#type: make_type!(prim 41:43 U64) },
        ] }, strings ["array", "data", "size", "cap"]
    }
}
