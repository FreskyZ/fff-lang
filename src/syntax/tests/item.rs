use super::*;

#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ "enum E { M1, M2 = 1,}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(7, 20), all_span: Span::new(0, 20), base_type: None, variants: vec![
            EnumVariant{ name: IsId::new(3), name_span: Span::new(9, 10), value: None, all_span: Span::new(9, 10) },
            EnumVariant{ name: IsId::new(4), name_span: Span::new(13, 14), value: Some(make_expr!(i32 1 18:18)), all_span: Span::new(13, 18) }]}
    }

    //      0123456789012
    case!{ "enum E: u8 {}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(11, 12), all_span: Span::new(0, 12), variants: Vec::new(),
            base_type: Some(PrimitiveType{ name: Keyword::U8, span: Span::new(8, 9) }), }
    }
}

#[test]
fn fn_def_parse() {
    //                                012345678901
    case!{ "fn main() {}" as FnDef,
        FnDef::new(Span::new(0, 11), 2, Span::new(3, 6), Span::new(7, 8), 
            vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //                      0        1
    //                      0123456789012345678
    case!{ "fn main(ac: i32) {}" as FnDef,
        FnDef::new(Span::new(0, 18), 2, Span::new(3, 6), Span::new(7, 15), 
            vec![
                FnParam::new(3, Span::new(8, 9), make_type!(prim 12:14 I32)),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        ), strings ["main", "ac"]
    }

    //      0         1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ " fn mainxxx(argv:&    string   ,this:i32, some_other: char, )  { println(this); }" as FnDef,
        FnDef::new(Span::new(1, 80), 2, Span::new(4, 10), Span::new(11, 60), 
            vec![
                FnParam::new(3, Span::new(12, 15), make_type!(ref 17:27 make_type!(simple 22:27 #4))),
                FnParam::new(5, Span::new(32, 35), make_type!(prim 37:39 I32)),
                FnParam::new(6, Span::new(42, 51), make_type!(prim 54:57 Char)),
            ],
            None,
            Block::new(Span::new(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(65, 78),
                    make_expr!(fn 65:77 paren 72:77
                        make_name!(simple 65:71 #7),
                        make_name!(simple 73:76 #5))
                ))
            ]) //    2          3       4         5       6             7
        ), strings ["mainxxx", "argv", "string", "this", "some_other", "println"]
    }

    //                                0        1               
    //                                1234567890123456789
    case!{ "fn main() -> i32 {}" as FnDef,
        FnDef::new(Span::new(0, 18),
            2, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(make_type!(prim 13:15 I32)),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //      0         1         2         3         4         5         6
    //      0123456789012345678901234567890123456789012345678901234567890
    case!{ "fn ffff(argc: i32, argv: &&byte,   envv:  &string,) -> i32 {}" as FnDef,
        FnDef::new(Span::new(0, 60), 2, Span::new(3, 6), Span::new(7, 50), vec![
            FnParam::new(3, Span::new(8, 11), make_type!(prim 14:16 I32)),
            FnParam::new(4, Span::new(19, 22), make_type!(ref 25:30 make_type!(ref 26:30 make_type!(simple 27:30 #5)))),
            FnParam::new(6, Span::new(35, 38), make_type!(ref 42:48 make_type!(simple 43:48 #7))),
        ], 
            Some(make_type!(prim 55:57 I32)), 
            Block::new(Span::new(59, 60), vec![])
            //       2       3       4       5      6         7
        ), strings ["ffff", "argc", "argv", "byte", "envv", "string"]
    }

    //      0         1         2         3         4         5         6         7         8         9         0         1         2
    //      01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ "fn impl_visit(this: &This, node: &Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result {}" as FnDef,
        FnDef::new(Span::new(0, 124), 2, Span::new(3, 12), Span::new(13, 106), vec![
            FnParam::new(3, Span::new(14, 17), make_type!(ref 20:24 make_type!(simple 21:24 #4))),
            FnParam::new(5, Span::new(27, 30), make_type!(ref 33:37 make_type!(simple 34:37 #6))),
            FnParam::new(7, Span::new(40, 44), make_type!(simple 47:52 #8)),
            FnParam::new(9, Span::new(55, 58), make_type!(simple 61:64 #10)),
            FnParam::new(11, Span::new(67, 70), make_type!(fn ret 73:105 paren 75:76 [],
                make_type!(plain 81:105 false, None,
                    make_type!(segment generic 81:105 #12 81:86 quote 87:105
                        make_type!(ref 88:92 make_type!(simple 89:92 #4)),
                        make_type!(plain 95:104 false, None,
                            make_type!(segment 95:97 #13),
                            make_type!(segment 100:104 #14)))))),
        ],
            Some(make_type!(plain 111:121 false, None, 
                make_type!(segment 111:113 #13),
                make_type!(segment 116:121 #12))),
            Block::new(Span::new(123, 124), vec![]),
        //           2             3       4       5       6       7        8         9       10      11      12        13     14
        ), strings ["impl_visit", "this", "This", "node", "Node", "title", "string", "span", "Span", "then", "Result", "fmt", "Error"]
    }
}

#[test]
fn type_def_parse() {
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
