use super::*;

#[test]
fn parse_block_stmt() {
    case!{ parse_block_stmt "{}",
        BlockStatement{ 
            span: Span::new(0, 1), 
            label: None,
            body: make_stmt!(block 0:1) }
    }

    case!{ parse_block_stmt "@: {}",
        BlockStatement{ 
            span: Span::new(0, 4),
            label: make_stmt!(label 0:0 #1),
            body: make_stmt!(block 3:4) }
    }
}

#[test]
fn expr_stmt_parse() {
    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ parse_expr_stmt "writeln(\"helloworld\");",
        Statement::SimpleExpr(make_stmt!(expr 0:21
            make_expr!(call 0:20 paren 7:20
                make_expr!(path simple 0:6 #2),
                make_expr!(str #3 8:19))))
    }

    //                      012345678901
    case!{ parse_expr_stmt "1 + 1 <<= 2;",  // to show I have 3 char Separator available
        Statement::AssignExpr(make_stmt!(assign 0:11 LtLtEq 6:8
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 1 4:4)),
            make_expr!(i32 2 10:10)))
    }
}

#[test]
fn parse_for_stmt() {
    //                      0123456789012345678
    case!{ parse_for_stmt "@2: for i in 42 {}",
        make_stmt!(for 0:17 var #3 8:8
            make_stmt!(label 0:1 #2),
            make_expr!(i32 42 13:14),
            make_stmt!(block 16:17))
    }

    //              0         1         2         3         4         5         6         7         
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    case!{ parse_for_stmt "@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }",
        make_stmt!(for 0:77 var #3 12:12
            make_stmt!(label 0:5 #2),
            make_expr!(call 17:50 paren 49:50
                make_expr!(member 17:48 dot 41:41 #6 42:48
                    make_expr!(call 17:40 paren 39:40
                        make_expr!(member 17:38 dot 29:29 #5 30:38
                            make_expr!(call 17:28 paren 22:28
                                make_expr!(path simple 17:21 #4),
                                make_expr!(i32 0 23:23),
                                make_expr!(i32 10 26:27))),)),),
            make_stmt!(block 52:77
                make_stmt!(expr 54:75
                    make_expr!(call 54:74 paren 61:74
                        make_expr!(path simple 54:60 #7),
                        make_expr!(str #8 62:73))))
        ), strings ["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"]
    }
}

#[test]
fn if_stmt_parse() {
    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ parse_if_stmt "if true { } else if false { } else {}",
        IfStatement{ span: Span::new(0, 36),
            if_clause: IfClause{ span: Span::new(0, 10), 
                condition: make_expr!(true 3:6),
                body: make_stmt!(block 8:10) }, 
            elseif_clauses: vec![IfClause{ span: Span::new(12, 28), 
                condition: make_expr!(false 20:24),
                body: make_stmt!(block 26:28) }],
            else_clause: Some(ElseClause{ span: Span::new(30, 36),
                body: make_stmt!(block 35:36) }) }
    }

    //              0         1         2         3         4         5         6         7
    //              012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ parse_if_stmt "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}",
        IfStatement{ span: Span::new(0, 71),
            if_clause: IfClause{ span: Span::new(0, 41), 
                condition: make_expr!(i32 1 3:3),
                body: make_stmt!(block 5:41
                    make_stmt!(expr 7:20
                        make_expr!(call 7:19 paren 17:19
                            make_expr!(member 7:16 dot 10:10 #3 11:16
                                make_expr!(path simple 7:9 #2)),
                            make_expr!(path simple 18:18 #4))),
                    make_stmt!(expr 22:39
                        make_expr!(call 22:38 paren 36:38
                            make_expr!(member 22:35 dot 27:27 #6 28:35
                                make_expr!(path simple 22:26 #5)),
                            make_expr!(path simple 37:37 #7)))) },
            elseif_clauses: vec![],
            else_clause: Some(ElseClause{ span: Span::new(43, 71),
                body: make_stmt!(block 48:71
                    make_stmt!(expr 50:70
                        make_expr!(call 50:69 paren 61:69
                            make_expr!(member 50:60 dot 57:57 #8 58:60
                                make_expr!(array 50:56
                                    make_expr!(i32 1 51:51),
                                    make_expr!(i32 2 53:53),
                                    make_expr!(i32 3 55:55))),
                            make_expr!(path simple 62:68 #9)))) }),
        }, strings ["sth", "do_sth", "a", "other", "do_other", "b", "map", "writeln"]
    }

    // if condition does not expect object literal, unless parened
    //      0         1         2
    //      012345678901234567890123
    case!{ parse_if_stmt "if a {} else if (b{}) {}",
        IfStatement{ span: Span::new(0, 23), 
            if_clause: IfClause{ span: Span::new(0, 6),
                condition: make_expr!(path simple 3:3 #2),
                body: make_stmt!(block 5:6) },
            elseif_clauses: vec![IfClause{ span: Span::new(8, 23),
                condition: make_expr!(paren 16:20
                    make_expr!(object 17:19 quote 18:19
                        make_expr!(path simple 17:17 #3),)),
                body: make_stmt!(block 22:23) }],
            else_clause: None,
        }
    }
}

#[test]
fn jump_stmt_parse() {
    
    case!{ parse_continue_stmt "continue;", 
        make_stmt!(continue 0:8 make_stmt!(label none))
    }
    case!{ parse_continue_stmt "continue @1;",
        make_stmt!(continue 0:11 make_stmt!(label 9:10 #2))
    }
    
    case!{ parse_break_stmt "break;",
        make_stmt!(break 0:5 make_stmt!(label none))
    }
    case!{ parse_break_stmt "break @1;",
        make_stmt!(break 0:8 make_stmt!(label 6:7 #2))
    }
}

#[test]
fn parse_loop_stmt() {

    case!{ parse_loop_stmt "loop {}",
        make_stmt!(loop 0:6
            make_stmt!(label none),
            make_stmt!(block 5:6))
    }
    //                                        1234567890123456789 0123 45678
    case!{ parse_loop_stmt "@@: loop { println(\"233\"); }",
        make_stmt!(loop 0:27
            make_stmt!(label 0:1 #2),
            make_stmt!(block 9:27
                make_stmt!(expr 11:25
                    make_expr!(call 11:24 paren 18:24
                        make_expr!(path simple 11:17 #3),
                        make_expr!(str #4 19:23))))
        ), strings ["@", "println", "233"]
    }
}

#[test]
fn parse_module_stmt() {

    case!{ parse_module_stmt "module a;",
        ModuleStatement{ name: make_stmt!(id 7:7 #2), path: None, span: Span::new(0, 8) },
    }
    //                   012345678901234567890
    case!{ parse_module_stmt "module os \"windows\";",
        ModuleStatement{ name: make_stmt!(id 7:8 #2), path: Some(make_stmt!(id 10:18 #3)), span: Span::new(0, 19) },
    }

    //      0         1          2
    //      012345678901234567 89012345 6
    case!{ parse_module_stmt "module otherdir r\"ab/c.f3\";",
        ModuleStatement{ name: make_stmt!(id 7:14 #2), path: Some(make_stmt!(id 16:25 #3)), span: Span::new(0, 26) },
    }
}

#[test]
fn module_parse() {
    //                      0123456789012345678901234
    case!{ parse_module "use a; module b; 3; b; a;",
        Module{ file: crate::source::FileId::new(1), items: vec![
            Item::Use(UseStatement{ span: Span::new(0, 5), alias: None, path: make_path!(simple 4:4 #2) }),
            Item::Import(ModuleStatement{ name: make_stmt!(id 14:14 #3), path: None, span: Span::new(7, 15) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(17, 18), expr: make_expr!(i32 3 17:17) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(20, 21), expr: make_expr!(path simple 20:20 #3) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(23, 24), expr: make_expr!(path simple 23:23 #2) }),
        ] }
    }
}

#[test]
fn ret_stmt_parse() {

    case!{ parse_ret_stmt "return;",
        make_stmt!(ret 0:6 None)
    }
    case!{ parse_ret_stmt "return 1 + 1;",
        make_stmt!(ret 0:12
            Some(make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11)))),
    }
}

#[test]
fn use_stmt_parse() {

    case!{ parse_use_stmt "use a;",
        UseStatement{ span: Span::new(0, 5), alias: None, 
            path: make_path!(simple 4:4 #2) },
    }
    //                   0123456789012345678901234567890
    case!{ parse_use_stmt "use std::fmt::Debug as Display;",
        UseStatement{ span: Span::new(0, 30), alias: Some(make_stmt!(id 23:29 #5)),
            path: make_path!(4:18
                make_path!(segment simple 4:6 #2),
                make_path!(segment simple 9:11 #3),
                make_path!(segment simple 14:18 #4),
            ) },
    }
}

#[test]
fn var_decl_stmt_parse() {
    //                                           12345678901234
    case!{ parse_var_decl "const abc = 0;",
        make_stmt!(const 0:13 #2 6:8
            None,
            Some(make_expr!(i32 0 12:12)))
    }

    //                                           0        1         
    //                                           12345678901234567890
    case!{ parse_var_decl "var hij = [1, 3, 5];",
        make_stmt!(var 0:19 #2 4:6
            None,
            Some(make_expr!(array 10:18
                make_expr!(i32 1 11:11),
                make_expr!(i32 3 14:14),
                make_expr!(i32 5 17:17))))
    }
    
    //       1234567890123456789
    case!{ parse_var_decl "const input: string;",
        make_stmt!(const 0:19 #2 6:10
            Some(make_type!(simple 13:18 #3)),
            None)
    }
    
    //      0         1         2
    //      012345678901234567890123
    case!{ parse_var_decl "var buf: [(u8, char);1];",
        make_stmt!(var 0:23 #2 4:6
            Some(make_type!(array 9:22
                make_type!(tuple 10:19
                    make_type!(prim 11:12 U8),
                    make_type!(prim 15:18 Char)),
                make_expr!(i32 1 21:21).into())),
            None)
    }

    // Future Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //      0         1         2         3         4
    //      01234567890123456789012345678901234567890123456789
    case!{ parse_var_decl "var buf: ([u8;3], u32) = ([1u8, 5u8, 0x7u8], abc);",
        make_stmt!(var 0:49 #2 4:6
            Some(make_type!(tuple 9:21
                make_type!(array 10:15 
                    make_type!(prim 11:12 U8), 
                    make_expr!(i32 3 14:14).into()), 
                make_type!(prim 18:20 U32))),
            Some(make_expr!(tuple 25:48
                make_expr!(array 26:42
                    make_expr!(u8 1 27:29),
                    make_expr!(u8 5 32:34),
                    make_expr!(u8 7 37:41)),
                make_expr!(path simple 45:47 #3)))
        ), strings ["buf", "abc"]
    }

    case!{ parse_var_decl "var a;",
        make_stmt!(var 0:5 #2 4:4 None, None), errors make_errors!(
            e: e.emit("require type annotation")
                .detail(Span::new(4, 4), "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression")),
    }
}

#[test]
fn parse_while_stmt() {

    case!{ parse_while_stmt "while 1 {}",
        make_stmt!(while 0:9
            make_stmt!(label none),
            make_expr!(i32 1 6:6),
            make_stmt!(block 8:9))
    }
    //      0        1         2         3         4        
    //      01234567890123456789012345 67890123456789012 3456
    case!{ parse_while_stmt "@2: while true { writeln(\"fresky hellooooo\"); }",
        make_stmt!(while 0:46
            make_stmt!(label 0:1 #2),
            make_expr!(true 10:13),
            make_stmt!(block 15:46
                make_stmt!(expr 17:44
                    make_expr!(call 17:43 paren 24:43
                        make_expr!(path simple 17:23 #3),
                        make_expr!(str #4 25:42))))
        ), strings ["2", "writeln", "fresky hellooooo"]
    }
}

#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ parse_enum_def "enum E { M1, M2 = 1,}", 
        EnumDef{ name: make_stmt!(id 5:5 #2), quote_span: Span::new(7, 20), span: Span::new(0, 20), base_type: None, variants: vec![
            EnumDefVariant{ name: make_stmt!(id 9:10 #3), value: None, span: Span::new(9, 10) },
            EnumDefVariant{ name: make_stmt!(id 13:14 #4), value: Some(make_expr!(i32 1 18:18)), span: Span::new(13, 18) }]}
    }

    //      0123456789012
    case!{ parse_enum_def "enum E: u8 {}",
        EnumDef{ name: make_stmt!(id 5:5 #2), quote_span: Span::new(11, 12), span: Span::new(0, 12), variants: Vec::new(),
            base_type: Some(PrimitiveType{ base: Keyword::U8, span: Span::new(8, 9) }), }
    }
}

#[test]
fn fn_def_parse() {
    //                                012345678901
    case!{ parse_fn_def "fn main() {}",
        FnDef{
            span: Span::new(0, 11), 
            name: make_stmt!(name 3:6 #2), 
            quote_span: Span::new(7, 8), 
            parameters: vec![], 
            ret_type: None,
            wheres: Vec::new(),
            body: make_stmt!(block 10:11),
        }
    }

    //                      0        1
    //                      0123456789012345678
    case!{ parse_fn_def "fn main(ac: i32) {}",
        FnDef{
            span: Span::new(0, 18), 
            name: make_stmt!(name 3:6 #2), 
            quote_span: Span::new(7, 15), 
            parameters: vec![
                make_stmt!(fp 8:14 #3 8:9
                    make_type!(prim 12:14 I32)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: make_stmt!(block 17:18),
        }, strings ["main", "ac"]
    }

    //      0         1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def " fn mainxxx(argv:&    string   ,this:i32, some_other: char, )  { println(this); }",
        FnDef{
            span: Span::new(1, 80),
            name: make_stmt!(name 4:10 #2),
            quote_span: Span::new(11, 60), 
            parameters: vec![
                make_stmt!(fp 12:27 #3 12:15
                    make_type!(ref 17:27 make_type!(simple 22:27 #4))),
                make_stmt!(fp 32:39 #5 32:35
                    make_type!(prim 37:39 I32)),
                make_stmt!(fp 42:57 #6 42:51
                    make_type!(prim 54:57 Char)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: make_stmt!(block 63:80
                make_stmt!(expr 65:78
                    make_expr!(call 65:77 paren 72:77
                        make_expr!(path simple 65:71 #7),
                        make_expr!(path simple 73:76 #5)))),
            //    2          3       4         5       6             7
        }, strings ["mainxxx", "argv", "string", "this", "some_other", "println"]
    }

    //                                0        1               
    //                                1234567890123456789
    case!{ parse_fn_def "fn main() -> i32 {}",
        FnDef{
            span: Span::new(0, 18),
            name: make_stmt!(name 3:6 #2), 
            quote_span: Span::new(7, 8), 
            parameters: vec![],
            ret_type: Some(make_type!(prim 13:15 I32)),
            wheres: Vec::new(),
            body: make_stmt!(block 17:18),
        }
    }
    //      0         1         2         3         4         5         6
    //      0123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def "fn ffff(argc: i32, argv: &&byte,   envv:  &string,) -> i32 {}",
        FnDef{
            span: Span::new(0, 60), 
            name: make_stmt!(name 3:6 #2), 
            quote_span: Span::new(7, 50), 
            parameters: vec![
                make_stmt!(fp 8:16 #3 8:11
                    make_type!(prim 14:16 I32)),
                make_stmt!(fp 19:30 #4 19:22
                    make_type!(ref 25:30 make_type!(ref 26:30 make_type!(simple 27:30 #5)))),
                make_stmt!(fp 35:48 #6 35:38
                    make_type!(ref 42:48 make_type!(simple 43:48 #7))),
            ], 
            ret_type: Some(make_type!(prim 55:57 I32)),
            wheres: Vec::new(),
            body: make_stmt!(block 59:60)
            //       2       3       4       5      6         7
        }, strings ["ffff", "argc", "argv", "byte", "envv", "string"]
    }

    //      0         1         2         3         4         5         6         7         8         9         0         1         2
    //      01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ parse_fn_def "fn impl_visit(this: &This, node: &Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result {}",
        FnDef{
            span: Span::new(0, 124), 
            name: make_stmt!(name 3:12 #2), 
            quote_span: Span::new(13, 106), 
            parameters: vec![
                make_stmt!(fp 14:24 #3 14:17
                    make_type!(ref 20:24 make_type!(simple 21:24 #4))),
                make_stmt!(fp 27:37 #5 27:30
                    make_type!(ref 33:37 make_type!(simple 34:37 #6))),
                make_stmt!(fp 40:52 #7 40:44
                    make_type!(simple 47:52 #8)),
                make_stmt!(fp 55:64 #9 55:58
                    make_type!(simple 61:64 #10)),
                make_stmt!(fp 67:105 #11 67:70
                    make_type!(fn ret 73:105 paren 75:76 [],
                        make_type!(path 81:105
                            make_path!(segment generic 81:105 #12 81:86 quote 87:105
                                make_type!(ref 88:92 make_type!(simple 89:92 #4)),
                                make_type!(path 95:104
                                    make_path!(segment simple 95:97 #13),
                                    make_path!(segment simple 100:104 #14)))))),
            ],
            ret_type: Some(make_type!(path 111:121
                make_path!(segment simple 111:113 #13),
                make_path!(segment simple 116:121 #12))),
            wheres: Vec::new(),
            body: make_stmt!(block 123:124)
        //           2             3       4       5       6       7        8         9       10      11      12        13     14
        }, strings ["impl_visit", "this", "This", "node", "Node", "title", "string", "span", "Span", "then", "Result", "fmt", "Error"]
    }

    // // although currently I think fn type is type not class, and callobject is implementing std::ops::Call, not Fn
    // // I f**king human parse this code once and pass?
    //                   0         1         2         3         4         5         6         7
    //                   01234567890123456789012345678901234567890123456789012345678901234567890123
    case!{ parse_fn_def "fn map<T, U, F>(this: Option<T>, f: F) -> Option<U> where F: fn(T) -> U {}",
        FnDef{
            span: Span::new(0, 73),
            name: make_stmt!(name 3:14 #2 3:5 quote 6:14
                make_stmt!(gp 7:7 #3),
                make_stmt!(gp 10:10 #4),
                make_stmt!(gp 13:13 #5)),
            quote_span: Span::new(15, 37),
            parameters: vec![
                make_stmt!(fp 16:30 #6 16:19
                    make_type!(path 22:30
                        make_path!(segment generic 22:30 #7 22:27 quote 28:30
                            make_type!(path simple 29:29 #3)))),
                make_stmt!(fp 33:36 #8 33:33
                    make_type!(path simple 36:36 #5)),
            ],
            ret_type: Some(make_type!(path 42:50
                make_path!(segment generic 42:50 #7 42:47 quote 48:50
                    make_type!(simple 49:49 #4)))),
            wheres: vec![
                WhereClause{ span: Span::new(58, 70), name: IdSpan::new(5, Span::new(58, 58)), constraints: vec![
                    make_type!(fn ret 61:70 paren 63:65 [
                        make_type!(fp 64:64 
                            make_type!(simple 64:64 #3))
                    ], make_type!(simple 70:70 #4))
                ]},
            ],
            body: make_stmt!(block 72:73),
        //           2      3    4    5    6       7         8
        }, strings ["map", "T", "U", "F", "this", "Option", "f"],
    }
}

#[test]
fn parse_type_def() {
    //                                  01234567890123456
    case!{ parse_type_def "type x { x: i32 }",
        TypeDef{ 
            span: Span::new(0, 16),
            name: make_stmt!(name 5:5 #2),
            fields: vec![
                TypeDefField{ span: Span::new(9, 14), name: make_stmt!(id 9:9 #2), colon_span: Span::new(10, 10),
                    r#type: make_type!(prim 12:14 I32) }] }
    }

    //                     0123456789
    case!{ parse_type_def "type x<>{}",
        TypeDef{
            span: Span::new(0, 9),
            name: make_stmt!(name 5:7 #2 5:5 quote 6:7),
            fields: Vec::new(),
        }, errors make_errors!(e: e.emit(strings::EmptyGenericParameterList).span(Span::new(6, 7)))
    }

    case!{ parse_type_def "type x { x: i32,}",
        TypeDef{ 
            span: Span::new(0, 16), 
            name: make_stmt!(name 5:5 #2), 
            fields: vec![
                TypeDefField{ span: Span::new(9, 14), name: make_stmt!(id 9:9 #2), colon_span: Span::new(10, 10),
                    r#type: make_type!(prim 12:14 I32) }] }
    }
    //                     0         1         2         3         4
    //                     0123456789012345678901234567890123456789012345
    case!{ parse_type_def "type array { data:  &u8, size: u64, cap: u64 }",
        TypeDef{ 
            span: Span::new(0, 45), 
            name: make_stmt!(name 5:9 #2), 
            fields: vec![
                TypeDefField{ span: Span::new(13, 22), name: make_stmt!(id 13:16 #3), colon_span: Span::new(17, 17),
                    r#type: make_type!(ref 20:22 make_type!(prim 21:22 U8)) },
                TypeDefField{ span: Span::new(25, 33), name: make_stmt!(id 25:28 #4), colon_span: Span::new(29, 29),
                    r#type: make_type!(prim 31:33 U64) },
                TypeDefField{ span: Span::new(36, 43), name: make_stmt!(id 36:38 #5), colon_span: Span::new(39, 39),
                    r#type: make_type!(prim 41:43 U64) },
            ]
        }, strings ["array", "data", "size", "cap"]
    }

    //                     0         1         2         3         4
    //                     01234567890123456789012345678901234567890123456789
    case!{ parse_type_def "type list<T> { data: &T, size: usize, cap: usize }",
        TypeDef{
            span: Span::new(0, 49),
            name: make_stmt!(name 5:11 #2 5:8 quote 9:11
                make_stmt!(gp 10:10 #3)),
            fields: vec![
                TypeDefField{ span: Span::new(15, 22), name: make_stmt!(id 15:18 #4), colon_span: Span::new(19, 19),
                    r#type: make_type!(ref 21:22 make_type!(path simple 22:22 #3)) },
                TypeDefField{ span: Span::new(25, 35), name: make_stmt!(id 25:28 #5), colon_span: Span::new(29, 29),
                    r#type: make_type!(path simple 31:35 #6) },
                TypeDefField{ span: Span::new(38, 47), name: make_stmt!(id 38:40 #7), colon_span: Span::new(41, 41),
                    r#type: make_type!(path simple 43:47 #6) },
            ]
        }, strings ["list", "T", "data", "size", "usize", "cap"]
    }

    //                     0         1         2         3         4         5
    //                     012345678901234567890123456789012345678901234567890123456
    case!{ parse_type_def "type hashmap<K, V, A> { buckets: &&(K, V), allocator: A }",
        TypeDef{
            span: Span::new(0, 56),
            name: make_stmt!(name 5:20 #2 5:11 quote 12:20
                make_stmt!(gp 13:13 #3),
                make_stmt!(gp 16:16 #4),
                make_stmt!(gp 19:19 #5)),
            fields: vec![
                TypeDefField{ span: Span::new(24, 40), name: make_stmt!(id 24:30 #6), colon_span: Span::new(31, 31),
                    r#type: make_type!(ref 33:40
                        make_type!(ref 34:40
                            make_type!(tuple 35:40
                                make_type!(path simple 36:36 #3),
                                make_type!(path simple 39:39 #4)))) },
                TypeDefField{ span: Span::new(43, 54), name: make_stmt!(id 43:51 #7), colon_span: Span::new(52, 52),
                    r#type: make_type!(path simple 54:54 #5) },
            ]
        }
    }
}
