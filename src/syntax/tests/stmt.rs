use super::*;

#[test]
fn parse_block_stmt() {
    case!{ parse_block_stmt "{}",
        |_| BlockStatement{
            span: Span::new(0, 1),
            label: None,
            body: make_stmt!(block 0:1) }
    }

    case!{ parse_block_stmt "@: {}",
        |x| BlockStatement{
            span: Span::new(0, 4),
            label: make_stmt!(x label 0:0 #""),
            body: make_stmt!(block 3:4) }
    }
}

#[test]
fn expr_stmt_parse() {
    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ parse_expr_stmt "writeln(\"helloworld\");",
        |x| Statement::SimpleExpr(make_stmt!(expr 0:21
            make_expr!(call 0:20 paren 7:20
                make_expr!(x path simple 0:6 #writeln),
                make_expr!(x str #"helloworld" 8:19))))
    }

    //                      012345678901
    case!{ parse_expr_stmt "1 + 1 <<= 2;",  // to show I have 3 char Separator available
        |_| Statement::AssignExpr(make_stmt!(assign 0:11 LtLtEq 6:8
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 1 4:4)),
            make_expr!(i32 2 10:10)))
    }
}

#[test]
fn parse_for_stmt() {
    //                      0123456789012345678
    case!{ parse_for_stmt "@2: for i in 42 {}", |x|
        make_stmt!(x for 0:17 var #i 8:8
            make_stmt!(x label 0:1 #"2"),
            make_expr!(i32 42 13:14),
            make_stmt!(block 16:17))
    }

    //              0         1         2         3         4         5         6         7
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    case!{ parse_for_stmt "@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }", |x|
        make_stmt!(x for 0:77 var #"_" 12:12
            make_stmt!(x label 0:5 #hello),
            make_expr!(call 17:50 paren 49:50
                make_expr!(x member 17:48 dot 41:41 #reverse 42:48
                    make_expr!(call 17:40 paren 39:40
                        make_expr!(x member 17:38 dot 29:29 #enumerate 30:38
                            make_expr!(call 17:28 paren 22:28
                                make_expr!(x path simple 17:21 #range),
                                make_expr!(i32 0 23:23),
                                make_expr!(i32 10 26:27))),)),),
            make_stmt!(block 52:77
                make_stmt!(expr 54:75
                    make_expr!(call 54:74 paren 61:74
                        make_expr!(x path simple 54:60 #writeln),
                        make_expr!(x str #"helloworld" 62:73)))))
    }
}

#[test]
fn if_stmt_parse() {
    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ parse_if_stmt "if true { } else if false { } else {}",
        |_| IfStatement{ span: Span::new(0, 36),
            if_clause: IfClause{ span: Span::new(0, 10),
                condition: make_expr!(true 3:6),
                body: make_stmt!(block 8:10) },
            elseif_clauses: vec![IfClause{ span: Span::new(12, 28),
                condition: make_expr!(false 20:24),
                body: make_stmt!(block 26:28) }],
            else_clause: Some(ElseClause{ span: Span::new(30, 36),
                body: make_stmt!(block 35:36) }) }
    }

    //                    0         1         2         3         4         5         6         7
    //                    012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ parse_if_stmt "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}",
        |x| IfStatement{ span: Span::new(0, 71),
            if_clause: IfClause{ span: Span::new(0, 41),
                condition: make_expr!(i32 1 3:3),
                body: make_stmt!(block 5:41
                    make_stmt!(expr 7:20
                        make_expr!(call 7:19 paren 17:19
                            make_expr!(x member 7:16 dot 10:10 #do_sth 11:16
                                make_expr!(x path simple 7:9 #sth)),
                            make_expr!(x path simple 18:18 #a))),
                    make_stmt!(expr 22:39
                        make_expr!(call 22:38 paren 36:38
                            make_expr!(x member 22:35 dot 27:27 #do_other 28:35
                                make_expr!(x path simple 22:26 #other)),
                            make_expr!(x path simple 37:37 #b)))) },
            elseif_clauses: vec![],
            else_clause: Some(ElseClause{ span: Span::new(43, 71),
                body: make_stmt!(block 48:71
                    make_stmt!(expr 50:70
                        make_expr!(call 50:69 paren 61:69
                            make_expr!(x member 50:60 dot 57:57 #map 58:60
                                make_expr!(array 50:56
                                    make_expr!(i32 1 51:51),
                                    make_expr!(i32 2 53:53),
                                    make_expr!(i32 3 55:55))),
                            make_expr!(x path simple 62:68 #writeln)))) }),
        }
    }

    // if condition does not expect object literal, unless parened
    //      0         1         2
    //      012345678901234567890123
    case!{ parse_if_stmt "if a {} else if (b{}) {}",
        |x| IfStatement{ span: Span::new(0, 23),
            if_clause: IfClause{ span: Span::new(0, 6),
                condition: make_expr!(x path simple 3:3 #a),
                body: make_stmt!(block 5:6) },
            elseif_clauses: vec![IfClause{ span: Span::new(8, 23),
                condition: make_expr!(paren 16:20
                    make_expr!(object 17:19 quote 18:19
                        make_expr!(x path simple 17:17 #b),)),
                body: make_stmt!(block 22:23) }],
            else_clause: None,
        }
    }
}

#[test]
fn jump_stmt_parse() {

    case!{ parse_continue_stmt "continue;", |_| make_stmt!(continue 0:8 make_stmt!(label none)) }
    case!{ parse_continue_stmt "continue @1;", |x| make_stmt!(continue 0:11 make_stmt!(x label 9:10 #"1")) }

    case!{ parse_break_stmt "break;", |_| make_stmt!(break 0:5 make_stmt!(label none)) }
    case!{ parse_break_stmt "break @1;", |x| make_stmt!(break 0:8 make_stmt!(x label 6:7 #"1")) }
}

#[test]
fn parse_loop_stmt() {

    case!{ parse_loop_stmt "loop {}", |_|
        make_stmt!(loop 0:6
            make_stmt!(label none),
            make_stmt!(block 5:6))
    }
    //                                        1234567890123456789 0123 45678
    case!{ parse_loop_stmt "@@: loop { println(\"233\"); }", |x|
        make_stmt!(loop 0:27
            make_stmt!(x label 0:1 #"@"),
            make_stmt!(block 9:27
                make_stmt!(expr 11:25
                    make_expr!(call 11:24 paren 18:24
                        make_expr!(x path simple 11:17 #println),
                        make_expr!(x str #"233" 19:23)))))
    }
}

#[test]
fn parse_module_stmt() {

    case!{ parse_module_stmt "module a;",
        |x| ModuleStatement{ name: make_stmt!(x id 7:7 #a), path: None, span: Span::new(0, 8) },
    }
    //                   012345678901234567890
    case!{ parse_module_stmt "module os \"windows\";",
        |x| ModuleStatement{ name: make_stmt!(x id 7:8 #os), path: Some(make_stmt!(x id 10:18 #"windows")), span: Span::new(0, 19) },
    }

    //      0         1          2
    //      012345678901234567 89012345 6
    case!{ parse_module_stmt "module otherdir r\"ab/c.f3\";",
        |x| ModuleStatement{ name: make_stmt!(x id 7:14 #otherdir), path: Some(make_stmt!(x id 16:25 #"ab/c.f3")), span: Span::new(0, 26) },
    }
}

#[test]
fn module_parse() {
    //                      0123456789012345678901234
    case!{ parse_module "use a; module b; 3; b; a;",
        |x| Module{ file: crate::source::FileId::new(1), items: vec![
            Item::Use(UseStatement{ span: Span::new(0, 5), alias: None, path: make_path!(x simple 4:4 #a) }),
            Item::Import(ModuleStatement{ name: make_stmt!(x id 14:14 #b), path: None, span: Span::new(7, 15) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(17, 18), expr: make_expr!(i32 3 17:17) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(20, 21), expr: make_expr!(x path simple 20:20 #b) }),
            Item::SimpleExpr(SimpleExprStatement{ span: Span::new(23, 24), expr: make_expr!(x path simple 23:23 #a) }),
        ] }
    }
}

#[test]
fn ret_stmt_parse() {

    case!{ parse_ret_stmt "return;", |_| make_stmt!(ret 0:6 None) }

    case!{ parse_ret_stmt "return 1 + 1;", |_|
        make_stmt!(ret 0:12
            Some(make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11)))),
    }
}

#[test]
fn use_stmt_parse() {

    case!{ parse_use_stmt "use a;",
        |x| UseStatement{ span: Span::new(0, 5), alias: None,
            path: make_path!(x simple 4:4 #a) },
    }
    //                   0123456789012345678901234567890
    case!{ parse_use_stmt "use std::fmt::Debug as Display;",
        |x| UseStatement{ span: Span::new(0, 30), alias: Some(make_stmt!(x id 23:29 #Display)),
            path: make_path!(4:18
                make_path!(x segment simple 4:6 #std),
                make_path!(x segment simple 9:11 #fmt),
                make_path!(x segment simple 14:18 #Debug),
            ) },
    }
}

#[test]
fn var_decl_stmt_parse() {
    //                                           12345678901234
    case!{ parse_var_decl "const abc = 0;", |x|
        make_stmt!(x const 0:13 #abc 6:8
            None,
            Some(make_expr!(i32 0 12:12)))
    }

    //                                           0        1
    //                                           12345678901234567890
    case!{ parse_var_decl "var hij = [1, 3, 5];", |x|
        make_stmt!(x var 0:19 #hij 4:6
            None,
            Some(make_expr!(array 10:18
                make_expr!(i32 1 11:11),
                make_expr!(i32 3 14:14),
                make_expr!(i32 5 17:17))))
    }

    //       1234567890123456789
    case!{ parse_var_decl "const input: string;", |x|
        make_stmt!(x const 0:19 #input 6:10
            Some(make_type!(x simple 13:18 #string)),
            None)
    }

    //      0         1         2
    //      012345678901234567890123
    case!{ parse_var_decl "var buf: [(u8, char);1];", |x|
        make_stmt!(x var 0:23 #buf 4:6
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
    case!{ parse_var_decl "var buf: ([u8;3], u32) = ([1u8, 5u8, 0x7u8], abc);", |x|
        make_stmt!(x var 0:49 #buf 4:6
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
                make_expr!(x path simple 45:47 #abc))))
    }

    case!{ parse_var_decl "var a;",
        |x| make_stmt!(x var 0:5 #a 4:4 None, None),
        |e| e.emit("require type annotation")
                .detail(Span::new(4, 4), "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression"),
    }
}

#[test]
fn parse_while_stmt() {

    case!{ parse_while_stmt "while 1 {}", |_|
        make_stmt!(while 0:9
            make_stmt!(label none),
            make_expr!(i32 1 6:6),
            make_stmt!(block 8:9))
    }
    //      0        1         2         3         4
    //      01234567890123456789012345 67890123456789012 3456
    case!{ parse_while_stmt "@2: while true { writeln(\"fresky hellooooo\"); }", |x|
        make_stmt!(while 0:46
            make_stmt!(x label 0:1 #"2"),
            make_expr!(true 10:13),
            make_stmt!(block 15:46
                make_stmt!(expr 17:44
                    make_expr!(call 17:43 paren 24:43
                        make_expr!(x path simple 17:23 #writeln),
                        make_expr!(x str #"fresky hellooooo" 25:42)))))
    }
}

#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ parse_enum_def "enum E { M1, M2 = 1,}",
        |x| EnumDef{ name: make_stmt!(x id 5:5 #E), quote_span: Span::new(7, 20), span: Span::new(0, 20), base_type: None, variants: vec![
            EnumDefVariant{ name: make_stmt!(x id 9:10 #M1), value: None, span: Span::new(9, 10) },
            EnumDefVariant{ name: make_stmt!(x id 13:14 #M2), value: Some(make_expr!(i32 1 18:18)), span: Span::new(13, 18) }]}
    }

    //      0123456789012
    case!{ parse_enum_def "enum E: u8 {}",
        |x| EnumDef{ name: make_stmt!(x id 5:5 #E), quote_span: Span::new(11, 12), span: Span::new(0, 12), variants: Vec::new(),
            base_type: Some(PrimitiveType{ base: Keyword::U8, span: Span::new(8, 9) }), }
    }
}

#[test]
fn parse_fn_def() {
    //                                012345678901
    case!{ parse_fn_def "fn main() {}",
        |x| FnDef{
            span: Span::new(0, 11),
            name: make_stmt!(x name 3:6 #main),
            quote_span: Span::new(7, 8),
            parameters: vec![],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(block 10:11)),
        }
    }

    case!{ parse_fn_def "fn main();",
        |x| FnDef{
            span: Span::new(0, 9),
            name: make_stmt!(x name 3:6 #main),
            quote_span: Span::new(7, 8),
            parameters: vec![],
            ret_type: None,
            wheres: Vec::new(),
            body: None,
        }
    }

    //                      0        1
    //                      0123456789012345678
    case!{ parse_fn_def "fn main(ac: i32) {}",
        |x| FnDef{
            span: Span::new(0, 18),
            name: make_stmt!(x name 3:6 #main),
            quote_span: Span::new(7, 15),
            parameters: vec![
                make_stmt!(x fp 8:14 #ac 8:9
                    make_type!(prim 12:14 I32)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(block 17:18)),
        }
    }

    //      0         1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def " fn mainxxx(argv:&    string   ,this:i32, some_other: char, )  { println(this); }",
        |x| FnDef{
            span: Span::new(1, 80),
            name: make_stmt!(x name 4:10 #mainxxx),
            quote_span: Span::new(11, 60),
            parameters: vec![
                make_stmt!(x fp 12:27 #argv 12:15
                    make_type!(ref 17:27 make_type!(x simple 22:27 #string))),
                make_stmt!(x fp 32:39 #this 32:35
                    make_type!(prim 37:39 I32)),
                make_stmt!(x fp 42:57 #some_other 42:51
                    make_type!(prim 54:57 Char)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(block 63:80
                make_stmt!(expr 65:78
                    make_expr!(call 65:77 paren 72:77
                        make_expr!(x path simple 65:71 #println),
                        make_expr!(x path simple 73:76 #this))))),
        }
    }

    //                                0        1
    //                                1234567890123456789
    case!{ parse_fn_def "fn main() -> i32 {}",
        |x| FnDef{
            span: Span::new(0, 18),
            name: make_stmt!(x name 3:6 #main),
            quote_span: Span::new(7, 8),
            parameters: vec![],
            ret_type: Some(make_type!(prim 13:15 I32)),
            wheres: Vec::new(),
            body: Some(make_stmt!(block 17:18)),
        }
    }
    case!{ parse_fn_def "fn main() -> i32;",
        |x| FnDef{
            span: Span::new(0, 16),
            name: make_stmt!(x name 3:6 #main),
            quote_span: Span::new(7, 8),
            parameters: vec![],
            ret_type: Some(make_type!(prim 13:15 I32)),
            wheres: Vec::new(),
            body: None,
        }
    }
    //      0         1         2         3         4         5         6
    //      0123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def "fn ffff(argc: i32, argv: &&byte,   envv:  &string,) -> i32 {}",
        |x| FnDef{
            span: Span::new(0, 60),
            name: make_stmt!(x name 3:6 #ffff),
            quote_span: Span::new(7, 50),
            parameters: vec![
                make_stmt!(x fp 8:16 #argc 8:11
                    make_type!(prim 14:16 I32)),
                make_stmt!(x fp 19:30 #argv 19:22
                    make_type!(ref 25:30 make_type!(ref 26:30 make_type!(x simple 27:30 #byte)))),
                make_stmt!(x fp 35:48 #envv 35:38
                    make_type!(ref 42:48 make_type!(x simple 43:48 #string))),
            ],
            ret_type: Some(make_type!(prim 55:57 I32)),
            wheres: Vec::new(),
            body: Some(make_stmt!(block 59:60)),
        }
    }

    //                   0         1         2         3         4         5         6         7         8         9         0         1         2
    //                   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ parse_fn_def "fn impl_visit(this: &This, node: &Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result {}",
        |x| FnDef{
            span: Span::new(0, 124),
            name: make_stmt!(x name 3:12 #impl_visit),
            quote_span: Span::new(13, 106),
            parameters: vec![
                make_stmt!(x fp 14:24 #this 14:17
                    make_type!(ref 20:24 make_type!(x simple 21:24 #This))),
                make_stmt!(x fp 27:37 #node 27:30
                    make_type!(ref 33:37 make_type!(x simple 34:37 #Node))),
                make_stmt!(x fp 40:52 #title 40:44
                    make_type!(x simple 47:52 #string)),
                make_stmt!(x fp 55:64 #span 55:58
                    make_type!(x simple 61:64 #Span)),
                make_stmt!(x fp 67:105 #then 67:70
                    make_type!(fn ret 73:105 paren 75:76 [],
                        make_type!(path 81:105
                            make_path!(x segment generic 81:105 #Result 81:86 quote 87:105
                                make_type!(ref 88:92 make_type!(x simple 89:92 #This)),
                                make_type!(path 95:104
                                    make_path!(x segment simple 95:97 #fmt),
                                    make_path!(x segment simple 100:104 #Error)))))),
            ],
            ret_type: Some(make_type!(path 111:121
                make_path!(x segment simple 111:113 #fmt),
                make_path!(x segment simple 116:121 #Result))),
            wheres: Vec::new(),
            body: Some(make_stmt!(block 123:124)),
        }
    }

    // this was from the method inside the case! macro for some time
    //                   0         1         2         3         4         5         6         7         8  
    //                   0123456789012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ parse_fn_def "fn case_until_node<N, F>(input: &string, f: F, expect_node: N, expect_diagnostics:
        crate::diagnostics::Diagnostics, expect_strings: &string, backtrace: u32) -> Result<(), (N, N, SourceContext<VirtualFileSystem>)> where N: PartialEq + fmt::Debug, F: fn(&Parser) -> Result<N, Unexpected>;",
        // 45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
        // 9    10        11        12        13        14        15        16        17        18        19        20        21        22        23        24        25        26        27        28        29
        |x| FnDef{
            span: Span::new(0, 293),
            name: make_stmt!(x name 3:23 #case_until_node 3:17 quote 18:23
                make_stmt!(x gp 19:19 #N),
                make_stmt!(x gp 22:22 #F)),
            quote_span: Span::new(24, 163),
            parameters: vec![
                make_stmt!(x fp 25:38 #input 25:29
                    make_type!(ref 32:38 make_type!(x simple 33:38 #string))),
                make_stmt!(x fp 41:44 #f 41:41
                    make_type!(x simple 44:44 #F)),
                make_stmt!(x fp 47:60 #expect_node 47:57
                    make_type!(x simple 60:60 #N)),
                make_stmt!(x fp 63:121 #expect_diagnostics 63:80
                    make_type!(path 91:121
                        make_path!(x segment simple 91:95 #crate),
                        make_path!(x segment simple 98:108 #diagnostics),
                        make_path!(x segment simple 111:121 #Diagnostics))),
                make_stmt!(x fp 124:146 #expect_strings 124:137
                    make_type!(ref 140:146 make_type!(x simple 141:146 #string))),
                make_stmt!(x fp 149:162 #backtrace 149:157
                    make_type!(prim 160:162 U32)),
            ],
            ret_type: Some(make_type!(path 168:219
                make_path!(x segment generic 168:219 #Result 168:173 quote 174:219
                    make_type!(tuple 175:176),
                    make_type!(tuple 179:218
                        make_type!(x simple 180:180 #N),
                        make_type!(x simple 183:183 #N),
                        make_type!(path 186:217
                            make_path!(x segment generic 186:217 #SourceContext 186:198 quote 199:217
                                make_type!(x simple 200:216 #VirtualFileSystem))))))),
            wheres: vec![
                WhereClause{ span: Span::new(227, 251), name: IdSpan::new(make_isid!(x, N), Span::new(227, 227)), constraints: vec![
                    make_type!(x simple 230:238 #PartialEq),
                    make_type!(path 242:251
                        make_path!(x segment simple 242:244 #fmt),
                        make_path!(x segment simple 247:251 #Debug)),
                ]},
                WhereClause{ span: Span::new(254, 292), name: IdSpan::new(make_isid!(x, F), Span::new(254, 254)), constraints: vec![
                    make_type!(fn ret 257:292 paren 259:267 [
                        make_type!(fp 260:266
                            make_type!(ref 260:266 make_type!(x simple 261:266 #Parser))),
                    ], 
                    make_type!(path 272:292
                        make_path!(x segment generic 272:292 #Result 272:277 quote 278:292
                            make_type!(x simple 279:279 #N),
                            make_type!(x simple 282:291 #Unexpected)))),
                ]}
            ],
            body: None,
        }
    }

    // this is from the method inside the case! macro for now, currently
    //                   0         1         2         3         4         5         6         7         8         9        10
    //                   012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    case!{ parse_fn_def "fn case_until_node<V, P, E>(input: &str, actual_value_getter: P, expect_value_getter: E, expect_diagnostics:
        crate::diagnostics::Diagnostics, backtrace: u32) ->Result<(), (V, V, SourceContext<VirtualFileSystem>)> where V: PartialEq + fmt::Debug, P: fn(&Parser) -> Result<V, Unexpected>, E: fn(&Parser) -> V;",
        // 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
        // 12       13        14        15        16        17        18        19        20        21        22        23        24        25        26        27        28        29        30        31
        |x| FnDef{
            span: Span::new(0, 314),
            name: make_stmt!(x name 3:26 #case_until_node 3:17 quote 18:26
                make_stmt!(x gp 19:19 #V),
                make_stmt!(x gp 22:22 #P),
                make_stmt!(x gp 25:25 #E)),
            quote_span: Span::new(27, 164),
            parameters: vec![
                make_stmt!(x fp 28:38 #input 28:32
                    make_type!(ref 35:38 make_type!(x simple 36:38 #str))),
                make_stmt!(x fp 41:62 #actual_value_getter 41:59
                    make_type!(x simple 62:62 #P)),
                make_stmt!(x fp 65:86 #expect_value_getter 65:83
                    make_type!(x simple 86:86 #E)),
                make_stmt!(x fp 89:147 #expect_diagnostics 89:106
                    make_type!(path 117:147
                        make_path!(x segment simple 117:121 #crate),
                        make_path!(x segment simple 124:134 #diagnostics),
                        make_path!(x segment simple 137:147 #Diagnostics))),
                make_stmt!(x fp 150:163 #backtrace 150:158
                    make_type!(prim 161:163 U32)),
            ],
            ret_type: Some(make_type!(path 168:219
                make_path!(x segment generic 168:219 #Result 168:173 quote 174:219
                    make_type!(tuple 175:176),
                    make_type!(tuple 179:218
                        make_type!(x simple 180:180 #V),
                        make_type!(x simple 183:183 #V),
                        make_type!(path 186:217
                            make_path!(x segment generic 186:217 #SourceContext 186:198 quote 199:217
                                make_type!(x simple 200:216 #VirtualFileSystem))))))),
            wheres: vec![
                WhereClause{ span: Span::new(227, 251), name: IdSpan::new(make_isid!(x, V), Span::new(227, 227)), constraints: vec![
                    make_type!(x simple 230:238 #PartialEq),
                    make_type!(path 242:251
                        make_path!(x segment simple 242:244 #fmt),
                        make_path!(x segment simple 247:251 #Debug)),
                ]},
                WhereClause{ span: Span::new(254, 292), name: IdSpan::new(make_isid!(x, P), Span::new(254, 254)), constraints: vec![
                    make_type!(fn ret 257:292 paren 259:267 [
                        make_type!(fp 260:266
                            make_type!(ref 260:266 make_type!(x simple 261:266 #Parser))),
                    ], 
                    make_type!(path 272:292
                        make_path!(x segment generic 272:292 #Result 272:277 quote 278:292
                            make_type!(x simple 279:279 #V),
                            make_type!(x simple 282:291 #Unexpected)))),
                ]},
                WhereClause{ span: Span::new(295, 313), name: IdSpan::new(make_isid!(x, E), Span::new(295, 295)), constraints: vec![
                    make_type!(fn ret 298:313 paren 300:308 [
                        make_type!(fp 301:307
                            make_type!(ref 301:307 make_type!(x simple 302:307 #Parser))),
                    ],
                    make_type!(x simple 313:313 #V))
                ]},
            ],
            body: None,
        }
    }

    // TODO
    // case!{ parse_fn_def "fn emplace_tagged<T, U, I, W>(this: &This, wrap: W, init: I) -> TagIndex<U> where T: Sized, I: fn(&T), W: fn(Index<T>) -> U" }

    // // although currently I think fn type is type not class, and callobject is implementing std::ops::Call, not Fn
    // // I f**king human parse this code once and pass?
    //                   0         1         2         3         4         5         6         7
    //                   01234567890123456789012345678901234567890123456789012345678901234567890123
    case!{ parse_fn_def "fn map<T, U, F>(this: Option<T>, f: F) -> Option<U> where F: fn(T) -> U {}",
        |x| FnDef{
            span: Span::new(0, 73),
            name: make_stmt!(x name 3:14 #map 3:5 quote 6:14
                make_stmt!(x gp 7:7 #T),
                make_stmt!(x gp 10:10 #U),
                make_stmt!(x gp 13:13 #F)),
            quote_span: Span::new(15, 37),
            parameters: vec![
                make_stmt!(x fp 16:30 #this 16:19
                    make_type!(path 22:30
                        make_path!(x segment generic 22:30 #Option 22:27 quote 28:30
                            make_type!(x simple 29:29 #T)))),
                make_stmt!(x fp 33:36 #f 33:33
                    make_type!(x simple 36:36 #F)),
            ],
            ret_type: Some(make_type!(path 42:50
                make_path!(x segment generic 42:50 #Option 42:47 quote 48:50
                    make_type!(x simple 49:49 #U)))),
            wheres: vec![
                WhereClause{ span: Span::new(58, 70), name: IdSpan::new(make_isid!(x, F), Span::new(58, 58)), constraints: vec![
                    make_type!(fn ret 61:70 paren 63:65 [
                        make_type!(fp 64:64
                            make_type!(x simple 64:64 #T))
                    ], make_type!(x simple 70:70 #U))
                ]},
            ],
            body: Some(make_stmt!(block 72:73)),
        }
    }

    // unknown generic parameter, but that's not syntax error
    //                   012345678901234567
    case!{ parse_fn_def "fn f() where T: T;",
        |x| FnDef {
            span: Span::new(0, 17),
            name: make_stmt!(x name 3:3 #f),
            quote_span: Span::new(4, 5),
            parameters: Vec::new(),
            ret_type: None,
            wheres: vec![
                WhereClause{ span: Span::new(13, 16), name: IdSpan::new(make_isid!(x, T), Span::new(13, 13)), constraints: vec![
                    make_type!(x simple 16:16 #T)
                ]}
            ],
            body: None,
        }
    }
}

#[test]
fn parse_struct_def() {
    //                                  01234567890123456
    case!{ parse_struct_def "struct x { x: i32 }",
        |x| StructDef{
            span: Span::new(0, 18),
            name: make_stmt!(x name 7:7 #x),
            fields: vec![
                FieldDef{ span: Span::new(11, 16), name: make_stmt!(x id 11:11 #x), colon_span: Span::new(12, 12),
                    r#type: make_type!(prim 14:16 I32) }] }
    }

    //                     0123456789
    case!{ parse_struct_def "struct x<>{}",
        |x| StructDef{
            span: Span::new(0, 11),
            name: make_stmt!(x name 7:9 #x 7:7 quote 8:9),
            fields: Vec::new(),
        }, |e| e.emit(strings::EmptyGenericParameterList).span(Span::new(8, 9))
    }

    case!{ parse_struct_def "struct x { x: i32,}",
        |x| StructDef{
            span: Span::new(0, 18),
            name: make_stmt!(x name 7:7 #x),
            fields: vec![
                FieldDef{ span: Span::new(11, 16), name: make_stmt!(x id 11:11 #x), colon_span: Span::new(12, 12),
                    r#type: make_type!(prim 14:16 I32) }] }
    }
    //                     0         1         2         3         4
    //                     0123456789012345678901234567890123456789012345
    case!{ parse_struct_def "struct array { data:  &u8, size: u64, cap: u64 }",
        |x| StructDef{
            span: Span::new(0, 47),
            name: make_stmt!(x name 7:11 #array),
            fields: vec![
                FieldDef{ span: Span::new(15, 24), name: make_stmt!(x id 15:18 #data), colon_span: Span::new(19, 19),
                    r#type: make_type!(ref 22:24 make_type!(prim 23:24 U8)) },
                FieldDef{ span: Span::new(27, 35), name: make_stmt!(x id 27:30 #size), colon_span: Span::new(31, 31),
                    r#type: make_type!(prim 33:35 U64) },
                FieldDef{ span: Span::new(38, 45), name: make_stmt!(x id 38:40 #cap), colon_span: Span::new(41, 41),
                    r#type: make_type!(prim 43:45 U64) },
            ]
        }
    }

    //                     0         1         2         3         4
    //                     01234567890123456789012345678901234567890123456789
    case!{ parse_struct_def "struct list<T> { data: &T, size: usize, cap: usize }",
        |x| StructDef{
            span: Span::new(0, 51),
            name: make_stmt!(x name 7:13 #list 7:10 quote 11:13
                make_stmt!(x gp 12:12 #T)),
            fields: vec![
                FieldDef{ span: Span::new(17, 24), name: make_stmt!(x id 17:20 #data), colon_span: Span::new(21, 21),
                    r#type: make_type!(ref 23:24 make_type!(x simple 24:24 #T)) },
                FieldDef{ span: Span::new(27, 37), name: make_stmt!(x id 27:30 #size), colon_span: Span::new(31, 31),
                    r#type: make_type!(x simple 33:37 #usize) },
                FieldDef{ span: Span::new(40, 49), name: make_stmt!(x id 40:42 #cap), colon_span: Span::new(43, 43),
                    r#type: make_type!(x simple 45:49 #usize) },
            ]
        }
    }

    //                     0         1         2         3         4         5
    //                     012345678901234567890123456789012345678901234567890123456
    case!{ parse_struct_def "struct hashmap<K, V, A> { buckets: &&(K, V), allocator: A }",
        |x| StructDef{
            span: Span::new(0, 58),
            name: make_stmt!(x name 7:22 #hashmap 7:13 quote 14:22
                make_stmt!(x gp 15:15 #K),
                make_stmt!(x gp 18:18 #V),
                make_stmt!(x gp 21:21 #A)),
            fields: vec![
                FieldDef{ span: Span::new(26, 42), name: make_stmt!(x id 26:32 #buckets), colon_span: Span::new(33, 33),
                    r#type: make_type!(ref 35:42
                        make_type!(ref 36:42
                            make_type!(tuple 37:42
                                make_type!(x simple 38:38 #K),
                                make_type!(x simple 41:41 #V)))) },
                FieldDef{ span: Span::new(45, 56), name: make_stmt!(x id 45:53 #allocator), colon_span: Span::new(54, 54),
                    r#type: make_type!(x simple 56:56 #A) },
            ]
        }
    }
}

#[test]
fn parse_type_def() {

    //                       0123456789012
    case!{ parse_type_def "type a = i32;", |x|
        make_stmt!(type 0:12
            make_stmt!(x name 5:5 #a),
            make_type!(prim 9:11 I32)),
    }

    //                       0         1         2         3
    //                       012345678901234567890123456789012345
    case!{ parse_type_def "type Result<T> = Result<T, MyError>;", |x|
        make_stmt!(type 0:35
            make_stmt!(x name 5:13 #Result 5:10 quote 11:13
                make_stmt!(x gp 12:12 #T)),
            make_type!(path 17:34
                make_path!(x segment generic 17:34 #Result 17:22 quote 23:34
                    make_type!(x simple 24:24 #T),
                    make_type!(x simple 27:33 #MyError))))
    }

    case!{ parse_type_def "type a;", |x|
        make_stmt!(type 0:6
            make_stmt!(x name 5:5 #a))
    }

    case!{ parse_type_def "type Result<T>;", |x|
        make_stmt!(type 0:14
            make_stmt!(x name 5:13 #Result 5:10 quote 11:13
                make_stmt!(x gp 12:12 #T)))
    }
}

#[test]
fn parse_class_def() {
    //                      0         1         2         3         4         5
    //                      0123456789012345678901234567890123456789012345678901234
    case!{ parse_class_def "class Equal { fn eq(self: &Self, rhs: &Self) -> bool; }",
        |x| ClassDef{
            span: Span::new(0, 54),
            name: make_stmt!(x name 6:10 #Equal),
            quote_span: Span::new(12, 54),
            types: Vec::new(),
            functions: vec![
                FnDef{
                    span: Span::new(14, 52),
                    name: make_stmt!(x name 17:18 #eq),
                    quote_span: Span::new(19, 43),
                    parameters: vec![
                        make_stmt!(x fp 20:30 #self 20:23
                            make_type!(ref 26:30 make_type!(x simple 27:30 #Self))),
                        make_stmt!(x fp 33:42 #rhs 33:35
                            make_type!(ref 38:42 make_type!(x simple 39:42 #Self))),
                    ],
                    ret_type: Some(make_type!(prim 48:51 Bool)),
                    wheres: Vec::new(),
                    body: None,
                },
            ],
        }
    }

    //                      0         1         2         3         4         5         6         7
    //                      0123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_class_def "class Add<R> { type Result; fn add(self: Self, rhs: R) -> Self::Result; }",
        |x| ClassDef{
            span: Span::new(0, 72),
            name: make_stmt!(x name 6:11 #Add 6:8 quote 9:11
                make_stmt!(x gp 10:10 #R)),
            quote_span: Span::new(13, 72),
            types: vec![
                make_stmt!(type 15:26
                    make_stmt!(x name 20:25 #Result)),
            ],
            functions: vec![
                FnDef{
                    span: Span::new(28, 70),
                    name: make_stmt!(x name 31:33 #add),
                    quote_span: Span::new(34, 53),
                    parameters: vec![
                        make_stmt!(x fp 35:44 #self 35:38
                            make_type!(x simple 41:44 #Self)),
                        make_stmt!(x fp 47:52 #rhs 47:49
                            make_type!(x simple 52:52 #R)),
                    ],
                    ret_type: Some(make_type!(path 58:69
                        make_path!(x segment simple 58:61 #Self),
                        make_path!(x segment simple 64:69 #Result))),
                    wheres: Vec::new(),
                    body: None,
                }
            ],
        }
    }

    //                      0         1         2         3         4         5         6         7
    //                      0123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_class_def "class Iterable { type Item; fn next(self: &Self) -> Option<Self::Item>; }",
        |x| ClassDef{
            span: Span::new(0, 72),
            name: make_stmt!(x name 6:13 #Iterable),
            quote_span: Span::new(15, 72),
            types: vec![
                make_stmt!(type 17:26
                    make_stmt!(x name 22:25 #Item)),
            ],
            functions: vec![
                FnDef{
                    span: Span::new(28, 70),
                    name: make_stmt!(x name 31:34 #next),
                    quote_span: Span::new(35, 47),
                    parameters: vec![
                        make_stmt!(x fp 36:46 #self 36:39
                            make_type!(ref 42:46
                                make_type!(x simple 43:46 #Self))),
                    ],
                    ret_type: Some(make_type!(path 52:69
                        make_path!(x segment generic 52:69 #Option 52:57 quote 58:69
                            make_type!(path 59:68
                                make_path!(x segment simple 59:62 #Self),
                                make_path!(x segment simple 65:68 #Item))))),
                    wheres: Vec::new(),
                    body: None,
                }
            ],
        }
    }
}

#[test]
fn parse_impl() {
    //                 0         1         2         3         4         5         6         7         8         9         0         1         2         3         4
    //                 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012 34567 89012345678901
    case!{ parse_impl "impl<T> fmt::Debug for Vec<T> where T: fmt::Debug { fn fmt(self: &Self, f: &fmt::Formatter) { for item in self { write(f, \"{:?}\", item); } } }",
        |x| Implementation{
            span: Span::new(0, 141),
            parameters: vec![
                GenericParameter{ span: Span::new(5, 5), name: IdSpan::new(make_isid!(x, T), Span::new(5, 5)) },
            ],
            class: Some(make_type!(path 8:17
                make_path!(x segment simple 8:10 #fmt),
                make_path!(x segment simple 13:17 #Debug))),
            r#type: make_type!(path 23:28
                make_path!(x segment generic 23:28 #Vec 23:25 quote 26:28
                    make_type!(x simple 27:27 #T))),
            wheres: vec![
                WhereClause{ span: Span::new(36, 48), name: IdSpan::new(make_isid!(x, T), Span::new(36, 36)), constraints: vec![
                    make_type!(path 39:48
                        make_path!(x segment simple 39:41 #fmt),
                        make_path!(x segment simple 44:48 #Debug)),
                ]}
            ],
            quote_span: Span::new(50, 141),
            types: Vec::new(),
            functions: vec![
                FnDef{
                    span: Span::new(52, 139),
                    name: make_stmt!(x name 55:57 #fmt),
                    quote_span: Span::new(58, 90),
                    parameters: vec![
                        make_stmt!(x fp 59:69 #self 59:62
                            make_type!(ref 65:69
                                make_type!(x simple 66:69 #Self))),
                        make_stmt!(x fp 72:89 #f 72:72
                            make_type!(ref 75:89
                                make_type!(path 76:89
                                    make_path!(x segment simple 76:78 #fmt),
                                    make_path!(x segment simple 81:89 #Formatter)))),
                    ],
                    ret_type: None,
                    wheres: Vec::new(),
                    body: Some(make_stmt!(block 92:139
                        make_stmt!(x for 94:137 var #item 98:101 None,
                            make_expr!(x path simple 106:109 #self),
                        make_stmt!(block 111:137
                            make_stmt!(expr 113:135
                                make_expr!(call 113:134 paren 118:134
                                    make_expr!(x path simple 113:117 #write),
                                    make_expr!(x path simple 119:119 #f),
                                    make_expr!(x str #"{:?}" 122:127),
                                    make_expr!(x path simple 130:133 #item))))))),
                }
            ],
        }
    }
}
