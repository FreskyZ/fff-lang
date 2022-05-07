use super::*;

#[test]
fn parse_block_stmt() {
    case!{ parse_block_stmt(None) "{}", |x| 
        make_stmt!(x block 0:1 None,
            make_stmt!(x body 0:1)),
    }

    case!{ parse_labeled_stmt_as_stmt "@: {}", |x| 
        make_stmt!(x block 0:4
            make_stmt!(x label 0:0 #""),
            make_stmt!(x body 3:4)),
    }
}

#[test]
fn expr_stmt_parse() {
    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ parse_expr_stmt_as_stmt "writeln(\"helloworld\");", |x|
        make_stmt!(x expr 0:21
            make_expr!(x call 0:20 paren 7:20
                make_expr!(x path simple 0:6 #writeln),
                make_expr!(x str #"helloworld" 8:19)))
    }

    //                      012345678901
    case!{ parse_expr_stmt_as_stmt "1 + 1 <<= 2;", |x| // to show I have 3 char Separator available
       make_stmt!(x assign 0:11 LtLtEq 6:8
            make_expr!(x binary 0:4 Add 2:2
                make_expr!(x i32 1 0:0),
                make_expr!(x i32 1 4:4)),
            make_expr!(x i32 2 10:10)),
    }
}

#[test]
fn parse_for_stmt() {
    //                      0123456789012345678
    case!{ parse_labeled_stmt_as_stmt "@2: for i in 42 {}", |x|
        make_stmt!(x for 0:17 var #i 8:8
            make_stmt!(x label 0:1 #"2"),
            make_expr!(x i32 42 13:14),
            make_stmt!(x body 16:17))
    }

    //              0         1         2         3         4         5         6         7
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    case!{ parse_labeled_stmt_as_stmt "@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }", |x|
        make_stmt!(x for 0:77 var #"_" 12:12
            make_stmt!(x label 0:5 #hello),
            make_expr!(x call 17:50 paren 49:50
                make_expr!(x member 17:48 dot 41:41 #reverse 42:48
                    make_expr!(x call 17:40 paren 39:40
                        make_expr!(x member 17:38 dot 29:29 #enumerate 30:38
                            make_expr!(x call 17:28 paren 22:28
                                make_expr!(x path simple 17:21 #range),
                                make_expr!(x i32 0 23:23),
                                make_expr!(x i32 10 26:27))),)),),
            make_stmt!(x body 52:77
                make_stmt!(x expr 54:75
                    make_expr!(x call 54:74 paren 61:74
                        make_expr!(x path simple 54:60 #writeln),
                        make_expr!(x str #"helloworld" 62:73)))))
    }
}

#[test]
fn if_stmt_parse() {
    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ parse_if_stmt "if true { } else if false { } else {}", |x|
        make_stmt!(x if 0:36
            make_stmt!(x if clause 0:10
                make_expr!(x true 3:6),
                make_stmt!(x body 8:10)),
            make_stmt!(x if clause 12:28
                make_expr!(x false 20:24),
                make_stmt!(x body 26:28)),
            else: Some(make_stmt!(x else 30:36
                make_stmt!(x body 35:36)))),
    }

    //                    0         1         2         3         4         5         6         7
    //                    012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ parse_if_stmt "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}", |x|
        make_stmt!(x if 0:71
            make_stmt!(x if clause 0:41
                make_expr!(x i32 1 3:3),
                make_stmt!(x body 5:41
                    make_stmt!(x expr 7:20
                        make_expr!(x call 7:19 paren 17:19
                            make_expr!(x member 7:16 dot 10:10 #do_sth 11:16
                                make_expr!(x path simple 7:9 #sth)),
                            make_expr!(x path simple 18:18 #a))),
                    make_stmt!(x expr 22:39
                        make_expr!(x call 22:38 paren 36:38
                            make_expr!(x member 22:35 dot 27:27 #do_other 28:35
                                make_expr!(x path simple 22:26 #other)),
                            make_expr!(x path simple 37:37 #b))))),
            else: Some(make_stmt!(x else 43:71
                make_stmt!(x body 48:71
                    make_stmt!(x expr 50:70
                        make_expr!(x call 50:69 paren 61:69
                            make_expr!(x member 50:60 dot 57:57 #map 58:60
                                make_expr!(x array 50:56
                                    make_expr!(x i32 1 51:51),
                                    make_expr!(x i32 2 53:53),
                                    make_expr!(x i32 3 55:55))),
                            make_expr!(x path simple 62:68 #writeln))))))),
    }

    // if condition does not expect object literal, unless parened
    //      0         1         2
    //      012345678901234567890123
    case!{ parse_if_stmt "if a {} else if (b{}) {}", |x|
        make_stmt!(x if 0:23
            make_stmt!(x if clause 0:6
                make_expr!(x path simple 3:3 #a),
                make_stmt!(x body 5:6)),
            make_stmt!(x if clause 8:23
                make_expr!(x paren 16:20
                    make_expr!(x object 17:19 quote 18:19
                        make_expr!(x path simple 17:17 #b),)),
                make_stmt!(x body 22:23)),
            else: None),
    }
}

#[test]
fn jump_stmt_parse() {

    case!{ parse_continue_stmt "continue;", |x|
        make_stmt!(x continue 0:8 make_stmt!(label none)) }
    case!{ parse_continue_stmt "continue @1;", |x|
        make_stmt!(x continue 0:11 make_stmt!(x label 9:10 #"1")) }

    case!{ parse_break_stmt "break;", |x|
        make_stmt!(x break 0:5 make_stmt!(label none)) }
    case!{ parse_break_stmt "break @1;", |x|
        make_stmt!(x break 0:8 make_stmt!(x label 6:7 #"1")) }
}

#[test]
fn parse_loop_stmt() {

    case!{ parse_loop_stmt(None) "loop {}", |x|
        make_stmt!(x loop 0:6
            make_stmt!(label none),
            make_stmt!(x body 5:6))
    }
    //                                        1234567890123456789 0123 45678
    case!{ parse_labeled_stmt_as_stmt "@@: loop { println(\"233\"); }", |x|
        make_stmt!(x loop 0:27
            make_stmt!(x label 0:1 #"@"),
            make_stmt!(x body 9:27
                make_stmt!(x expr 11:25
                    make_expr!(x call 11:24 paren 18:24
                        make_expr!(x path simple 11:17 #println),
                        make_expr!(x str #"233" 19:23)))))
    }
}

#[test]
fn parse_module_stmt() {

    case!{ parse_module_stmt "module a;", |x| 
        make_stmt!(x import 0:8 #a 7:7 None),
    }
    //                   012345678901234567890
    case!{ parse_module_stmt "module os \"windows\";", |x|
        make_stmt!(x import 0:19 #os 7:8
            Some(make_stmt!(x id 10:18 #"windows"))),
    }

    //      0         1          2
    //      012345678901234567 89012345 6
    case!{ parse_module_stmt "module otherdir r\"ab/c.f3\";", |x|
        make_stmt!(x import 0:26 #otherdir 7:14
            Some(make_stmt!(x id 16:25 #"ab/c.f3"))),
    }
}

#[test]
fn parse_module() {
    //                      0123456789012345678901234
    case!{ parse_module "use a; module b; 3; b; a;", |x|
        make_stmt!(x module 1
            make_stmt!(x uses item 0:5 make_path!(x simple 4:4 #a)),
            make_stmt!(x import item 7:15 #b 14:14 None),
            make_stmt!(x expr item 17:18 make_expr!(x i32 3 17:17)),
            make_stmt!(x expr item 20:21 make_expr!(x path simple 20:20 #b)),
            make_stmt!(x expr item 23:24 make_expr!(x path simple 23:23 #a))),
    }
}

#[test]
fn ret_stmt_parse() {

    case!{ parse_ret_stmt "return;", |x| make_stmt!(x ret 0:6 None) }

    case!{ parse_ret_stmt "return 1 + 1;", |x|
        make_stmt!(x ret 0:12
            Some(make_expr!(x binary 7:11 Add 9:9
                make_expr!(x i32 1 7:7),
                make_expr!(x i32 1 11:11)))),
    }
}

#[test]
fn use_stmt_parse() {

    case!{ parse_use_stmt "use a;", |x| 
        make_stmt!(x uses 0:5
            make_path!(x simple 4:4 #a)),
    }
    //                   0123456789012345678901234567890
    case!{ parse_use_stmt "use std::fmt::Debug as Display;", |x| 
        make_stmt!(x uses 0:30 #Display 23:29
            make_path!(x 4:18
                make_path!(x segment simple 4:6 #std),
                make_path!(x segment simple 9:11 #fmt),
                make_path!(x segment simple 14:18 #Debug))),
    }
}

#[test]
fn var_decl_stmt_parse() {
    //                                           12345678901234
    case!{ parse_var_decl "const abc = 0;", |x|
        make_stmt!(x const 0:13 #abc 6:8
            None,
            Some(make_expr!(x i32 0 12:12)))
    }

    //                                           0        1
    //                                           12345678901234567890
    case!{ parse_var_decl "var hij = [1, 3, 5];", |x|
        make_stmt!(x var 0:19 #hij 4:6
            None,
            Some(make_expr!(x array 10:18
                make_expr!(x i32 1 11:11),
                make_expr!(x i32 3 14:14),
                make_expr!(x i32 5 17:17))))
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
            Some(make_type!(x array 9:22
                make_type!(x tuple 10:19
                    make_type!(x prim 11:12 U8),
                    make_type!(x prim 15:18 Char)),
                make_expr!(x i32 1 21:21))),
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
            Some(make_type!(x tuple 9:21
                make_type!(x array 10:15
                    make_type!(x prim 11:12 U8),
                    make_expr!(x i32 3 14:14)),
                make_type!(x prim 18:20 U32))),
            Some(make_expr!(x tuple 25:48
                make_expr!(x array 26:42
                    make_expr!(x u8 1 27:29),
                    make_expr!(x u8 5 32:34),
                    make_expr!(x u8 7 37:41)),
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

    case!{ parse_while_stmt(None) "while 1 {}", |x|
        make_stmt!(x while 0:9
            make_stmt!(label none),
            make_expr!(x i32 1 6:6),
            make_stmt!(x body 8:9))
    }
    //      0        1         2         3         4
    //      01234567890123456789012345 67890123456789012 3456
    case!{ parse_labeled_stmt_as_stmt "@2: while true { writeln(\"fresky hellooooo\"); }", |x|
        make_stmt!(x while 0:46
            make_stmt!(x label 0:1 #"2"),
            make_expr!(x true 10:13),
            make_stmt!(x body 15:46
                make_stmt!(x expr 17:44
                    make_expr!(x call 17:43 paren 24:43
                        make_expr!(x path simple 17:23 #writeln),
                        make_expr!(x str #"fresky hellooooo" 25:42)))))
    }
}

#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ parse_enum_def "enum E { M1, M2 = 1,}", |x|
        make_stmt!(x enum 0:20 quote 7:20
            make_stmt!(x id 5:5 #E),
            None,
            make_stmt!(x variant 9:10 #M1 9:10),
            make_stmt!(x variant 13:18 #M2 13:14
                make_expr!(x i32 1 18:18))),
    }

    //      0123456789012
    case!{ parse_enum_def "enum E: u8 {}", |x| 
        make_stmt!(x enum 0:12 quote 11:12
            make_stmt!(x id 5:5 #E),
            Some(make_type!(x prim bare 8:9 U8)),),
    }
}

#[test]
fn parse_fn_def() {
    //                                012345678901
    case!{ parse_fn_def "fn main() {}", |x|
        make_stmt!(x fn 0:11 quote 7:8
            name: make_stmt!(x name 3:6 #main),
            parameters: vec![],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 10:11))),
    }

    case!{ parse_fn_def "fn main();", |x|
        make_stmt!(x fn 0:9 quote 7:8
            name: make_stmt!(x name 3:6 #main),
            parameters: vec![],
            ret_type: None,
            wheres: Vec::new(),
            body: None)
    }

    //                      0        1
    //                      0123456789012345678
    case!{ parse_fn_def "fn main(ac: i32) {}", |x|
        make_stmt!(x fn 0:18 quote 7:15
            name: make_stmt!(x name 3:6 #main),
            parameters: vec![
                make_stmt!(x fp 8:14 #ac 8:9
                    make_type!(x prim 12:14 I32)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 17:18))),
    }

    //      0         1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def " fn mainxxx(argv:&    string   ,this:i32, some_other: char, )  { println(this); }", |x|
        make_stmt!(x fn 1:80 quote 11:60
            name: make_stmt!(x name 4:10 #mainxxx),
            parameters: vec![
                make_stmt!(x fp 12:27 #argv 12:15
                    make_type!(x ref 17:27 make_type!(x simple 22:27 #string))),
                make_stmt!(x fp 32:39 #this 32:35
                    make_type!(x prim 37:39 I32)),
                make_stmt!(x fp 42:57 #some_other 42:51
                    make_type!(x prim 54:57 Char)),
            ],
            ret_type: None,
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 63:80
                make_stmt!(x expr 65:78
                    make_expr!(x call 65:77 paren 72:77
                        make_expr!(x path simple 65:71 #println),
                        make_expr!(x path simple 73:76 #this))))))
    }

    //                                0        1
    //                                1234567890123456789
    case!{ parse_fn_def "fn main() -> i32 {}", |x|
        make_stmt!(x fn 0:18 quote 7:8
            name: make_stmt!(x name 3:6 #main),
            parameters: vec![],
            ret_type: Some(make_type!(x prim 13:15 I32)),
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 17:18))),
    }
    case!{ parse_fn_def "fn main() -> i32;", |x|
        make_stmt!(x fn 0:16 quote 7:8
            name: make_stmt!(x name 3:6 #main),
            parameters: vec![],
            ret_type: Some(make_type!(x prim 13:15 I32)),
            wheres: Vec::new(),
            body: None),
    }
    //      0         1         2         3         4         5         6
    //      0123456789012345678901234567890123456789012345678901234567890
    case!{ parse_fn_def "fn ffff(argc: i32, argv: &&byte,   envv:  &string,) -> i32 {}", |x|
        make_stmt!(x fn 0:60 quote 7:50
            name: make_stmt!(x name 3:6 #ffff),
            parameters: vec![
                make_stmt!(x fp 8:16 #argc 8:11
                    make_type!(x prim 14:16 I32)),
                make_stmt!(x fp 19:30 #argv 19:22
                    make_type!(x ref 25:30 make_type!(x ref 26:30 make_type!(x simple 27:30 #byte)))),
                make_stmt!(x fp 35:48 #envv 35:38
                    make_type!(x ref 42:48 make_type!(x simple 43:48 #string))),
            ],
            ret_type: Some(make_type!(x prim 55:57 I32)),
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 59:60)))
    }

    //                   0         1         2         3         4         5         6         7         8         9         0         1         2
    //                   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ parse_fn_def "fn impl_visit(this: &This, node: &Node, title: string, span: Span, then: fn() -> Result<&This, fmt::Error>) -> fmt::Result {}", |x|
        make_stmt!(x fn 0:124 quote 13:106
            name: make_stmt!(x name 3:12 #impl_visit),
            parameters: vec![
                make_stmt!(x fp 14:24 #this 14:17
                    make_type!(x ref 20:24 make_type!(x simple 21:24 #This))),
                make_stmt!(x fp 27:37 #node 27:30
                    make_type!(x ref 33:37 make_type!(x simple 34:37 #Node))),
                make_stmt!(x fp 40:52 #title 40:44
                    make_type!(x simple 47:52 #string)),
                make_stmt!(x fp 55:64 #span 55:58
                    make_type!(x simple 61:64 #Span)),
                make_stmt!(x fp 67:105 #then 67:70
                    make_type!(x fn ret 73:105 paren 75:76 [],
                        make_type!(x path 81:105
                            make_path!(x segment generic 81:105 #Result 81:86 quote 87:105
                                make_type!(x ref 88:92 make_type!(x simple 89:92 #This)),
                                make_type!(x path 95:104
                                    make_path!(x segment simple 95:97 #fmt),
                                    make_path!(x segment simple 100:104 #Error)))))),
            ],
            ret_type: Some(make_type!(x path 111:121
                make_path!(x segment simple 111:113 #fmt),
                make_path!(x segment simple 116:121 #Result))),
            wheres: Vec::new(),
            body: Some(make_stmt!(x body 123:124)))
    }

    // this was from the method inside the case! macro for some time
    //                   0         1         2         3         4         5         6         7         8  
    //                   0123456789012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ parse_fn_def "fn case_until_node<N, F>(input: &string, f: F, expect_node: N, expect_diagnostics:
        crate::diagnostics::Diagnostics, expect_strings: &string, backtrace: u32) -> Result<(), (N, N, SourceContext<VirtualFileSystem>)> where N: PartialEq + fmt::Debug, F: fn(&Parser) -> Result<N, Unexpected>;", |x|
        // 45678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
        // 9    10        11        12        13        14        15        16        17        18        19        20        21        22        23        24        25        26        27        28        29
        make_stmt!(x fn 0:293 quote 24:163
            name: make_stmt!(x name 3:23 #case_until_node 3:17 quote 18:23
                make_stmt!(x gp 19:19 #N),
                make_stmt!(x gp 22:22 #F)),
            parameters: vec![
                make_stmt!(x fp 25:38 #input 25:29
                    make_type!(x ref 32:38 make_type!(x simple 33:38 #string))),
                make_stmt!(x fp 41:44 #f 41:41
                    make_type!(x simple 44:44 #F)),
                make_stmt!(x fp 47:60 #expect_node 47:57
                    make_type!(x simple 60:60 #N)),
                make_stmt!(x fp 63:121 #expect_diagnostics 63:80
                    make_type!(x path 91:121
                        make_path!(x segment simple 91:95 #crate),
                        make_path!(x segment simple 98:108 #diagnostics),
                        make_path!(x segment simple 111:121 #Diagnostics))),
                make_stmt!(x fp 124:146 #expect_strings 124:137
                    make_type!(x ref 140:146 make_type!(x simple 141:146 #string))),
                make_stmt!(x fp 149:162 #backtrace 149:157
                    make_type!(x prim 160:162 U32)),
            ],
            ret_type: Some(make_type!(x path 168:219
                make_path!(x segment generic 168:219 #Result 168:173 quote 174:219
                    make_type!(x tuple 175:176),
                    make_type!(x tuple 179:218
                        make_type!(x simple 180:180 #N),
                        make_type!(x simple 183:183 #N),
                        make_type!(x path 186:217
                            make_path!(x segment generic 186:217 #SourceContext 186:198 quote 199:217
                                make_type!(x simple 200:216 #VirtualFileSystem))))))),
            wheres: vec![
                make_stmt!(x where 227:251 #N 227:227
                    make_type!(x simple 230:238 #PartialEq),
                    make_type!(x path 242:251
                        make_path!(x segment simple 242:244 #fmt),
                        make_path!(x segment simple 247:251 #Debug))),
                make_stmt!(x where 254:292 #F 254:254
                    make_type!(x fn ret 257:292 paren 259:267 [
                        make_type!(x fp 260:266
                            make_type!(x ref 260:266 make_type!(x simple 261:266 #Parser))),
                    ], 
                    make_type!(x path 272:292
                        make_path!(x segment generic 272:292 #Result 272:277 quote 278:292
                            make_type!(x simple 279:279 #N),
                            make_type!(x simple 282:291 #Unexpected))))),
            ],
            body: None),
    }

    // this is from the method inside the case! macro for now, currently
    //                   0         1         2         3         4         5         6         7         8         9        10
    //                   012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
    case!{ parse_fn_def "fn case_until_node<V, P, E>(input: &str, actual_value_getter: P, expect_value_getter: E, expect_diagnostics:
        crate::diagnostics::Diagnostics, backtrace: u32) ->Result<(), (V, V, SourceContext<VirtualFileSystem>)> where V: PartialEq + fmt::Debug, P: fn(&Parser) -> Result<V, Unexpected>, E: fn(&Parser) -> V;", |x|
        // 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
        // 12       13        14        15        16        17        18        19        20        21        22        23        24        25        26        27        28        29        30        31
        make_stmt!(x fn 0:314 quote 27:164
            name: make_stmt!(x name 3:26 #case_until_node 3:17 quote 18:26
                make_stmt!(x gp 19:19 #V),
                make_stmt!(x gp 22:22 #P),
                make_stmt!(x gp 25:25 #E)),
            parameters: vec![
                make_stmt!(x fp 28:38 #input 28:32
                    make_type!(x ref 35:38 make_type!(x simple 36:38 #str))),
                make_stmt!(x fp 41:62 #actual_value_getter 41:59
                    make_type!(x simple 62:62 #P)),
                make_stmt!(x fp 65:86 #expect_value_getter 65:83
                    make_type!(x simple 86:86 #E)),
                make_stmt!(x fp 89:147 #expect_diagnostics 89:106
                    make_type!(x path 117:147
                        make_path!(x segment simple 117:121 #crate),
                        make_path!(x segment simple 124:134 #diagnostics),
                        make_path!(x segment simple 137:147 #Diagnostics))),
                make_stmt!(x fp 150:163 #backtrace 150:158
                    make_type!(x prim 161:163 U32)),
            ],
            ret_type: Some(make_type!(x path 168:219
                make_path!(x segment generic 168:219 #Result 168:173 quote 174:219
                    make_type!(x tuple 175:176),
                    make_type!(x tuple 179:218
                        make_type!(x simple 180:180 #V),
                        make_type!(x simple 183:183 #V),
                        make_type!(x path 186:217
                            make_path!(x segment generic 186:217 #SourceContext 186:198 quote 199:217
                                make_type!(x simple 200:216 #VirtualFileSystem))))))),
            wheres: vec![
                make_stmt!(x where 227:251 #V 227:227
                    make_type!(x simple 230:238 #PartialEq),
                    make_type!(x path 242:251
                        make_path!(x segment simple 242:244 #fmt),
                        make_path!(x segment simple 247:251 #Debug))),
                make_stmt!(x where 254:292 #P 254:254
                    make_type!(x fn ret 257:292 paren 259:267 [
                        make_type!(x fp 260:266
                            make_type!(x ref 260:266 make_type!(x simple 261:266 #Parser))),
                        ],
                        make_type!(x path 272:292
                            make_path!(x segment generic 272:292 #Result 272:277 quote 278:292
                                make_type!(x simple 279:279 #V),
                                make_type!(x simple 282:291 #Unexpected))))),
                make_stmt!(x where 295:313 #E 295:295
                    make_type!(x fn ret 298:313 paren 300:308 [
                        make_type!(x fp 301:307
                            make_type!(x ref 301:307 make_type!(x simple 302:307 #Parser))),
                    ],
                    make_type!(x simple 313:313 #V))),
            ],
            body: None),
    }

    // TODO
    // case!{ parse_fn_def "fn emplace_tagged<T, U, I, W>(this: &This, wrap: W, init: I) -> TagIndex<U> where T: Sized, I: fn(&T), W: fn(Index<T>) -> U" }
    // and the current tag_case, notag_case and novisit_case

    // // although currently I think fn type is type not class, and callobject is implementing std::ops::Call, not Fn
    // // I f**king human parse this code once and pass?
    //                   0         1         2         3         4         5         6         7
    //                   01234567890123456789012345678901234567890123456789012345678901234567890123
    case!{ parse_fn_def "fn map<T, U, F>(this: Option<T>, f: F) -> Option<U> where F: fn(T) -> U {}", |x|
        make_stmt!(x fn 0:73 quote 15:37
            name: make_stmt!(x name 3:14 #map 3:5 quote 6:14
                make_stmt!(x gp 7:7 #T),
                make_stmt!(x gp 10:10 #U),
                make_stmt!(x gp 13:13 #F)),
            parameters: vec![
                make_stmt!(x fp 16:30 #this 16:19
                    make_type!(x path 22:30
                        make_path!(x segment generic 22:30 #Option 22:27 quote 28:30
                            make_type!(x simple 29:29 #T)))),
                make_stmt!(x fp 33:36 #f 33:33
                    make_type!(x simple 36:36 #F)),
            ],
            ret_type: Some(make_type!(x path 42:50
                make_path!(x segment generic 42:50 #Option 42:47 quote 48:50
                    make_type!(x simple 49:49 #U)))),
            wheres: vec![
                make_stmt!(x where 58:70 #F 58:58
                    make_type!(x fn ret 61:70 paren 63:65 [
                        make_type!(x fp 64:64
                            make_type!(x simple 64:64 #T))
                    ], make_type!(x simple 70:70 #U))),
            ],
            body: Some(make_stmt!(x body 72:73))),
    }

    // unknown generic parameter, but that's not syntax error
    //                   012345678901234567
    case!{ parse_fn_def "fn f() where T: T;", |x|
        make_stmt!(x fn 0:17 quote 4:5
            name: make_stmt!(x name 3:3 #f),
            parameters: Vec::new(),
            ret_type: None,
            wheres: vec![
                make_stmt!(x where 13:16 #T 13:13
                    make_type!(x simple 16:16 #T)),
            ],
            body: None)
    }
}

#[test]
fn parse_struct_def() {
    //                                  01234567890123456
    case!{ parse_struct_def "struct x { x: i32 }", |x|
        make_stmt!(x struct 0:18
            make_stmt!(x name 7:7 #x),
            make_stmt!(x struct field 11:16 #x 11:11 colon 12:12
                make_type!(x prim 14:16 I32))),
    }

    //                     0123456789
    case!{ parse_struct_def "struct x<>{}", |x| 
        make_stmt!(x struct 0:11
            make_stmt!(x name 7:9 #x 7:7 quote 8:9),),
        |e| e.emit(strings::EmptyGenericParameterList).span(Span::new(8, 9))
    }

    case!{ parse_struct_def "struct x { x: i32,}", |x| 
        make_stmt!(x struct 0:18
            make_stmt!(x name 7:7 #x),
            make_stmt!(x struct field 11:16 #x 11:11 colon 12:12
                make_type!(x prim 14:16 I32))),
    }
    //                     0         1         2         3         4
    //                     0123456789012345678901234567890123456789012345
    case!{ parse_struct_def "struct array { data:  &u8, size: u64, cap: u64 }", |x|
        make_stmt!(x struct 0:47
            make_stmt!(x name 7:11 #array),
            make_stmt!(x struct field 15:24 #data 15:18 colon 19:19
                make_type!(x ref 22:24 make_type!(x prim 23:24 U8))),
            make_stmt!(x struct field 27:35 #size 27:30 colon 31:31
                make_type!(x prim 33:35 U64)),
            make_stmt!(x struct field 38:45 #cap 38:40 colon 41:41
                make_type!(x prim 43:45 U64))),
    }

    //                     0         1         2         3         4
    //                     01234567890123456789012345678901234567890123456789
    case!{ parse_struct_def "struct list<T> { data: &T, size: usize, cap: usize }", |x|
        make_stmt!(x struct 0:51
            make_stmt!(x name 7:13 #list 7:10 quote 11:13
                make_stmt!(x gp 12:12 #T)),
            make_stmt!(x struct field 17:24 #data 17:20 colon 21:21
                make_type!(x ref 23:24 make_type!(x simple 24:24 #T))),
            make_stmt!(x struct field 27:37 #size 27:30 colon 31:31
                make_type!(x simple 33:37 #usize)),
            make_stmt!(x struct field 40:49 #cap 40:42 colon 43:43
                make_type!(x simple 45:49 #usize))),
    }

    //                     0         1         2         3         4         5
    //                     012345678901234567890123456789012345678901234567890123456
    case!{ parse_struct_def "struct hashmap<K, V, A> { buckets: &&(K, V), allocator: A }", |x|
        make_stmt!(x struct 0:58
            make_stmt!(x name 7:22 #hashmap 7:13 quote 14:22
                make_stmt!(x gp 15:15 #K),
                make_stmt!(x gp 18:18 #V),
                make_stmt!(x gp 21:21 #A)),
            make_stmt!(x struct field 26:42 #buckets 26:32 colon 33:33
                make_type!(x ref 35:42
                    make_type!(x ref 36:42
                        make_type!(x tuple 37:42
                            make_type!(x simple 38:38 #K),
                            make_type!(x simple 41:41 #V))))),
            make_stmt!(x struct field 45:56 #allocator 45:53 colon 54:54
                make_type!(x simple 56:56 #A))),
    }
}

#[test]
fn parse_type_def() {

    //                       0123456789012
    case!{ parse_type_def "type a = i32;", |x|
        make_stmt!(x type 0:12
            make_stmt!(x name 5:5 #a),
            make_type!(x prim 9:11 I32)),
    }

    //                       0         1         2         3
    //                       012345678901234567890123456789012345
    case!{ parse_type_def "type Result<T> = Result<T, MyError>;", |x|
        make_stmt!(x type 0:35
            make_stmt!(x name 5:13 #Result 5:10 quote 11:13
                make_stmt!(x gp 12:12 #T)),
            make_type!(x path 17:34
                make_path!(x segment generic 17:34 #Result 17:22 quote 23:34
                    make_type!(x simple 24:24 #T),
                    make_type!(x simple 27:33 #MyError))))
    }

    case!{ parse_type_def "type a;", |x|
        make_stmt!(x type 0:6
            make_stmt!(x name 5:5 #a))
    }

    case!{ parse_type_def "type Result<T>;", |x|
        make_stmt!(x type 0:14
            make_stmt!(x name 5:13 #Result 5:10 quote 11:13
                make_stmt!(x gp 12:12 #T)))
    }
}

#[test]
fn parse_class_def() {
    //                      0         1         2         3         4         5
    //                      0123456789012345678901234567890123456789012345678901234
    case!{ parse_class_def "class Equal { fn eq(self: &Self, rhs: &Self) -> bool; }", |x|
        make_stmt!(x class 0:54 quote 12:54
            make_stmt!(x name 6:10 #Equal),
            make_stmt!(x fn item 14:52 quote 19:43
                name: make_stmt!(x name 17:18 #eq),
                parameters: vec![
                    make_stmt!(x fp 20:30 #self 20:23
                        make_type!(x ref 26:30 make_type!(x simple 27:30 #Self))),
                    make_stmt!(x fp 33:42 #rhs 33:35
                        make_type!(x ref 38:42 make_type!(x simple 39:42 #Self))),
                ],
                ret_type: Some(make_type!(x prim 48:51 Bool)),
                wheres: Vec::new(),
                body: None)),
    }

    //                      0         1         2         3         4         5         6         7
    //                      0123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_class_def "class Add<R> { type Result; fn add(self: Self, rhs: R) -> Self::Result; }", |x|
        make_stmt!(x class 0:72 quote 13:72
            make_stmt!(x name 6:11 #Add 6:8 quote 9:11
                make_stmt!(x gp 10:10 #R)),
            make_stmt!(x type item 15:26
                make_stmt!(x name 20:25 #Result)),
            make_stmt!(x fn item 28:70 quote 34:53
                name: make_stmt!(x name 31:33 #add),
                parameters: vec![
                    make_stmt!(x fp 35:44 #self 35:38
                        make_type!(x simple 41:44 #Self)),
                    make_stmt!(x fp 47:52 #rhs 47:49
                        make_type!(x simple 52:52 #R)),
                ],
                ret_type: Some(make_type!(x path 58:69
                    make_path!(x segment simple 58:61 #Self),
                    make_path!(x segment simple 64:69 #Result))),
                wheres: Vec::new(),
                body: None)),
    }

    //                      0         1         2         3         4         5         6         7
    //                      0123456789012345678901234567890123456789012345678901234567890123456789012
    case!{ parse_class_def "class Iterable { type Item; fn next(self: &Self) -> Option<Self::Item>; }", |x|
        make_stmt!(x class 0:72 quote 15:72
            make_stmt!(x name 6:13 #Iterable),
            make_stmt!(x type item 17:26
                make_stmt!(x name 22:25 #Item)),
            make_stmt!(x fn item 28:70 quote 35:47
                name: make_stmt!(x name 31:34 #next),
                parameters: vec![
                    make_stmt!(x fp 36:46 #self 36:39
                        make_type!(x ref 42:46
                            make_type!(x simple 43:46 #Self))),
                ],
                ret_type: Some(make_type!(x path 52:69
                    make_path!(x segment generic 52:69 #Option 52:57 quote 58:69
                        make_type!(x path 59:68
                            make_path!(x segment simple 59:62 #Self),
                            make_path!(x segment simple 65:68 #Item))))),
                wheres: Vec::new(),
                body: None)),
    }
}

#[test]
fn parse_impl_block() {
    //                 0         1         2         3         4         5         6         7         8         9         0         1         2         3         4
    //                 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012 34567 89012345678901
    case!{ parse_impl_block "impl<T> fmt::Debug for Vec<T> where T: fmt::Debug { fn fmt(self: &Self, f: &fmt::Formatter) { for item in self { write(f, \"{:?}\", item); } } }", |x|
        make_stmt!(x impl 0:141
            parameters: vec![
                make_stmt!(x gp 5:5 #T),
            ],
            class: Some(make_type!(x path 8:17
                make_path!(x segment simple 8:10 #fmt),
                make_path!(x segment simple 13:17 #Debug))),
            r#type: make_type!(x path 23:28
                make_path!(x segment generic 23:28 #Vec 23:25 quote 26:28
                    make_type!(x simple 27:27 #T))),
            wheres: vec![
                make_stmt!(x where 36:48 #T 36:36
                    make_type!(x path 39:48
                        make_path!(x segment simple 39:41 #fmt),
                        make_path!(x segment simple 44:48 #Debug))),
            ],
            quote_span: Span::new(50, 141),
            items: vec![
                make_stmt!(x fn item 52:139 quote 58:90
                    name: make_stmt!(x name 55:57 #fmt),
                    parameters: vec![
                        make_stmt!(x fp 59:69 #self 59:62
                            make_type!(x ref 65:69
                                make_type!(x simple 66:69 #Self))),
                        make_stmt!(x fp 72:89 #f 72:72
                            make_type!(x ref 75:89
                                make_type!(x path 76:89
                                    make_path!(x segment simple 76:78 #fmt),
                                    make_path!(x segment simple 81:89 #Formatter)))),
                    ],
                    ret_type: None,
                    wheres: Vec::new(),
                    body: Some(make_stmt!(x body 92:139
                        make_stmt!(x for 94:137 var #item 98:101 None,
                            make_expr!(x path simple 106:109 #self),
                        make_stmt!(x body 111:137
                            make_stmt!(x expr 113:135
                                make_expr!(x call 113:134 paren 118:134
                                    make_expr!(x path simple 113:117 #write),
                                    make_expr!(x path simple 119:119 #f),
                                    make_expr!(x str #"{:?}" 122:127),
                                    make_expr!(x path simple 130:133 #item)))))))),
            ]),
    }
}
