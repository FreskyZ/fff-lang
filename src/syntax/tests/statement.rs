use super::*;

#[test]
fn block_stmt_parse() {
    case!{ parse_block_stmt "{}", 
        BlockStatement{ name: None, all_span: Span::new(0, 1), 
            body: Block::new(Span::new(0, 1), vec![]) }
    }

    case!{ parse_block_stmt "@: {}",
        BlockStatement{ all_span: Span::new(0, 4),
            name: Some(LabelDef::new(1, Span::new(0, 1))),
            body: Block::new(Span::new(3, 4), vec![]) }
    }
}

#[test]
fn expr_stmt_parse() {
    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ "writeln(\"helloworld\");" as AssignExprStatement,
        Statement::SimpleExpr(SimpleExprStatement::new(Span::new(0, 21),
            make_expr!(fn 0:20 paren 7:20
                make_name!(simple 0:6 #2),
                make_expr!(str #3 8:19))
        ))
    }

    //                      012345678901
    case!{ "1 + 1 <<= 2;" as AssignExprStatement,  // to show I have 3 char Separator available
        Statement::AssignExpr(AssignExprStatement::new(Span::new(0, 11),
            Separator::LtLtEq, Span::new(6, 8),
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 1 4:4)),
            make_expr!(i32 2 10:10)
        ))
    }
}

#[test]
fn for_stmt_parse() {
    //                      0123456789012345678
    case!{ "@2: for i in 42 {}" as ForStatement,
        make_stmt!(for 0:17 label #2 0:2 for 4:6 var #3 8:8
            make_expr!(i32 42 13:14),
            Block::new(Span::new(16, 17), vec![]))
    }

    //              0         1         2         3         4         5         6         7         
    //              01234567890123456789012345678901234567890123456789012345678901 23456789012 34567
    case!{ "@hello: for _ in range(0, 10).enumerate().reverse() { writeln(\"helloworld\"); }" as ForStatement,
        make_stmt!(for 0:77 label #2 0:6 for 8:10 var #3 12:12
            make_expr!(fn 17:50 paren 49:50
                make_expr!(member 17:48 dot 41:41
                    make_expr!(fn 17:40 paren 39:40
                        make_expr!(member 17:38 dot 29:29
                            make_expr!(fn 17:28 paren 22:28
                                make_name!(simple 17:21 #4),
                                make_expr!(i32 0 23:23),
                                make_expr!(i32 10 26:27)),
                            make_name!(simple bare 30:38 #5)),),
                    make_name!(simple bare 42:48 #6)),),
            Block::new(Span::new(52, 77), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(54, 75),
                    make_expr!(fn 54:74 paren 61:74
                        make_name!(simple 54:60 #7),
                        make_expr!(str #8 62:73))
                ))
            ])
        ), strings ["hello", "_", "range", "enumerate", "reverse", "writeln", "helloworld"]
    }
}

#[test]
fn if_stmt_parse() {
    //                                      0        1         2         3
    //                                      0123456789012345678901234567890123456
    case!{ "if true { } else if false { } else {}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 36),
            if_clause: IfClause{ all_span: Span::new(0, 10), 
                condition: make_expr!(true 3:6),
                body: Block::new(Span::new(8, 10), vec![]) }, 
            elseif_clauses: vec![IfClause{ all_span: Span::new(12, 28), 
                condition: make_expr!(false 20:24),
                body: Block::new(Span::new(26, 28), vec![]) }],
            else_clause: Some(ElseClause{ all_span: Span::new(30, 36),
                body: Block::new(Span::new(35, 36), vec![]) }) }
    }

    //              0         1         2         3         4         5         6         7
    //              012345678901234567890123456789012345678901234567890123456789012345678901
    case!{ "if 1 { sth.do_sth(a); other.do_other(b); } else { [1,2,3].map(writeln);}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 71),
            if_clause: IfClause{ all_span: Span::new(0, 41), 
                condition: make_expr!(i32 1 3:3),
                body: Block::new(Span::new(5, 41), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(7, 20),
                        make_expr!(fn 7:19 paren 17:19
                            make_expr!(member 7:16 dot 10:10
                                make_name!(simple 7:9 #2),
                                make_name!(simple bare 11:16 #3)),
                            make_name!(simple 18:18 #4)))),
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(22, 39),
                        make_expr!(fn 22:38 paren 36:38
                            make_expr!(member 22:35 dot 27:27
                                make_name!(simple 22:26 #5),
                                make_name!(simple bare 28:35 #6)),
                            make_name!(simple 37:37 #7))))]) },
            elseif_clauses: vec![],
            else_clause: Some(ElseClause{ all_span: Span::new(43, 71),
                body: Block::new(Span::new(48, 71), vec![
                    Statement::SimpleExpr(SimpleExprStatement::new(Span::new(50, 70),
                        make_expr!(fn 50:69 paren 61:69
                            make_expr!(member 50:60 dot 57:57
                                make_expr!(array 50:56
                                    make_expr!(i32 1 51:51),
                                    make_expr!(i32 2 53:53),
                                    make_expr!(i32 3 55:55)),
                                make_name!(simple bare 58:60 #8)),
                            make_name!(simple 62:68 #9))))]) }),
        }, strings ["sth", "do_sth", "a", "other", "do_other", "b", "map", "writeln"]
    }

    // if condition does not expect object literal, unless parened
    //      0         1         2
    //      012345678901234567890123
    case!{ "if a {} else if (b{}) {}" as IfStatement,
        IfStatement{ all_span: Span::new(0, 23), 
            if_clause: IfClause{ all_span: Span::new(0, 6),
                condition: make_name!(simple 3:3 #2),
                body: Block::new(Span::new(5, 6), vec![]) },
            elseif_clauses: vec![IfClause{ all_span: Span::new(8, 23),
                condition: make_expr!(paren 16:20
                    make_expr!(object 17:19 quote 18:19
                        make_name!(simple 17:17 #3),)),
                body: Block::new(Span::new(22, 23), vec![]) }],
            else_clause: None,
        }
    }
}

#[test]
fn jump_stmt_parse() {
    
    case!{ "continue;" as ContinueStatement, 
        make_stmt!(continue 0:8)
    }
    case!{ "continue @1;" as ContinueStatement,
        make_stmt!(continue 0:11 label #2 9:10)
    }
    
    case!{ "break;" as BreakStatement,
        make_stmt!(break 0:5)
    }
    case!{ "break @1;" as BreakStatement,
        make_stmt!(break 0:8 label #2 6:7)
    }
}

#[test]
fn loop_stmt_parse() {

    case!{ "loop {}" as LoopStatement,
        make_stmt!(loop 0:6 loop 0:3
            Block::new(Span::new(5, 6), vec![]))
    }
    //                                        1234567890123456789 0123 45678
    case!{ "@@: loop { println(\"233\"); }" as LoopStatement,
        make_stmt!(loop 0:27 label #2 0:2 loop 4:7
            Block::new(Span::new(9, 27), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(11, 25), 
                    make_expr!(fn 11:24 paren 18:24
                        make_name!(simple 11:17 #3),
                        make_expr!(str #4 19:23))
                ))
            ])
        ), strings ["@", "println", "233"]
    }
}

#[test]
fn module_stmt_parse() {

    case!{ "module a;" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 7), path: None, all_span: Span::new(0, 8) },
    }
    //                   012345678901234567890
    case!{ "module os \"windows\";" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 8), path: Some((IsId::new(3), Span::new(10, 18))), all_span: Span::new(0, 19) },
    }

    //      0         1          2
    //      012345678901234567 89012345 6
    case!{ "module otherdir r\"ab/c.f3\";" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 14), path: Some((IsId::new(3), Span::new(16, 25))), all_span: Span::new(0, 26) },
    }
}

#[test]
fn module_parse() {
    //                      0123456789012345678901234
    case!{ "use a; module b; 3; b; a;" as Module,
        Module{ file: crate::source::FileId::new(1), items: vec![
            Item::Use(UseStatement{ all_span: Span::new(0, 5), alias: None,
                name: make_name!(simple bare 4:4 #2) }),
            Item::Import(ModuleStatement{ name: IsId::new(3), name_span: Span::new(14, 14), path: None, all_span: Span::new(7, 15) }),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(17, 18), 
                make_expr!(i32 3 17:17)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(20, 21), 
                make_name!(simple 20:20 #3)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(23, 24), 
                make_name!(simple 23:23 #2)
            )),
        ] }
    }
}

#[test]
fn ret_stmt_parse() {

    case!{ "return;" as ReturnStatement, 
        ReturnStatement::new_unit(Span::new(0, 6)) 
    }
    case!{ "return 1 + 1;" as ReturnStatement, 
        ReturnStatement::new_expr(
            Span::new(0, 12),
            make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11))
        )
    }
}

#[test]
fn use_stmt_parse() {

    case!{ "use a;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 5), alias: None, 
            name: make_name!(simple bare 4:4 #2) },
    }
    //                   0123456789012345678901234567890
    case!{ "use std::fmt::Debug as Display;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 30), alias: Some((IsId::new(5), Span::new(23, 29))),
            name: make_name!(bare 4:18 false, None,
                make_name!(segment 4:6 #2),
                make_name!(segment 9:11 #3),
                make_name!(segment 14:18 #4),
            ) },
    }
}

#[test]
fn var_decl_stmt_parse() {
    //                                           12345678901234
    case!{ "const abc = 0;" as VarDeclStatement,
        make_stmt!(const 0:13 #2 6:8
            None,
            Some(make_expr!(i32 0 12:12)))
    }

    //                                           0        1         
    //                                           12345678901234567890
    case!{ "var hij = [1, 3, 5];" as VarDeclStatement,
        make_stmt!(var 0:19 #2 4:6
            None,
            Some(make_expr!(array 10:18
                make_expr!(i32 1 11:11),
                make_expr!(i32 3 14:14),
                make_expr!(i32 5 17:17))))
    }
    
    //       1234567890123456789
    case!{ "const input: string;" as VarDeclStatement,
        make_stmt!(const 0:19 #2 6:10
            Some(make_type!(simple 13:18 #3)),
            None)
    }
    
    //      0         1         2
    //      012345678901234567890123
    case!{ "var buf: [(u8, char);1];" as VarDeclStatement,
        make_stmt!(var 0:23 #2 4:6
            Some(make_type!(array 9:22
                make_type!(tuple 10:19 [
                    make_type!(prim 11:12 U8),
                    make_type!(prim 15:18 Char),
                ]),
                make_expr!(i32 1 21:21).into())),
            None)
    }

    // Future Attention: after bits type added, the `0x7u8` will have different type as before, this is the Option::unwrap failure in test
    // and after advanced type infer, change the 0x7u8 to 7, and try to infer it as 7u8, which requires 2 major changes
    //     do not infer i32 in num lit if no postfix provided
    //     desugar array primary expr to call array_tid::new() and array.push, which infer 7's type as array_tid' push method's parameter
    //      0         1         2         3         4
    //      01234567890123456789012345678901234567890123456789
    case!{ "var buf: ([u8;3], u32) = ([1u8, 5u8, 0x7u8], abc);" as VarDeclStatement,
        make_stmt!(var 0:49 #2 4:6
            Some(make_type!(tuple 9:21 [
                make_type!(array 10:15 
                    make_type!(prim 11:12 U8), 
                    make_expr!(i32 3 14:14).into()), 
                make_type!(prim 18:20 U32)])),
            Some(make_expr!(tuple 25:48
                make_expr!(array 26:42
                    make_expr!(u8 1 27:29),
                    make_expr!(u8 5 32:34),
                    make_expr!(u8 7 37:41)),
                make_name!(simple 45:47 #3)))
        ), strings ["buf", "abc"]
    }

    case!{ "var a;" as VarDeclStatement,
        make_stmt!(var 0:5 #2 4:4 None, None), errors make_errors!(
            e: e.emit("require type annotation")
                .detail(Span::new(4, 4), "variable declaration here")
                .help("cannot infer type without both type annotation and initialization expression")),
    }
}

#[test]
fn while_stmt_parse() {
    //      0        1         2         3         4        
    //      01234567890123456789012345 67890123456789012 3456
    case!{ "@2: while true { writeln(\"fresky hellooooo\"); }" as WhileStatement,
        make_stmt!(while 0:46 label #2 0:2 while 4:8
            make_expr!(true 10:13),
            Block::new(Span::new(15, 46), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(17, 44),
                    make_expr!(fn 17:43 paren 24:43
                        make_name!(simple 17:23 #3),
                        make_expr!(str #4 25:42))
                ))
            ])
        ), strings ["2", "writeln", "fresky hellooooo"]
    }
}
