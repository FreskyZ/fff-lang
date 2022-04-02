///! fff-lang
///!
///! syntax/for_stmt

#[cfg(test)] #[test]
fn for_stmt_parse() {use super::prelude::*;
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
