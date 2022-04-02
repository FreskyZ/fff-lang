///! syntax::if_stmt:
///! 

#[cfg(test)] #[test]
fn if_stmt_parse() {use super::prelude::*;
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
