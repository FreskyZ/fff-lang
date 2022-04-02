///! syntax::loop_stmt:





#[cfg(test)] #[test]
fn loop_stmt_parse() {use super::prelude::*;
    use super::{ast::Statement, ast::SimpleExprStatement};

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
