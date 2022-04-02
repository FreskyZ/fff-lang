///! fff-lang
///!
///! syntax/while_stmt

#[cfg(test)] #[test]
fn while_stmt_parse() {use super::prelude::*;
    use super::{ast::Statement, ast::SimpleExprStatement};
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
