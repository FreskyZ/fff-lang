
#[cfg(test)] #[test]
fn expr_stmt_parse() {
use super::prelude::*;
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