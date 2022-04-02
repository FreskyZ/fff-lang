///! fff-lang
///!
///! syntax/unary_expr
///! 

#[cfg(test)] #[test]
fn unary_expr_parse() {use super::prelude::*;
    
    case!{ "1" as Expr, 
        make_expr!(i32 1 0:0)
    }

    case!{ "!~!1" as UnaryExpr,
        make_expr!(unary 0:3 Not 0:0
            make_expr!(unary 1:3 Tilde 1:1
                make_expr!(unary 2:3 Not 2:2
                    make_expr!(i32 1 3:3))))
    }

    case!{ "&a(&b)" as UnaryExpr,
        make_expr!(unary 0:5 And 0:0
            make_expr!(fn 1:5 paren 2:5
                make_name!(simple 1:1 #2),
                make_expr!(unary 3:4 And 3:3
                    make_name!(simple 4:4 #3))))
    }
}
