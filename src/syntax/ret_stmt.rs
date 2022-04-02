///! fff-lang
///!
///! syntax/ret_stmt
///!

#[cfg(test)] #[test]
fn ret_stmt_parse() {use super::prelude::*;
    case!{ "return;" as ReturnStatement, ReturnStatement::new_unit(Span::new(0, 6)) }
    case!{ "return 1 + 1;" as ReturnStatement, 
        ReturnStatement::new_expr(
            Span::new(0, 12),
            make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11))
        )
    }
}
