///! fff-lang
///!
///! syntax/range_expr

#[cfg(test)] #[test]
fn range_expr_parse() {
use super::prelude::*;use super::RangeExpr;

    case!{ ".." as RangeExpr, 
        make_expr!(range full 0:1)
    }

    case!{ "..1 + 1" as RangeExpr,
        make_expr!(range right 0:6
            make_expr!(binary 2:6 Add 4:4
                make_expr!(i32 1 2:2),
                make_expr!(i32 1 6:6)))
    }

    case!{ "1 .." as RangeExpr,
        make_expr!(range left 0:3
            make_expr!(i32 1 0:0))
    }
}
