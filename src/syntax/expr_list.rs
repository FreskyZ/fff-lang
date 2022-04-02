///! syntax/tuple_def_expr, paren_expr
 
#[cfg(test)]
use super::prelude::*;

#[cfg(test)] #[test]
fn expr_list_parse() {

    case!{ "[1, 2, 3]" as ExprList, 
        ExprListParseResult::Normal(Span::new(0, 8), ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7),
        ] })
    }
    
    case!{ "(1, 2, 3,)" as ExprList, 
        ExprListParseResult::EndWithComma(Span::new(0, 9), ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7),
        ] })
    }

    case!{ "[]" as ExprList, 
        ExprListParseResult::Empty(Span::new(0, 1))
    }

    case!{ "{,}" as ExprList,
        ExprListParseResult::SingleComma(Span::new(0, 2))
    }
}
