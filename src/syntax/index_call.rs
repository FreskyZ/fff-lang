///! syntax::index_call_expr:




#[cfg(test)]
#[test]
fn index_call_parse() {
use super::prelude::*;

    case!{ "[1, 2, ]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 7), params: ExprList{ items: vec![
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
        ] } }
    }

    case!{ "[\"hello\"]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 8), params: ExprList{ items: vec![
            make_expr!(str #2 1:7)
        ] } }
    }

    case!{ "[,]" as IndexCallExpr,
        IndexCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), bracket_span: Span::new(0, 2), params: ExprList{ items: Vec::new() } }, 
        errors make_errors!(e: e.emit(strings::EmptyIndexCall).detail(Span::new(0, 2), strings::IndexCallHere)),
    }
}
