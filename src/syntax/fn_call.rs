///! syntax::fn_call_expr:
///! 

#[cfg(test)]
#[test]
fn fn_call_parse() {use super::prelude::*;

    case!{ "()" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 1), params: ExprList{ items: Vec::new() } }
    }

    case!{ "(\"hello\")" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 8), params: ExprList{ items: vec![
            make_expr!(str #2 1:7),
        ] } }
    }

    case!{ "(,)" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 2), params: ExprList{ items: Vec::new() } },
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::FnCallHere))
    }
}
