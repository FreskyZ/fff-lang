///! fff-lang
///! 
///! syntax/tuple_def, paren_expr
///! tuple_def = '(' expr_list ')'
///! paren_expr = '(' expr ')'
///! unit_lit = '(' ')'

use super::prelude::*;


impl Parser for TupleDef {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftParen)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {

        match cx.expect::<ExprList>()? {
            ExprListParseResult::Empty(span) => {
                Ok(Expr::Lit(LitExpr::new(LitValue::Unit, span)))
            }
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::TupleDefHere);
                Ok(Expr::Tuple(TupleDef{ paren_span: span, items: ExprList{ items: Vec::new() } }))
            }
            ExprListParseResult::Normal(span, exprlist) => {
                if exprlist.items.len() == 1 {
                    Ok(Expr::Paren(ParenExpr{ span, expr: Box::new(exprlist.items.into_iter().last().unwrap()) }))
                } else {
                    Ok(Expr::Tuple(TupleDef{ paren_span: span, items: exprlist }))
                }
            }
            ExprListParseResult::EndWithComma(span, exprlist) => {
                Ok(Expr::Tuple(TupleDef{ paren_span: span, items: exprlist }))
            }
        }
    }
}


#[cfg(test)] #[test]
fn tuple_def_parse() {
    //                                   01234567
    case!{ "(1, '2')" as TupleDef,
        make_expr!(tuple 0:7
            make_expr!(i32 1 1:1),
            make_expr!(char 4:6 '2'))
    }
    //                                   0123456
    case!{ "(1 + 1)" as TupleDef,
        make_expr!(paren 0:6
            make_expr!(binary 1:5 Add 3:3
                make_expr!(i32 1 1:1),
                make_expr!(i32 1 5:5)))
    }
    
    case!{ "( , )" as TupleDef,
        make_expr!(tuple 0: 4),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::TupleDefHere)),
    }
}
