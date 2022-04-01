///! fff-lang
///!
///! array_def = '[' [ expr_list ] ']'

use super::prelude::*;

impl Parser for ArrayDef {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftBracket)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> {
        
        match cx.expect::<ExprList>()? {
            ExprListParseResult::Empty(span) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } })),
            ExprListParseResult::Normal(span, exprlist) 
            | ExprListParseResult::EndWithComma(span, exprlist) =>
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: exprlist })),
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::ArrayDefHere);
                Ok(Expr::Array(ArrayDef{ bracket_span: span, items: ExprList{ items: Vec::new() } }))
            }
        }
    }
}

#[cfg(test)]
#[test]
fn array_def_parse() {
    case!{ "[a]" as ArrayDef,
        make_expr!(array 0:2
            make_name!(simple 1:1 #2)),
    }

    //                                   01234567
    case!{ "[1, '2']" as ArrayDef,
        make_expr!(array 0:7
            make_expr!(i32 1 1:1),
            make_expr!(char 4:6 '2')),
    }
    //                                   01234567
    case!{ "[1 + 1,]" as ArrayDef,
        make_expr!(array 0:7
            make_expr!(binary 1:5 Add 3:3
                make_expr!(i32 1 1:1),
                make_expr!(i32 1 5:5))),
    }
}

#[cfg(test)] #[test]
fn array_def_errors() {

    case!{ "[ , ]" as ArrayDef,
        make_expr!(array 0:4),
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 4), strings::ArrayDefHere))
    }
}
