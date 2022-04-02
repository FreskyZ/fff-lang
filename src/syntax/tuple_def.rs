///! fff-lang
///! 
///! syntax/tuple_def, paren_expr

#[cfg(test)] #[test]
fn tuple_def_parse() {use super::prelude::*;
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
