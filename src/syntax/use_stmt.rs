////! fff-lang
///!
///! syntax/use_stmt
///! 

#[cfg(test)] #[test]
fn use_stmt_parse() {
    use super::prelude::*;
    

    case!{ "use a;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 5), alias: None, 
            name: make_name!(simple bare 4:4 #2) },
    }
    //                   0123456789012345678901234567890
    case!{ "use std::fmt::Debug as Display;" as UseStatement,
        UseStatement{ all_span: Span::new(0, 30), alias: Some((IsId::new(5), Span::new(23, 29))),
            name: make_name!(bare 4:18 false, None,
                make_name!(segment 4:6 #2),
                make_name!(segment 9:11 #3),
                make_name!(segment 14:18 #4),
            ) },
    }
}
