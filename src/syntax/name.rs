///! syntax::name:

#[cfg(test)]
macro_rules! make_name {
    (simple $start:literal:$end:literal #$id:literal) => (
        make_name!($start:$end false, None, make_name!(segment $start:$end #$id)));
    ($start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (
        crate::syntax::ast::Expr::Name(crate::syntax::ast::Name{ type_as_segment: $as, global: $global, all_span: Span::new($start, $end), segments: vec![$($segment,)*] }));
    (segment $start:literal:$end:literal #$id:literal) => (
        crate::syntax::ast::NameSegment::Normal(IsId::new($id), Span::new($start, $end)));
    (segment generic $start:literal:$end:literal $($ty:expr),*$(,)?) => (
        crate::syntax::ast::NameSegment::Generic(vec![$($ty,)*], Span::new($start, $end)));
    // bare version for use outside of expr
    (simple bare $start:literal:$end:literal #$id:literal) => (
        make_name!(bare $start:$end false, None, make_name!(segment $start:$end #$id)));
    (bare $start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (
        crate::syntax::ast::Name{ type_as_segment: $as, global: $global, all_span: Span::new($start, $end), segments: vec![$($segment,)*] });
}
#[cfg(test)]
pub(crate) use make_name;

#[cfg(test)]
#[test]
fn name_parse() {use super::prelude::*;

    case!{ "hello" as Name, 
        make_name!(bare 0:4 false, None,
            make_name!(segment 0:4 #2)),
    }
    //              0        1         2         3         4
    //              01234567890123456789012345678901234567890
    case!{ "std::network::wlan::native::GetWLANHandle" as Name,
        make_name!(bare 0:40 false, None,
            make_name!(segment 0:2 #2), 
            make_name!(segment 5:11 #3),
            make_name!(segment 14:17 #4),
            make_name!(segment 20:25 #5),
            make_name!(segment 28:40 #6))
    }

    //      0         1         2      v this is not part of name
    //      012345678901234567890123456
    case!{ "::abc::def::<ghi, jkl>::mno<" as Name, 
        make_name!(bare 0:26 true, None,
            make_name!(segment 2:4 #2),
            make_name!(segment 7:9 #3),
            make_name!(segment generic 12:21
                make_type!(simple 13:15 #4),
                make_type!(simple 18:20 #5)),
            make_name!(segment 24:26 #6))
    }

    //      0         1         2
    //      01234567890123456789012
    case!{ "<Name as Parser>::parse" as Name,
        make_name!(bare 0:22 false,
            make_type!(segment as 0:15
                make_type!(simple 1:4 #2),
                make_type!(simple 9:14 #3)),
            make_name!(segment 18:22 #4)),
    }

    //      01234567890123
    case!{ "a::<b>::<c>::d" as Name,
        make_name!(bare 0:13 false, None,
            make_name!(segment 0:0 #2),
            make_name!(segment generic 3:5
                make_type!(simple 4:4 #3)),
            make_name!(segment generic 8:10
                make_type!(simple 9:9 #4)),
            make_name!(segment 13:13 #5)
        ), errors make_errors!(e: e.emit(strings::InvalidNameSegment).detail(Span::new(8, 8), strings::NameSegmentExpect)),
    }
}
