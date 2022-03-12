///! syntax/test_helper

macro_rules! make_node {
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty as ISyntaxParse<crate::source::VirtualFileSystem>>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, make_messages![]);
        node
    }};
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty as ISyntaxParse<crate::source::VirtualFileSystem>>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, make_messages![]);
        node
    }};
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty as ISyntaxParse<crate::source::VirtualFileSystem>>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, make_messages![]);
        node
    }};
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?], and messages) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty as ISyntaxParse<crate::source::VirtualFileSystem>>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
}
pub(crate) use make_node;
