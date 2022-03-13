///! syntax::node: syntax tree node trait

use crate::source::FileSystem;
use crate::lexical::Token;
use super::parse_sess::{ParseSession, ParseResult};

pub trait Node {
    type ParseOutput;
    
    fn matches(_current: &Token) -> bool {
        return false;
    }
    fn matches3(_current: &Token, _peek1: &Token, _peek2: &Token) -> bool {
        return false;
    }

    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<Self::ParseOutput> where F: FileSystem;

    // check matches_first, if pass, parse, return Ok(Some(T)) or Err(()), else return None
    fn try_parse<F>(sess: &mut ParseSession<F>) -> ParseResult<Option<Self::ParseOutput>> where F: FileSystem {
        Ok(if Self::matches(&sess.current) || Self::matches3(&sess.current, &sess.peek, &sess.peek2) { Some(Self::parse(sess)?) } else { None })
    }
}

#[cfg(test)]
macro_rules! make_node {
    ($code:literal) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = crate::syntax::Module::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, and messages) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let chars = scx.entry("1");
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?]) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        assert_eq!(ecx, crate::diagnostics::make_errors!());
        node
    }};
    // TODO change span string and value string to be expected value not prepare value
    ($code:literal as $ty:ty, [$($span_string:expr),*$(,)?], [$($value_string:expr),*$(,)?], and messages) => {{
        let mut ecx = crate::diagnostics::MessageCollection::new();
        let mut scx = crate::source::make_source!($code);
        let mut chars = scx.entry("1");
        $( chars.intern_span($span_string); )*
        $( chars.intern($value_string); )*
        let lcx = crate::lexical::Parser::new(chars, &mut ecx);
        let mut pcx = ParseSession::new(lcx);
        let node = <$ty>::parse(&mut pcx).expect("failed to parse test input");
        (node, ecx)
    }};
}
#[cfg(test)]
pub(crate) use make_node;
