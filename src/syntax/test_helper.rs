///! syntax/test_helper

use crate::source::SourceContext;
use crate::diagnostics::MessageCollection;
use crate::lexical::TokenStream;
use super::{ParseSession, ISyntaxParse};

pub fn try_make_node<U, T: ISyntaxParse<Output = U>>(scx: &SourceContext) -> Option<U> {
    let mut messages = MessageCollection::new();
    let tokens = TokenStream::new(scx.entry("1"), &mut messages);
    let mut context = ParseSession::new(scx, &tokens, &mut messages);
    T::parse(&mut context).ok()
}

macro_rules! make_node {
    ($code:literal) => {{
        crate::syntax::try_make_node(crate::source::make_source!($code)).expect("failed to parse test input")
    }};
    ($code:literal as $ty:ty) => {{
        crate::syntax::try_make_node::<_, $ty>(crate::source::make_source!($code)).expect("failed to parse test input")
    }}
}
pub(crate) use make_node;
