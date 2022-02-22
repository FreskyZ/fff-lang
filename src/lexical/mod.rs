#![macro_use]
///! fff-lang
///! 
///! lexical parser
///! TokenStream::new(source, messages) for formal use
///! TokenStream::with_test_str(input) for test use

#[cfg(test)]
macro_rules! make_lit {
    ($val: expr) => (LitValue::from($val));
    (str, $sid: expr) => (LitValue::new_str_lit_simple_usize($sid));
    (fstr, $($seg: expr),+) => ({
        let mut segs = Vec::new();
        $(segs.push(From::from($x));)*
        LitValue::new_str_lit_format(segs)
    });
    (fstr, $($seg: expr,)*) => (make_lit!(fstr, $($x),*));
}

mod token_def;
mod token_buf;
mod v1lexer;
mod v2lexer;
mod token_stream;

use token_buf::{ILexer, BufLexer, ParseSession};

pub use token_def::{Seperator, SeperatorCategory, Keyword, NumLitValue, StrLitValue, FormatStrLitSegment, LitValue, Token};
pub use token_stream::TokenStream;
