///! fff-lang
///! 
///! lexical parser
///! TokenStream::new(source, messages) for formal use
///! TokenStream::with_test_str(input) for test use

#[cfg_attr(test, macro_use)] mod token_def;
mod token_buf;
mod v1lexer;
mod v2lexer;
mod token_stream;

use token_buf::{ILexer, BufLexer, ParseSession};

pub use token_def::{Seperator, SeperatorCategory, Keyword, NumLitValue, StrLitValue, FormatStrLitSegment, LitValue, Token};
pub use token_stream::TokenStream;
