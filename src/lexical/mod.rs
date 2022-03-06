#![macro_use]
///! fff-lang
///! 
///! lexical parser
///! TokenStream::new(source, messages) for formal use
///! make_node!(input) for test use

mod token;
mod token_buf;
mod v1lexer;
mod v2lexer;
mod token_stream;
mod new_token;

use token_buf::{ILexer, BufLexer, ParseSession};

pub use token::{Separator, SeparatorKind, Keyword, KeywordKind, NumLitValue, LitValue};
pub use new_token::{Token, TokenFormat, Numeric, StringLiteralType};
pub use token_stream::TokenStream;
