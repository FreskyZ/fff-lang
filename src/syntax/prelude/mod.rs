///! syntax::prelude: common functions in syntax module

// 1. put syntax tree nodes in different 
//    groups expr/stmt/item makes a lot of a lot of 'super::super's
// 2. but put them in src/syntax/nodes will make this directory too large while kind of strange 
//    that src/syntax/nodes contains more than 30 files while src/syntax only contains several files
// 3. so put nodes directly in src/syntax while put common things in this module
// 4. functions in this module is used by nearly all node modules (not node_modules),
//    so this module is directly called prelude and included other common imports from source/diagnostics/lexical

mod parse_sess;
mod format_helper;

pub use std::fmt;
pub use crate::source::{FileSystem, Span, IsId};
pub use crate::diagnostics::strings;
pub use crate::lexical::{Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};

pub use parse_sess::{ParseSession, ParseResult, Node};
#[cfg(test)]
pub(crate) use parse_sess::make_node;
pub use format_helper::{Formatter, ISyntaxFormat};
