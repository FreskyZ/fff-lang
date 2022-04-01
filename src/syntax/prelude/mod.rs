///! syntax::prelude: node trait and related common functions

// 1. put syntax tree nodes in different 
//    groups expr/stmt/item makes a lot of a lot of 'super::super's
// 2. but put them in src/syntax/nodes will make this directory too large while kind of strange 
//    that src/syntax/nodes contains more than 30 files while src/syntax only contains several files
// 3. so put nodes directly in src/syntax while put common things in this module
// 4. functions in this module is used by nearly all node modules (not node_modules),
//    so this module is directly called prelude and included other common imports from source/diagnostics/lexical

pub use std::fmt;
pub use crate::source::{Span, IsId};
pub use crate::diagnostics::strings;
pub use crate::lexical::{Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
pub use super::ast::*;
pub use super::visit::{Node, Visitor};

mod context;
mod node_display;

pub use context::{ParseContext};
pub use node_display::{FormatVisitor, NodeDisplay};

/// unrecoverable unexpected for this parser, detail in diagnostics
// this should be more readable than previous Result<Self::Output, ()>
#[derive(Debug)]
pub struct Unexpected;

pub trait Parser: Sized {
    type Output: Node;

    fn matches(_current: &Token) -> bool {
        false
    }
    fn matches3(_current: &Token, _peek1: &Token, _peek2: &Token) -> bool {
        false
    }

    fn parse(_cx: &mut ParseContext) -> Result<Self::Output, Unexpected>;

    // check matches_first, if pass, parse, return Ok(Some(T)) or Err(Unexpected), else return None
    fn try_parse(cx: &mut ParseContext) -> Result<Option<Self::Output>, Unexpected> {
        Ok(if Self::matches(&cx.current) || Self::matches3(&cx.current, &cx.peek, &cx.peek2) { Some(cx.expect::<Self>()?) } else { None })
    }
}

#[cfg(test)]
pub fn ast_test_case<
    O: PartialEq + Node + fmt::Debug,
    N: Parser<Output = O>,
>(
    input: &'static str,
    expect_node: O, 
    expect_diagnostics: crate::diagnostics::Diagnostics, 
    expect_strings: &[&'static str], 
    backtrace: u32,
) {
    use std::fmt::Write;
    use crate::source::SourceContext;
    
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let mut context = ParseContext::new(crate::lexical::Parser::new(source.entry("1"), &mut actual_diagnostics));
    if let Ok(actual_node) = N::parse(&mut context) {
        context.finish();
        // for now does not check expect strings not provided, but ideally should always check interned strings
        if expect_strings.len() > 0 {
            // put interned string expect before node expect and diagnostics expect, or else it is too hard to fix expected values
            for (i, expect_string) in expect_strings.into_iter().enumerate() {
                assert_eq!(source.resolve_string(IsId::new(i as u32 + 2)), *expect_string, "line {} string #{} not same", backtrace, i + 2);
            }
        }
        if actual_node == expect_node {
            if actual_diagnostics == expect_diagnostics {
                // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
            let (actual_display, expect_display) = (actual_node.display(&source).to_string(), expect_node.display(&source).to_string());
            if actual_display == expect_display {
                panic!("line {} node not same while display is same\n{}\n{:?}\n{:?}\n", backtrace, actual_display, actual_node, expect_node);
            }

            let mut buf = format!("line {} node not same\n", backtrace);
            let (actual_lines, expect_lines) = (actual_display.lines().collect::<Vec<_>>(), expect_display.lines().collect::<Vec<_>>());
            let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
            for line in 0..common_line_count {
                if actual_lines[line] != expect_lines[line] {
                    writeln!(buf, "{: >3} |- {}", line + 1, actual_lines[line]).unwrap();
                    writeln!(buf, "    |+ {}", expect_lines[line]).unwrap();
                } else {
                    writeln!(buf, "{: >3} |  {}", line + 1, actual_lines[line]).unwrap();
                }
            }
            if actual_lines.len() > common_line_count {
                for line in common_line_count..actual_lines.len() {
                    writeln!(buf, "{: >3} |- {}", line + 1, actual_lines[line]).unwrap();
                }
            }
            if expect_lines.len() > common_line_count {
                for line in common_line_count..expect_lines.len() {
                    writeln!(buf, "{: >3} |+ {}", line + 1, expect_lines[line]).unwrap();
                }
            }
            panic!("{}", buf)
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

#[cfg(test)]
macro_rules! case {
    ($code:literal as $ty:ty, $expect:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, crate::diagnostics::make_errors!(), &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, $expect_diagnostics, &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, crate::diagnostics::make_errors![], &$expect_strings, line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case::<_, $ty>($code, $expect, $expect_diagnostics, &$expect_strings, line!());
    );
}

#[cfg(test)]
pub(crate) use case;
#[cfg(test)]
pub(crate) use crate::diagnostics::make_errors;
#[cfg(test)]
pub(crate) use super::abc::make_stmt;
#[cfg(test)]
pub(crate) use super::expr::make_expr;
#[cfg(test)]
pub(crate) use super::name::make_name;
#[cfg(test)]
pub(crate) use super::plain_type::make_type;
