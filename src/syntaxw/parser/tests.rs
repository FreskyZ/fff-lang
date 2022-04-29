
use std::fmt::{self, Write};
use std::str::from_utf8;
use crate::source::{SourceContext, Span, IdSpan, FileId};
use crate::diagnostics::{strings, make_errors};
use crate::lexical::{Numeric, Separator, Keyword};
use crate::common::arena::{Arena, TagIndex};
use super::super::visit::Visit;
use super::*;

struct DiffDisplay<'a, 'b>(&'a str, &'b str);

impl<'a, 'b> fmt::Display for DiffDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut actual_lines = self.0.lines().collect::<Vec<_>>();
        let mut expect_lines = self.1.lines().collect::<Vec<_>>();

        // if both side is single line with len > 100, then it is comparing debug fmt,
        // hard split (regardless of word boundary) them to fixed length lines
        // (in bytes, because ast debug print does not contain non ascii char, because identifiers and string literals are isid)
        const HL: usize = 72;
        if actual_lines.len() == 1 && actual_lines[0].len() > HL && expect_lines.len() == 1 && expect_lines[0].len() > HL {
            actual_lines = self.0.as_bytes().chunks(HL).map(|c| from_utf8(c).unwrap()).collect::<Vec<_>>();
            expect_lines = self.1.as_bytes().chunks(HL).map(|c| from_utf8(c).unwrap()).collect::<Vec<_>>();
        }

        let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
        for line in 0..common_line_count {
            if actual_lines[line] != expect_lines[line] {
                writeln!(f, "{: >3} |A {}", line + 1, actual_lines[line])?;
                writeln!(f, "    |E {}", expect_lines[line])?;
            } else {
                writeln!(f, "{: >3} |  {}", line + 1, actual_lines[line])?;
            }
        }
        if actual_lines.len() > common_line_count {
            for line in common_line_count..actual_lines.len() {
                writeln!(f, "{: >3} |A {}", line + 1, actual_lines[line])?;
            }
        }
        if expect_lines.len() > common_line_count {
            for line in common_line_count..expect_lines.len() {
                writeln!(f, "{: >3} |E {}", line + 1, expect_lines[line])?;
            }
        }
        Ok(())
    }
}

fn tag_case<
    N: Visit + asti::Eq,
    A: for<'a> FnOnce(&mut Parser, &'a Arena) -> Result<TagIndex<'a, N>, Unexpected>,
    E: for<'a> FnOnce((&mut Parser, &'a Arena)) -> TagIndex<'a, N>,
>(
    input: &'static str,
    actual_index_getter: A,
    expect_index_getter: E,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
) {
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let arena = Arena::new();
    let mut context = Parser::new(crate::lexical::Parser::new(source.entry("1", &mut actual_diagnostics).unwrap(), &mut actual_diagnostics));
    let actual_index = actual_index_getter(&mut context, &arena);
    if let Ok(actual_index) = actual_index {
        let expect_index = expect_index_getter((&mut context, &arena));
        context.finish();
        if asti::Eq::eq(actual_index.as_repr().as_ref(), expect_index.as_repr().as_ref(), &arena) {
            if actual_diagnostics == expect_diagnostics {
                // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
            let actual_display = asti::display(actual_index.as_repr().as_ref(), &source, &arena).to_string();
            let expect_display = asti::display(expect_index.as_repr().as_ref(), &source, &arena).to_string();
            if actual_display == expect_display {
                let actual_debug = format!("{:?}", asti::debug(actual_index.as_repr().as_ref(), &arena));
                let expect_debug = format!("{:?}", asti::debug(expect_index.as_repr().as_ref(), &arena));
                panic!("line {} node not same while display is same\n{}\n{}", backtrace, actual_display, DiffDisplay(&actual_debug, &expect_debug));
            }
            panic!("line {} node not same\n{}", backtrace, DiffDisplay(&actual_display, &expect_display));
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

fn notag_case<
    N: Visit + asti::Eq,
    A: for<'a> FnOnce(&mut Parser, &'a Arena) -> Result<Index<'a, N>, Unexpected>,
    E: for<'a> FnOnce((&mut Parser, &'a Arena)) -> Index<'a, N>,
>(
    input: &'static str,
    actual_index_getter: A,
    expect_index_getter: E,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
) {
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let arena = Arena::new();
    let mut context = Parser::new(crate::lexical::Parser::new(source.entry("1", &mut actual_diagnostics).unwrap(), &mut actual_diagnostics));
    let actual_index = actual_index_getter(&mut context, &arena);
    if let Ok(actual_index) = actual_index {
        let expect_index = expect_index_getter((&mut context, &arena));
        context.finish();
        let (actual_value, expect_value) = (arena.get(&actual_index), arena.get(&expect_index));
        if asti::Eq::eq(actual_value, expect_value, &arena) {
            if actual_diagnostics == expect_diagnostics {
                // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
            let actual_display = asti::display(actual_value, &source, &arena).to_string();
            let expect_display = asti::display(expect_value, &source, &arena).to_string();
            if actual_display == expect_display {
                let actual_debug = format!("{:?}", asti::debug(actual_value, &arena));
                let expect_debug = format!("{:?}", asti::debug(expect_value, &arena));
                panic!("line {} node not same while display is same\n{}\n{}", backtrace, actual_display, DiffDisplay(&actual_debug, &expect_debug));
            }
            panic!("line {} node not same\n{}", backtrace, DiffDisplay(&actual_display, &expect_display));
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

// some parse method does not return type that impl Node
fn notast_case<
    V,
    P: for<'a> FnOnce(&mut Parser, &'a Arena) -> Result<V, Unexpected>,
    E: for<'a> FnOnce((&mut Parser, &'a Arena)) -> V,
    S: Fn(&V, &V, &Arena) -> bool,
    D: fmt::Debug,
    F: Fn(&V, &Arena) -> D,
>(
    input: &'static str,
    actual_value_getter: P,
    expect_value_getter: E,
    equality_comparer: S,
    debug_formatter: F,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
) {
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let arena = Arena::new();
    let mut context = Parser::new(crate::lexical::Parser::new(source.entry("1", &mut actual_diagnostics).unwrap(), &mut actual_diagnostics));
    let actual_value = actual_value_getter(&mut context, &arena);
    if let Ok(actual_value) = actual_value {
        let expect_value = expect_value_getter((&mut context, &arena));
        context.finish();
        if equality_comparer(&actual_value, &expect_value, &arena) {
            if actual_diagnostics == expect_diagnostics {
                // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
                let actual_debug = format!("{:?}", debug_formatter(&actual_value, &arena));
                let expect_debug = format!("{:?}", debug_formatter(&expect_value, &arena));
                panic!("line {} result\n{}", backtrace, DiffDisplay(&actual_debug, &expect_debug));
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

macro_rules! case {
    // value is tagged index
    ($parser:ident $code:literal, |$x:ident| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        tag_case($code, |cx, a| cx.$parser(a), |$x| $expect, make_errors!($($($tt)+)?), line!());
    );
    // value is index not tagged index
    (notag $parser:ident $code:literal, |$x:ident| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        notag_case($code, |cx, a| cx.$parser(a), |$x| $expect, make_errors!($($($tt)+)?), line!());
    );
    // value is not Visit
    (notast($cmp:expr, $debug:expr) $parser:ident $code:literal, |$x:ident| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        notast_case($code, |cx, a| cx.$parser(a), |$x| $expect, $cmp, $debug, make_errors!($($($tt)+)?), line!());
    );
}

// used by following macros internally, redirect ident to literal, allowing direct #ident in these macros
// // this is approaching some level of quote!
macro_rules! make_isid {
    ($x:ident, $i:ident) => ($x.0.intern(stringify!($i)));
    ($x:ident, $i:literal) => ($x.0.intern($i));
}

// ATTENTION:
// these macros, make_path, make_expr, make_stmt, make_type
// should be same structure as pretty print in principle, this makes test case expect results look like pretty and looks pretty

macro_rules! make_expr {
    // literals does not have (lit prefix because they are used frequently
    ($x:ident unit $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Unit).into()
    );
    ($x:ident true $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Bool(true)).into()
    );
    ($x:ident false $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Bool(false)).into()
    );
    ($x:ident char $start:literal:$end:literal $v:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Char($v)).into()
    );
    ($x:ident str #$v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Str($x.0.intern($v))).into()
    );
    ($x:ident i32 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::I32($v))).into()
    );
    ($x:ident u8 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::U8($v))).into()
    );
    ($x:ident u32 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::U32($v))).into()
    );
    ($x:ident u64 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::U64($v))).into()
    );
    ($x:ident r32 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::R32($v))).into()
    );
    ($x:ident r64 $v:literal $start:literal:$end:literal) => (
        $x.1.emplace_lit_expr(Span::new($start, $end), LitValue::Num(Numeric::R64($v))).into()
    );
    ($x:ident path $($tt:tt)+) => (
        make_path!($x $($tt)+)
    );
    ($x:ident binary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (
        $x.1.emplace_binary_expr(Span::new($start, $end), $left, $right, Separator::$op, Span::new($op_start, $op_end)).into()
    );
    ($x:ident unary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $base:expr) => (
        $x.1.emplace_unary_expr(Span::new($start, $end), $base, Separator::$op, Span::new($op_start, $op_end)).into()
    );
    ($x:ident member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal #$ident:tt $ident_start:literal:$ident_end:literal $base:expr) => (
        $x.1.emplace_member_expr(Span::new($start, $end), $base, Span::new($dot_start, $dot_end), 
            IdSpan::new(make_isid!($x, $ident), Span::new($ident_start, $ident_end)), None).into()
    );
    ($x:ident member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal #$ident:tt $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        $x.1.emplace_member_expr(Span::new($start, $end), $base, Span::new($dot_start, $dot_end), 
            IdSpan::new(make_isid!($x, $ident), Span::new($ident_start, $ident_end)), Some($x.1.emplace_type_list(Span::new($quote_start, $quote_end), vec![$($parameter,)*]))).into()
    );
    ($x:ident array $start:literal:$end:literal $($item:expr),*$(,)?) => (
        $x.1.emplace_array_expr(Span::new($start, $end), vec![$($item,)*]).into()
    );
    ($x:ident tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (
        $x.1.emplace_tuple_expr(Span::new($start, $end), vec![$($item,)*]).into()
    );
    ($x:ident paren $start:literal:$end:literal $base:expr) => (
        $x.1.emplace_paren_expr(Span::new($start, $end), $base).into()
    );
    ($x:ident object $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($field:expr),*$(,)?) => (
        $x.1.emplace_object_expr(Span::new($start, $end), $base, Span::new($quote_start, $quote_end), vec![$($field,)*]).into()
    );
    ($x:ident object field $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal colon $colon_start:literal:$colon_end:literal $value:expr$(,)?) => (
        $x.1.emplace_object_expr_field(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), $value)
    );
    ($x:ident call $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        $x.1.emplace_call_expr(Span::new($start, $end), $base, Span::new($paren_start, $paren_end), vec![$($parameter,)*]).into()
    );
    ($x:ident array index $start:literal:$end:literal bracket $bracket_start:literal:$bracket_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        $x.1.emplace_array_index_expr(Span::new($start, $end), $base, vec![$($parameter,)*], Span::new($bracket_start, $bracket_end)).into()
    );
    ($x:ident tuple index $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal i32 $value:literal $value_start:literal:$value_end:literal $base:expr) => (
        $x.1.emplace_tuple_index_expr(Span::new($start, $end), $base, Span::new($dot_start, $dot_end), $value, Span::new($value_start, $value_end)).into()
    );
    ($x:ident range full $start:literal:$end:literal) => (
        $x.1.emplace_range_full_expr(Span::new($start, $end)).into()
    );
    ($x:ident range left $start:literal:$end:literal $base:expr) => (
        $x.1.emplace_range_left_expr(Span::new($start, $end), $base).into()
    );
    ($x:ident range right $start:literal:$end:literal $base:expr) => (
        $x.1.emplace_range_right_expr(Span::new($start, $end), $base).into()
    );
    ($x:ident range both $start:literal:$end:literal dotdot $dotdot_start:literal:$dotdot_end:literal $left:expr, $right:expr) => (
        $x.1.emplace_range_both_expr(Span::new($start, $end), $left, Span::new($dotdot_start, $dotdot_end), $right).into()
    );
}

macro_rules! make_stmt {
    // id and label has same implementation but has different semantic
    ($x:ident id $start:literal:$end:literal #$id:tt) => (
        IdSpan::new(make_isid!($x, $id), Span::new($start, $end))
    );
    ($x:ident label $start:literal:$end:literal #$id:tt) => (
        Some(IdSpan::new(make_isid!($x, $id), Span::new($start, $end)))
    );
    (label none) => (
        None
    );
    ($x:ident name $start:literal:$end:literal #$id:tt) => (
        $x.1.emplace_generic_name(Span::new($start, $end), IdSpan::new(make_isid!($x, $id), Span::new($start, $end)), Span::new(0, 0), Vec::new()).into()
    );
    ($x:ident name $start:literal:$end:literal #$id:tt $id_start:literal:$id_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),*$(,)?) => (
        $x.1.emplace_generic_name(Span::new($start, $end), IdSpan::new(make_isid!($x, $id), Span::new($start, $end)), Span::new($quote_start, $quote_end), vec![$($parameter,)*]).into()
    );
    // generic parameter currently is similar to idspan
    ($x:ident gp $start:literal:$end:literal #$id:tt) => (
        $x.1.emplace_generic_parameter(Span::new($start, $end), IdSpan::new(make_isid!($x, $id), Span::new($start, $end)))
    );
    ($x:ident body $start:literal:$end:literal $($item:expr),*$(,)?) => (
        $x.1.emplace_block(Span::new($start, $end), vec![$($item,)*]).into()
    );
    ($x:ident block $start:literal:$end:literal $label:expr, $body:expr) => (
        $x.1.emplace_block_stmt(Span::new($start, $end), $label, $body).into()
    );
    ($x:ident fp $start:literal:$end:literal #$id:tt $id_start:literal:$id_end:literal $type:expr) => (
        $x.1.emplace_fn_def_parameter(Span::new($start, $end), IdSpan::new(make_isid!($x, $id), Span::new($id_start, $id_end)), $type)
    );
    // too complex to make it look like pretty print, so it looks like struct literal, except 2 spans
    ($x:ident fn $(item)? $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal name: $name:expr, $($field:ident: $value:expr),+) => (
        $x.1.emplace_fn_def(Span::new($start, $end), $name, Span::new($quote_start, $quote_end), $($value,)+).into()
    );
    ($x:ident for $start:literal:$end:literal var #$iter_var:tt $iter_var_start:literal:$iter_var_end:literal $label:expr, $iter_expr:expr, $body:expr) => (
        $x.1.emplace_for_stmt(Span::new($start, $end), $label, IdSpan::new(make_isid!($x, $iter_var), Span::new($iter_var_start, $iter_var_end)), $iter_expr, $body).into()
    );
    ($x:ident loop $start:literal:$end:literal $label:expr, $body:expr) => (
        $x.1.emplace_loop_stmt(Span::new($start, $end), $label, $body).into()
    );
    ($x:ident while $start:literal:$end:literal $label:expr, $expr:expr, $body:expr) => (
        $x.1.emplace_while_stmt(Span::new($start, $end), $label, $expr, $body).into()
    );
    ($x:ident break $start:literal:$end:literal $label:expr) => (
        $x.1.emplace_break_stmt(Span::new($start, $end), $label).into()
    );
    ($x:ident continue $start:literal:$end:literal $label:expr) => (
        $x.1.emplace_continue_stmt(Span::new($start, $end), $label).into()
    );
    ($x:ident var $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        $x.1.emplace_var_decl_stmt(Span::new($start, $end), false, IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), $type, $init).into()
    );
    ($x:ident const $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        $x.1.emplace_var_decl_stmt(Span::new($start, $end), true, IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), $type, $init).into()
    );
    ($x:ident ret $start:literal:$end:literal $value:expr) => (
        $x.1.emplace_ret_stmt(Span::new($start, $end), $value).into()
    );
    ($x:ident expr $(item)? $start:literal:$end:literal $expr:expr) => (
        $x.1.emplace_simple_expr_stmt(Span::new($start, $end), $expr).into()
    );
    ($x:ident assign $(item)? $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (
        $x.1.emplace_assign_expr_stmt(Span::new($start, $end), $left, $right, Separator::$op, Span::new($op_start, $op_end)).into()
    );
    ($x:ident type $(item)? $start:literal:$end:literal $name:expr) => (
        $x.1.emplace_type_def(Span::new($start, $end), $name, None).into()
    );
    ($x:ident type $(item)? $start:literal:$end:literal $name:expr, $from:expr) => (
        $x.1.emplace_type_def(Span::new($start, $end), $name, Some($from)).into()
    );
    ($x:ident if $start:literal:$end:literal $if:expr, $($elseif:expr,)* else: $else: expr) => (
        $x.1.emplace_if_stmt(Span::new($start, $end), $if, vec![$($elseif,)*], $else).into()
    );
    ($x:ident if clause $start:literal:$end:literal $condition:expr, $body:expr) => (
        $x.1.emplace_if_clause(Span::new($start, $end), $condition, $body)
    );
    ($x:ident else $start:literal:$end:literal $body:expr) => (
        $x.1.emplace_else_clause(Span::new($start, $end), $body)
    );
    ($x:ident import $(item)? $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $path:expr) => (
        $x.1.emplace_module_stmt(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), $path).into()
    );
    // 'uses' not 'use': syntax highlight (the not semantic one) simply completely fails when 'use' is not correctly used
    ($x:ident uses $(item)? $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $path:expr) => (
        $x.1.emplace_use_stmt(Span::new($start, $end), $path, Some(IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)))).into()
    );
    ($x:ident uses $(item)? $start:literal:$end:literal $path:expr) => (
        $x.1.emplace_use_stmt(Span::new($start, $end), $path, None).into()
    );
    ($x:ident class $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $name:expr, $($item:expr),* $(,)?) => (
        $x.1.emplace_class_def(Span::new($start, $end), $name, Span::new($quote_start, $quote_end), vec![$($item,)*]).into()
    );
    ($x:ident enum $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $name:expr, $base:expr, $($variant:expr),*$(,)?) => (
        $x.1.emplace_enum_def(Span::new($start, $end), $name, $base, Span::new($quote_start, $quote_end), vec![$($variant,)*]).into()
    );
    ($x:ident variant $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal) => (
        $x.1.emplace_enum_def_variant(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), None)
    );
    ($x:ident variant $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $value:expr) => (
        $x.1.emplace_enum_def_variant(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), Some($value))
    );
    ($x:ident struct $start:literal:$end:literal $name:expr, $($field:expr),* $(,)?) => (
        $x.1.emplace_struct_def(Span::new($start, $end), $name, vec![$($field,)*]).into()
    );
    ($x:ident struct field $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal colon $colon_start:literal:$colon_end:literal $type:expr$(,)?) => (
        $x.1.emplace_field_def(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), Span::new($colon_start, $colon_end), $type)
    );
    // too complex to make it look like pretty print, so it looks like struct literal, except the span
    ($x:ident impl $start:literal:$end:literal $($field:ident: $value:expr),+) => (
        $x.1.emplace_impl_block(Span::new($start, $end), $($value,)+).into()
    );
    ($x:ident where $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $($constraint:expr),* $(,)?) => (
        $x.1.emplace_where_clause(Span::new($start, $end), IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end)), vec![$($constraint,)*])
    );
    // this is not statement, but put it here seems ok (include put parse_module test in stmt.rs)
    ($x:ident module $file_id:literal $($item:expr),*) => (
        $x.1.emplace_module(FileId::new($file_id), vec![$($item,)*])
    );
}

macro_rules! make_path {
    (segment global) => (
        TagIndex::new(PathSegment::Global)
    );
    ($x:ident segment simple $start:literal:$end:literal #$ident:tt) => (
        $x.1.emplace_simple_segment(Span::new($start, $end), make_isid!($x, $ident)).into()
    );
    ($x:ident segment cast $start:literal:$end:literal $left:expr, $right:expr) => (
        $x.1.emplace_cast_segment(Span::new($start, $end), $left, $right).into()
    );
    ($x:ident segment generic $start:literal:$end:literal #$ident:tt $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),*$(,)?) => (
        $x.1.emplace_generic_segment(Span::new($start, $end),
            IdSpan::new(make_isid!($x, $ident), Span::new($ident_start, $ident_end)),
            $x.1.emplace_type_list(Span::new($quote_start, $quote_end), vec![$($parameter,)*])).into()
    );
    ($x:ident $start:literal:$end:literal $($segment:expr),*$(,)?) => (
        $x.1.emplace_path(Span::new($start, $end), vec![$($segment,)*]).into()
    );
    ($x:ident simple $start:literal:$end:literal #$ident:tt) => (
        $x.1.emplace_path(Span::new($start, $end), vec![$x.1.emplace_simple_segment(Span::new($start, $end), make_isid!($x, $ident)).into()]).into()
    );
}

macro_rules! make_type {
    ($x:ident prim $(bare)? $start:literal:$end:literal $kw:ident) => (
        $x.1.emplace_primitive_type(Span::new($start, $end), Keyword::$kw).into()
    );
    ($x:ident ref $start:literal:$end:literal $inner:expr) => (
        $x.1.emplace_ref_type(Span::new($start, $end), $inner).into()
    );
    ($x:ident array $start:literal:$end:literal $base:expr, $size:expr) => (
        $x.1.emplace_array_type(Span::new($start, $end), $base, $size).into()
    );
    ($x:ident tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (
        $x.1.emplace_tuple_type(Span::new($start, $end), vec![$($item,)*]).into()
    );
    ($x:ident path $($tt:tt)+) => (
        make_path!($x $($tt)+)
    );
    ($x:ident simple $start:literal:$end:literal #$ident:tt) => (
        make_path!($x simple $start:$end #$ident)
    );
    ($x:ident fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?]) => (
        $x.1.emplace_fn_type(Span::new($start, $end), Span::new($paren_start, $paren_end), vec![$($parameter,)*], None).into()
    );
    ($x:ident fn ret $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?], $ret:expr) => (
        $x.1.emplace_fn_type(Span::new($start, $end), Span::new($paren_start, $paren_end), vec![$($parameter,)*], Some($ret)).into()
    );
    ($x:ident fp $start:literal:$end:literal $ty:expr) => (
        $x.1.emplace_fn_type_parameter(Span::new($start, $end), None, $ty)
    );
    ($x:ident fp named $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $ty:expr) => (
        $x.1.emplace_fn_type_parameter(Span::new($start, $end), Some(IdSpan::new(make_isid!($x, $name), Span::new($name_start, $name_end))), $ty)
    );
}

mod expr;
mod stmt;
mod path;
