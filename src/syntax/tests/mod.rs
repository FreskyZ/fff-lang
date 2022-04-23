
use std::fmt::{self, Write};
use std::str::from_utf8;
use crate::source::{SourceContext, VirtualFileSystem, Span, IdSpan, make_source};
use crate::diagnostics::{strings, make_errors};
use crate::lexical::{Numeric, Separator, Keyword};
use super::visit::Node;
use super::parser::{Parser, Unexpected};
use super::ast::*;

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

// the 2 types of case is same until result compare
fn case_until_node<
    V: PartialEq + fmt::Debug,
    P: FnOnce(&mut Parser) -> Result<V, Unexpected>,
    E: FnOnce(&mut Parser) -> V,
>(
    input: &'static str,
    actual_value_getter: P,
    expect_value_getter: E,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
    // err(actual node, expect node, source context)
) -> Result<(), (V, V, SourceContext<VirtualFileSystem>)> {
    let mut actual_diagnostics = crate::diagnostics::Diagnostics::new();
    let mut source = SourceContext::new_file_system(crate::source::VirtualFileSystem {
        cwd: "/".into(),
        files: [("1".into(), input.into())].into_iter().collect(),
    });
    let mut context = Parser::new(crate::lexical::Parser::new(source.entry("1", &mut actual_diagnostics).unwrap(), &mut actual_diagnostics));
    let actual_value = actual_value_getter(&mut context);
    if let Ok(actual_value) = actual_value {
        let expect_value = expect_value_getter(&mut context);
        context.finish();
        if actual_value == expect_value {
            if actual_diagnostics == expect_diagnostics {
                Ok(()) // finally success
            } else {
                let mut buf = format!("line {} diagnostics not same\n", backtrace);
                write!(buf, "{}", actual_diagnostics.display(&source)).unwrap();
                write!(buf, "{}", expect_diagnostics.display(&source)).unwrap();
                panic!("{}", buf)
            }
        } else {
            Err((actual_value, expect_value, source))
        }
    } else {
        context.finish();
        panic!("line {} parse failed\n{}", backtrace, actual_diagnostics.display(&source))
    }
}

fn ast_case<
    V: PartialEq + Node + fmt::Debug,
    P: FnOnce(&mut Parser) -> Result<V, Unexpected>,
    E: FnOnce(&mut Parser) -> V,
>(
    input: &'static str,
    actual_value_getter: P,
    expect_value_getter: E,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
) {
    if let Err((actual_node, expect_node, source)) = case_until_node(input, actual_value_getter, expect_value_getter, expect_diagnostics, backtrace) {
        let (actual_display, expect_display) = (actual_node.display(&source).to_string(), expect_node.display(&source).to_string());
        if actual_display == expect_display {
            let (actual_debug, expect_debug) = (format!("{:?}", actual_node), format!("{:?}", expect_node));
            panic!("line {} node not same while display is same\n{}\n{}", backtrace, actual_display, DiffDisplay(&actual_debug, &expect_debug));
        }
        panic!("line {} node not same\n{}", backtrace, DiffDisplay(&actual_display, &expect_display));
    }
}

// some parse method does not return type that impl Node
fn notast_case<
    V: PartialEq + fmt::Debug,
    P: FnOnce(&mut Parser) -> Result<V, Unexpected>,
    E: FnOnce(&mut Parser) -> V,
>(
    input: &'static str,
    actual_value_getter: P,
    expect_value_getter: E,
    expect_diagnostics: crate::diagnostics::Diagnostics,
    backtrace: u32,
) {
    if let Err((actual_node, expect_node, _)) = case_until_node(input, actual_value_getter, expect_value_getter, expect_diagnostics, backtrace) {
        let (actual_debug, expect_debug) = (format!("{:?}", actual_node), format!("{:?}", expect_node));
        panic!("line {} result not same\n{}", backtrace, DiffDisplay(&actual_debug, &expect_debug));
    }
}

macro_rules! case {
    // new
    ($parser:ident $code:literal, |$x:ident| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        ast_case($code, |cx| cx.$parser(), |$x| $expect, make_errors!($($($tt)+)?), line!());
    );
    (notast $parser:ident $code:literal, |$x:ident| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        notast_case($code, |cx| cx.$parser(), |$x| $expect, make_errors!($($($tt)+)?), line!());
    );

    // temp before x is required for creating every node
    ($parser:ident $code:literal, |_| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        ast_case($code, |cx| cx.$parser(), |_| $expect, make_errors!($($($tt)+)?), line!());
    );
    (notast $parser:ident $code:literal, |_| $expect:expr $(,)? $(,$($tt:tt)+)?) => (
        notast_case($code, |cx| cx.$parser(), |_| $expect, make_errors!($($($tt)+)?), line!());
    );
}

// used by following macros internally, redirect ident to literal, allowing direct #ident in these macros
// // this is approaching some level of quote!
macro_rules! make_isid {
    ($cx:ident, $i:ident) => ($cx.intern(stringify!($i)));
    ($cx:ident, $i:literal) => ($cx.intern($i));
}

// ATTENTION:
// these macros, make_path, make_expr, make_stmt, make_type
// should be same structure as pretty print in principle, this makes test case expect results look like pretty and looks pretty

macro_rules! make_expr {
    // literals does not have (lit prefix because they are used frequently
    (unit $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Unit, span: Span::new($start, $end) })
    );
    (true $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Bool(true), span: Span::new($start, $end) })
    );
    (false $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Bool(false), span: Span::new($start, $end) })
    );
    (char $start:literal:$end:literal $v:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Char($v), span: Span::new($start, $end) })
    );
    ($cx:ident str #$v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Str($cx.intern($v)), span: Span::new($start, $end) })
    );
    (i32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) })
    );
    (u8 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) })
    );
    (u32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) })
    );
    (u64 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{value: LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) })
    );
    (r32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) })
    );
    (r64 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) })
    );
    (path $($tt:tt)+) => (
        Expr::Path(make_path!($($tt)+))
    );
    ($cx:ident path $($tt:tt)+) => (
        Expr::Path(make_path!($cx $($tt)+))
    );
    (binary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (Expr::Binary(BinaryExpr{
        left: Box::new($left),
        right: Box::new($right),
        op: Separator::$op,
        op_span: Span::new($op_start, $op_end),
        span: Span::new($start, $end),
    }));
    (unary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $base:expr) => (Expr::Unary(UnaryExpr{
        base: Box::new($base),
        op: Separator::$op,
        op_span: Span::new($op_start, $op_end),
        span: Span::new($start, $end),
    }));
    ($cx:ident member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal #$ident:tt $ident_start:literal:$ident_end:literal $base:expr) => (
        Expr::Member(MemberExpr{
            span: Span::new($start, $end),
            base: Box::new($base),
            op_span: Span::new($dot_start, $dot_end),
            name: IdSpan::new(make_isid!($cx, $ident), Span::new($ident_start, $ident_end)),
            parameters: None,
        })
    );
    ($cx:ident member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal #$ident:tt $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        Expr::Member(MemberExpr{
            span: Span::new($start, $end),
            base: Box::new($base),
            op_span: Span::new($dot_start, $dot_end),
            name: IdSpan::new(make_isid!($cx, $ident), Span::new($ident_start, $ident_end)),
            parameters: Some(TypeList{ items: vec![$($parameter,)*], span: Span::new($quote_start, $quote_end) }),
        })
    );
    (array $start:literal:$end:literal $($item:expr),*$(,)?) => (Expr::Array(ArrayExpr{
        span: Span::new($start, $end),
        items: vec![$($item,)*],
    }));
    (tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (Expr::Tuple(TupleExpr{
        span: Span::new($start, $end),
        items: vec![$($item,)*],
    }));
    (paren $start:literal:$end:literal $base:expr) => (Expr::Paren(ParenExpr{
        base: Box::new($base),
        span: Span::new($start, $end),
    }));
    (object $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($field:expr),*$(,)?) => (
        Expr::Object(ObjectExpr{
            base: Box::new($base),
            quote_span: Span::new($quote_start, $quote_end),
            span: Span::new($start, $end),
            fields: vec![$($field,)*],
        })
    );
    ($cx:ident object field $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal colon $colon_start:literal:$colon_end:literal $value:expr$(,)?) => (
        ObjectExprField{
            name: IdSpan::new(make_isid!($cx, $name), Span::new($name_start, $name_end)),
            span: Span::new($start, $end),
            value: $value,
        }
    );
    (call $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        Expr::Call(CallExpr{
            base: Box::new($base),
            quote_span: Span::new($paren_start, $paren_end),
            span: Span::new($start, $end),
            parameters: vec![$($parameter,)*],
        })
    );
    (array index $start:literal:$end:literal bracket $bracket_start:literal:$bracket_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        Expr::ArrayIndex(ArrayIndexExpr{
            base: Box::new($base),
            quote_span: Span::new($bracket_start, $bracket_end),
            span: Span::new($start, $end),
            parameters: vec![$($parameter,)*],
        })
    );
    (tuple index $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal i32 $value:literal $value_start:literal:$value_end:literal $base:expr) => (
        Expr::TupleIndex(TupleIndexExpr{
            span: Span::new($start, $end),
            base: Box::new($base),
            op_span: Span::new($dot_start, $dot_end),
            value: (Numeric::I32($value), Span::new($value_start, $value_end)),
        })
    );
    (tuple index $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal numeric $variant:ident($value:literal) $value_start:literal:$value_end:literal $base:expr) => (
        Expr::TupleIndex(TupleIndexExpr{
            span: Span::new($start, $end),
            base: Box::new($base),
            op_span: Span::new($dot_start, $dot_end),
            value: (Numeric::$variant($value), Span::new($value_start, $value_end)),
        })
    );
    (range full $start:literal:$end:literal) => (Expr::RangeFull(RangeFullExpr{
        span: Span::new($start, $end),
    }));
    (range left $start:literal:$end:literal $base:expr) => (Expr::RangeLeft(RangeLeftExpr{
        span: Span::new($start, $end),
        base: Box::new($base),
    }));
    (range right $start:literal:$end:literal $base:expr) => (Expr::RangeRight(RangeRightExpr{
        span: Span::new($start, $end),
        base: Box::new($base),
    }));
    (range both $start:literal:$end:literal dotdot $dotdot_start:literal:$dotdot_end:literal $left:expr, $right:expr) => (
        Expr::RangeBoth(RangeBothExpr{
            span: Span::new($start, $end),
            op_span: Span::new($dotdot_start, $dotdot_end),
            left: Box::new($left),
            right: Box::new($right),
        })
    );
}

macro_rules! make_stmt {
    // id and label has same implementation but has different semantic
    ($cx:ident id $start:literal:$end:literal #$id:tt) => (
        IdSpan::new(make_isid!($cx, $id), Span::new($start, $end))
    );
    ($cx:ident label $start:literal:$end:literal #$id:tt) => (
        Some(IdSpan::new(make_isid!($cx, $id), Span::new($start, $end)))
    );
    (label none) => (
        None
    );
    ($cx:ident name $start:literal:$end:literal #$id:tt) => (
        GenericName{
            span: Span::new($start, $end),
            base: IdSpan::new(make_isid!($cx, $id), Span::new($start, $end)),
            quote_span: Span::new(0, 0),
            parameters: Vec::new(),
        }
    );
    ($cx:ident name $start:literal:$end:literal #$id:tt $id_start:literal:$id_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),*$(,)?) => (
        GenericName{
            span: Span::new($start, $end),
            base: IdSpan::new(make_isid!($cx, $id), Span::new($id_start, $id_end)),
            quote_span: Span::new($quote_start, $quote_end),
            parameters: vec![$($parameter,)*]
        }
    );
    // generic parameter currently is similar to idspan
    ($cx:ident gp $start:literal:$end:literal #$id:tt) => (
        GenericParameter{ span: Span::new($start, $end), name: IdSpan::new(make_isid!($cx, $id), Span::new($start, $end)) }
    );
    (block $start:literal:$end:literal $($item:expr),*$(,)?) => (
        Block{
            span: Span::new($start, $end),
            items: vec![$($item.into(),)*],
        }
    );
    // fn def is too long and recommend directly struct literal
    ($cx:ident fp $start:literal:$end:literal #$id:tt $id_start:literal:$id_end:literal $type:expr) => (
        FnDefParameter{
            span: Span::new($start, $end),
            name: IdSpan::new(make_isid!($cx, $id), Span::new($id_start, $id_end)),
            r#type: $type,
        }
    );
    ($cx:ident for $start:literal:$end:literal var #$iter_var:tt $iter_var_start:literal:$iter_var_end:literal $label:expr, $iter_expr:expr, $body:expr) => (
        ForStatement{
            label: $label,
            iter_name: IdSpan::new(make_isid!($cx, $iter_var), Span::new($iter_var_start, $iter_var_end)),
            iter_expr: $iter_expr,
            body: $body,
            span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal $label:expr, $body:expr) => (
        LoopStatement{
            label: $label,
            body: $body,
            span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal $label:expr, $expr:expr, $body:expr) => (
        WhileStatement{
            label: $label,
            condition: $expr,
            body: $body,
            span: Span::new($start, $end),
        }
    );
    (break $start:literal:$end:literal $label:expr) => (
        BreakStatement{
            label: $label,
            span: Span::new($start, $end),
        }
    );
    (continue $start:literal:$end:literal $label:expr) => (
        ContinueStatement{
            label: $label,
            span: Span::new($start, $end),
        }
    );
    ($cx:ident var $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        VarDeclStatement{
            r#const: false,
            name: IdSpan::new(make_isid!($cx, $name), Span::new($name_start, $name_end)),
            r#type: $type,
            init_value: $init,
            span: Span::new($start, $end),
        }
    );
    ($cx:ident const $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        VarDeclStatement{
            r#const: true,
            name: IdSpan::new(make_isid!($cx, $name), Span::new($name_start, $name_end)),
            r#type: $type,
            init_value: $init,
            span: Span::new($start, $end),
        }
    );
    (ret $start:literal:$end:literal $value:expr) => (
        ReturnStatement{
            span: Span::new($start, $end),
            value: $value,
        }
    );
    (expr $start:literal:$end:literal $expr:expr) => (
        SimpleExprStatement{
            span: Span::new($start, $end),
            expr: $expr,
        }
    );
    (assign $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (
        AssignExprStatement{
            span: Span::new($start, $end),
            left: $left,
            right: $right,
            op: Separator::$op,
            op_span: Span::new($op_start, $op_end),
        }
    );
    (type $start:literal:$end:literal $name:expr) => (
        TypeDef{
            span: Span::new($start, $end),
            name: $name,
            from: None,
        }
    );
    (type $start:literal:$end:literal $name:expr, $from:expr) => (
        TypeDef{
            span: Span::new($start, $end),
            name: $name,
            from: Some($from),
        }
    );
}

macro_rules! make_path {
    (segment global) => (
        PathSegment::Global
    );
    ($cx:ident segment simple $start:literal:$end:literal #$ident:tt) => (
        PathSegment::Simple(SimpleSegment{ span: Span::new($start, $end), name: make_isid!($cx, $ident) })
    );
    (segment cast $start:literal:$end:literal $left:expr, $right:expr) => (
        PathSegment::Cast(CastSegment{ span: Span::new($start, $end), left: $left, right: $right })
    );
    ($cx:ident segment generic $start:literal:$end:literal #$ident:tt $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),*$(,)?) => (
        PathSegment::Generic(GenericSegment{
            span: Span::new($start, $end),
            base: IdSpan::new(make_isid!($cx, $ident), Span::new($ident_start, $ident_end)),
            parameters: TypeList{ items: vec![$($parameter,)*], span: Span::new($quote_start, $quote_end) },
        })
    );
    ($start:literal:$end:literal $($segment:expr),*$(,)?) => (
        Path{ span: Span::new($start, $end), segments: vec![$($segment,)*] }
    );
    ($cx:ident simple $start:literal:$end:literal #$ident:tt) => (
        Path{ span: Span::new($start, $end), segments: vec![PathSegment::Simple(SimpleSegment{ name: make_isid!($cx, $ident), span: Span::new($start, $end) })] }
    );
}

macro_rules! make_type {
    (prim $start:literal:$end:literal $kw:ident) => (
        TypeRef::Primitive(PrimitiveType{ base: Keyword::$kw, span: Span::new($start, $end) })
    );
    (ref $start:literal:$end:literal $inner:expr) => (
        TypeRef::Ref(RefType{ span: Span::new($start, $end), base: Box::new($inner) })
    );
    (array $start:literal:$end:literal $base:expr, $size:expr) => (
        TypeRef::Array(ArrayType{ base: Box::new($base), size: $size, span: Span::new($start, $end) })
    );
    (tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (
        TypeRef::Tuple(TupleType{ parameters: vec![$($item,)*], span: Span::new($start, $end) })
    );
    (path $($tt:tt)+) => (
        TypeRef::Path(make_path!($($tt)+))
    );
    ($cx:ident simple $start:literal:$end:literal #$ident:tt) => (
        TypeRef::Path(Path{ span: Span::new($start, $end), segments: vec![PathSegment::Simple(SimpleSegment{ name: make_isid!($cx, $ident), span: Span::new($start, $end) })] })
    );
    (fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?]) => (TypeRef::Fn(FnType{
        quote_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: None,
        span: Span::new($start, $end),
    }));
    (fn ret $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?], $ret:expr) => (TypeRef::Fn(FnType{
        quote_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: Some(Box::new($ret)),
        span: Span::new($start, $end),
    }));
    (fp $start:literal:$end:literal $ty:expr) => (FnTypeParameter{
        name: None,
        r#type: $ty,
        span: Span::new($start, $end),
    });
    ($cx:ident fp named $start:literal:$end:literal #$name:tt $name_start:literal:$name_end:literal $ty:expr) => (FnTypeParameter{
        name: Some(IdSpan::new(make_isid!($cx, $name), Span::new($name_start, $name_end))),
        r#type: $ty,
        span: Span::new($start, $end),
    });
}

mod pretty;
mod expr;
mod stmt;
mod path;
