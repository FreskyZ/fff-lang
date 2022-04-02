
use std::fmt;
use crate::source::{Span, IsId};
use crate::diagnostics::strings;
use crate::lexical::{Numeric, Separator, Keyword};
use super::visit::Node;
use super::parser::{Parser, ParseContext, Unexpected, PostfixExpr, RangeExpr};
use super::ast::*;

fn ast_test_case<
    O: PartialEq + Node + fmt::Debug,
    F: FnOnce(&mut ParseContext) -> Result<O, Unexpected>,
>(
    input: &'static str,
    f: F,
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
    if let Ok(actual_node) = f(&mut context) {
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

macro_rules! case {
    ($code:literal as $ty:ty, $expect:expr $(,)?) => (
        ast_test_case($code, <$ty>::parse, $expect, crate::diagnostics::make_errors!(), &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr $(,)?) => (
        ast_test_case($code, <$ty>::parse, $expect, $expect_diagnostics, &[], line!());
    );
    ($code:literal as $ty:ty, $expect:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case($code, <$ty>::parse, $expect, crate::diagnostics::make_errors![], &$expect_strings, line!());
    );
    ($code:literal as $ty:ty, $expect:expr, errors $expect_diagnostics:expr, strings $expect_strings:expr $(,)?) => (
        ast_test_case($code, <$ty>::parse, $expect, $expect_diagnostics, &$expect_strings, line!());
    );
}

macro_rules! make_name {
    (simple $start:literal:$end:literal #$id:literal) => (
        make_name!($start:$end false, None, make_name!(segment $start:$end #$id)));
    ($start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (
        Expr::Name(Name{ type_as_segment: $as, global: $global, all_span: Span::new($start, $end), segments: vec![$($segment,)*] }));
    (segment $start:literal:$end:literal #$id:literal) => (
        NameSegment::Normal(IsId::new($id), Span::new($start, $end)));
    (segment generic $start:literal:$end:literal $($ty:expr),*$(,)?) => (
        NameSegment::Generic(vec![$($ty,)*], Span::new($start, $end)));
    // bare version for use outside of expr
    (simple bare $start:literal:$end:literal #$id:literal) => (
        make_name!(bare $start:$end false, None, make_name!(segment $start:$end #$id)));
    (bare $start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (
        Name{ type_as_segment: $as, global: $global, all_span: Span::new($start, $end), segments: vec![$($segment,)*] });
}

macro_rules! make_expr {
    // literals does not have (lit prefix because they are used frequently
    (unit $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Unit, span: Span::new($start, $end) }));
    (true $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Bool(true), span: Span::new($start, $end) }));
    (false $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Bool(false), span: Span::new($start, $end) }));
    (char $start:literal:$end:literal $v:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Char($v), span: Span::new($start, $end) }));
    (str #$v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Str(IsId::new($v)), span: Span::new($start, $end) }));
    (i32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) }));
    (u8 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) }));
    (u32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) }));
    (u64 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{value: LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) }));
    (r32 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) }));
    (r64 $v:literal $start:literal:$end:literal) => (
        Expr::Lit(LitExpr{ value: LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) }));
    (binary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (Expr::Binary(BinaryExpr{
        left_expr: Box::new($left),
        right_expr: Box::new($right),
        operator: Separator::$op,
        operator_span: Span::new($op_start, $op_end),
        all_span: Span::new($start, $end),
    }));
    (unary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $base:expr) => (Expr::Unary(UnaryExpr{
        base: Box::new($base),
        operator: Separator::$op,
        operator_span: Span::new($op_start, $op_end),
        all_span: Span::new($start, $end),
    }));
    (member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal $base:expr, $name:expr) => (
        Expr::MemberAccess(MemberAccessExpr{
            base: Box::new($base),
            dot_span: Span::new($dot_start, $dot_end),
            name: $name,
            all_span: Span::new($start, $end),
        })
    );
    (array $start:literal:$end:literal $($item:expr),*$(,)?) => (Expr::Array(ArrayDef{
        bracket_span: Span::new($start, $end),
        items: ExprList {
            items: vec![$($item,)*],
        }
    }));
    (tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (Expr::Tuple(TupleDef{
        paren_span: Span::new($start, $end),
        items: ExprList {
            items: vec![$($item,)*],
        }
    }));
    (paren $start:literal:$end:literal $base:expr) => (Expr::Paren(ParenExpr{
        expr: Box::new($base),
        span: Span::new($start, $end),
    }));
    (object $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($field:expr),*$(,)?) => (
        Expr::Object(ObjectLiteral{
            base: Box::new($base),
            quote_span: Span::new($quote_start, $quote_end),
            all_span: Span::new($start, $end),
            fields: vec![$($field,)*],
        })
    );
    (object field $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal colon $colon_start:literal:$colon_end:literal $value:expr$(,)?) => (
        ObjectLiteralField{
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            colon_span: Span::new($colon_start, $colon_end),
            all_span: Span::new($start, $end),
            value: $value,
        }
    );
    (fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        Expr::FnCall(FnCallExpr{
            base: Box::new($base),
            paren_span: Span::new($paren_start, $paren_end),
            all_span: Span::new($start, $end),
            params: ExprList{
                items: vec![$($parameter,)*],
            }
        })
    );
    (index $start:literal:$end:literal bracket $bracket_start:literal:$bracket_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        Expr::IndexCall(IndexCallExpr{
            base: Box::new($base),
            bracket_span: Span::new($bracket_start, $bracket_end),
            all_span: Span::new($start, $end),
            params: ExprList{
                items: vec![$($parameter,)*],
            }
        })
    );
    (range full $start:literal:$end:literal) => (Expr::RangeFull(RangeFullExpr{
        all_span: Span::new($start, $end),
    }));
    (range left $start:literal:$end:literal $base:expr) => (Expr::RangeLeft(RangeLeftExpr{
        all_span: Span::new($start, $end),
        expr: Box::new($base),
    }));
    (range right $start:literal:$end:literal $base:expr) => (Expr::RangeRight(RangeRightExpr{
        all_span: Span::new($start, $end),
        expr: Box::new($base),
    }));
    (range both $start:literal:$end:literal dotdot $dotdot_start:literal:$dotdot_end:literal $left:expr, $right:expr) => (
        Expr::RangeBoth(RangeBothExpr{
            all_span: Span::new($start, $end),
            op_span: Span::new($dotdot_start, $dotdot_end),
            left_expr: Box::new($left),
            right_expr: Box::new($right),
        })
    );
}

macro_rules! make_stmt {
    (for $start:literal:$end:literal for $for_start:literal:$for_end:literal var #$iter_var:literal $iter_var_start:literal:$iter_var_end:literal $iter_expr:expr, $body:expr) => (
        ForStatement{
            loop_name: None,
            for_span: Span::new($for_start, $for_end),
            iter_name: IsId::new($iter_var),
            iter_span: Span::new($iter_var_start, $iter_var_end),
            iter_expr: $iter_expr,
            body: $body,
            all_span: Span::new($start, $end),
        }
    );
    (for $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal for $for_start:literal:$for_end:literal var #$iter_var:literal $iter_var_start:literal:$iter_var_end:literal $iter_expr:expr, $body:expr) => (
        ForStatement{
            loop_name: Some(LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            for_span: Span::new($for_start, $for_end),
            iter_name: IsId::new($iter_var),
            iter_span: Span::new($iter_var_start, $iter_var_end),
            iter_expr: $iter_expr,
            body: $body,
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        LoopStatement{
            name: None,
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        LoopStatement{
            name: Some(LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        WhileStatement{
            name: None,
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        WhileStatement{
            name: Some(LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (break $start:literal:$end:literal) => (
        BreakStatement(JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (break $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        BreakStatement(JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal) => (
        ContinueStatement(JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        ContinueStatement(JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (var $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        VarDeclStatement{
            is_const: false,
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            r#type: $type,
            init_expr: $init,
            all_span: Span::new($start, $end),
        }
    );
    (const $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        VarDeclStatement{
            is_const: true,
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            r#type: $type,
            init_expr: $init,
            all_span: Span::new($start, $end),
        }
    );
}

macro_rules! make_type {
    (prim $start:literal:$end:literal $kw:ident) => (
        TypeRef::Primitive(PrimitiveType{ name: Keyword::$kw, span: Span::new($start, $end) }));
    (ref $start:literal:$end:literal $inner:expr) => (
        TypeRef::Ref(RefType{ span: Span::new($start, $end), base: Box::new($inner) }));
    (array $start:literal:$end:literal $base:expr, $size:expr) => (
        TypeRef::Array(ArrayType{ base: Box::new($base), size: $size, span: Span::new($start, $end) }));
    (tuple $start:literal:$end:literal [$($item:expr),*$(,)?]) => (
        TypeRef::Tuple(TupleType{ items: vec![$($item,)*], span: Span::new($start, $end) }));
    (segment $start:literal:$end:literal #$ident:literal) => (TypeSegment{ 
        ident: IsId::new($ident), 
        ident_span: Span::new($start, $end), 
        quote_span: Span::new(0, 0),
        parameters: Vec::new(),
        all_span: Span::new($start, $end),
    });
    (segment generic $start:literal:$end:literal #$ident:literal $ident_start:literal:$ident_end:literal quote $quote_start:literal:$quote_end:literal $($parameter:expr),*$(,)?) => (
        TypeSegment{
            ident: IsId::new($ident),
            ident_span: Span::new($ident_start, $ident_end),
            quote_span: Span::new($quote_start, $quote_end),
            parameters: vec![$($parameter,)*],
            all_span: Span::new($start, $end),
        }
    );
    (segment as $start:literal:$end:literal $from:expr, $to:expr) => (Some(TypeAsSegment{
        from: Box::new($from),
        to: Box::new($to),
        span: Span::new($start, $end),
    }));
    (plain $start:literal:$end:literal $global:expr, $as:expr, $($segment:expr),*$(,)?) => (TypeRef::Plain(PlainType{
        type_as_segment: $as,
        global: $global,
        segments: vec![$($segment,)*],
        all_span: Span::new($start, $end),
    }));
    (simple $start:literal:$end:literal #$id:literal) => (
        make_type!(plain $start:$end false, None, make_type!(segment $start:$end #$id)));
    (fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?]) => (TypeRef::Fn(FnType{
        paren_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: None,
        all_span: Span::new($start, $end),
    }));
    (fn ret $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal [$($parameter:expr),*$(,)?], $ret:expr) => (TypeRef::Fn(FnType{
        paren_span: Span::new($paren_start, $paren_end),
        parameters: vec![$($parameter,)*],
        ret_type: Some(Box::new($ret)),
        all_span: Span::new($start, $end),
    }));
    (param $start:literal:$end:literal $ty:expr) => (FnTypeParam{
        name: None,
        r#type: $ty,
        all_span: Span::new($start, $end),
    });
    (param named $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $ty:expr) => (FnTypeParam{
        name: Some((IsId::new($name), Span::new($name_start, $name_end))),
        r#type: $ty,
        all_span: Span::new($start, $end),
    });
}

use crate::diagnostics::make_errors;
use crate::source::make_source;

mod pretty;
mod expr;
mod statement;
mod item;
mod other;
