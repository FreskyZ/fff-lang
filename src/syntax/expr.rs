///! syntax::expr

use super::prelude::*;
use super::*;

macro_rules! define_expr {
    ($($ty:ty => $variant:ident, $visit:ident, $span:ident,)+) => (
impl Node for Expr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        match self {
        $(
            Expr::$variant(e) => v.$visit(e),
        )+
        }
    }
}
    )
}

define_expr! {
    LitExpr => Lit, visit_lit_expr, span,
    Name => Name, visit_name, all_span,
    ParenExpr => Paren, visit_paren_expr, span,
    TupleDef => Tuple, visit_tuple_def, paren_span,
    ArrayDef => Array, visit_array_def, bracket_span,
    FnCallExpr => FnCall, visit_fn_call_expr, all_span,
    IndexCallExpr => IndexCall, visit_index_call_expr, all_span,
    MemberAccessExpr => MemberAccess, visit_member_access, all_span,
    ObjectLiteral => Object, visit_object_literal, all_span,
    UnaryExpr => Unary, visit_unary_expr, all_span,
    BinaryExpr => Binary, visit_binary_expr, all_span,
    RangeBothExpr => RangeBoth, visit_range_both_expr, all_span,
    RangeFullExpr => RangeFull, visit_range_full_expr, all_span,
    RangeLeftExpr => RangeLeft, visit_range_left_expr, all_span,
    RangeRightExpr => RangeRight, visit_range_right_expr, all_span,
}


impl Parser for Expr {
    type Output = Expr;

    fn matches(current: &Token) -> bool { 
        LitExpr::matches(current)
        || Name::matches(current)
        || TupleDef::matches(current)
        || ArrayDef::matches(current)
        || UnaryExpr::matches(current)
        || matches!(current, Token::Sep(Separator::DotDot) | Token::Keyword(Keyword::This))
    }

    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool { 
        LitExpr::matches3(current, peek, peek2)
        || Name::matches3(current, peek, peek2)
        || TupleDef::matches3(current, peek, peek2)
        || ArrayDef::matches3(current, peek, peek2)
        || UnaryExpr::matches3(current, peek, peek2)
        || matches!(current, Token::Sep(Separator::DotDot) | Token::Keyword(Keyword::This))
    }
    fn parse(cx: &mut ParseContext) -> Result<Expr, Unexpected> { 
        cx.expect::<RangeExpr>()
    }
}

impl Default for Expr {
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::Num(Numeric::I32(0)), Span::new(0, 0))) }
}

#[cfg(test)]
macro_rules! make_expr {
    // literals does not have (lit prefix because they are used frequently
    (unit $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Unit, span: Span::new($start, $end) }));
    (true $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Bool(true), span: Span::new($start, $end) }));
    (false $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Bool(false), span: Span::new($start, $end) }));
    (char $start:literal:$end:literal $v:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Char($v), span: Span::new($start, $end) }));
    (str #$v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Str(IsId::new($v)), span: Span::new($start, $end) }));
    (i32 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) }));
    (u8 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) }));
    (u32 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) }));
    (u64 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{value: crate::syntax::ast::LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) }));
    (r32 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) }));
    (r64 $v:literal $start:literal:$end:literal) => (
        crate::syntax::ast::Expr::Lit(crate::syntax::ast::LitExpr{ value: crate::syntax::ast::LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) }));
    (binary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $left:expr, $right:expr) => (crate::syntax::ast::Expr::Binary(crate::syntax::ast::BinaryExpr{
        left_expr: Box::new($left),
        right_expr: Box::new($right),
        operator: Separator::$op,
        operator_span: Span::new($op_start, $op_end),
        all_span: Span::new($start, $end),
    }));
    (unary $start:literal:$end:literal $op:ident $op_start:literal:$op_end:literal $base:expr) => (crate::syntax::ast::Expr::Unary(crate::syntax::ast::UnaryExpr{
        base: Box::new($base),
        operator: Separator::$op,
        operator_span: Span::new($op_start, $op_end),
        all_span: Span::new($start, $end),
    }));
    (member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal $base:expr, $name:expr) => (
        crate::syntax::ast::Expr::MemberAccess(crate::syntax::ast::MemberAccessExpr{
            base: Box::new($base),
            dot_span: Span::new($dot_start, $dot_end),
            name: $name,
            all_span: Span::new($start, $end),
        })
    );
    (array $start:literal:$end:literal $($item:expr),*$(,)?) => (crate::syntax::ast::Expr::Array(crate::syntax::ast::ArrayDef{
        bracket_span: Span::new($start, $end),
        items: crate::syntax::ast::ExprList {
            items: vec![$($item,)*],
        }
    }));
    (tuple $start:literal:$end:literal $($item:expr),*$(,)?) => (crate::syntax::ast::Expr::Tuple(crate::syntax::ast::TupleDef{
        paren_span: Span::new($start, $end),
        items: crate::syntax::ast::ExprList {
            items: vec![$($item,)*],
        }
    }));
    (paren $start:literal:$end:literal $base:expr) => (crate::syntax::ast::Expr::Paren(crate::syntax::ast::ParenExpr{
        expr: Box::new($base),
        span: Span::new($start, $end),
    }));
    (object $start:literal:$end:literal quote $quote_start:literal:$quote_end:literal $base:expr, $($field:expr),*$(,)?) => (
        crate::syntax::ast::Expr::Object(crate::syntax::ast::ObjectLiteral{
            base: Box::new($base),
            quote_span: Span::new($quote_start, $quote_end),
            all_span: Span::new($start, $end),
            fields: vec![$($field,)*],
        })
    );
    (object field $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal colon $colon_start:literal:$colon_end:literal $value:expr$(,)?) => (
        crate::syntax::ast::ObjectLiteralField{
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            colon_span: Span::new($colon_start, $colon_end),
            all_span: Span::new($start, $end),
            value: $value,
        }
    );
    (fn $start:literal:$end:literal paren $paren_start:literal:$paren_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        crate::syntax::ast::Expr::FnCall(crate::syntax::ast::FnCallExpr{
            base: Box::new($base),
            paren_span: Span::new($paren_start, $paren_end),
            all_span: Span::new($start, $end),
            params: crate::syntax::ast::ExprList{
                items: vec![$($parameter,)*],
            }
        })
    );
    (index $start:literal:$end:literal bracket $bracket_start:literal:$bracket_end:literal $base:expr, $($parameter:expr),*$(,)?) => (
        crate::syntax::ast::Expr::IndexCall(crate::syntax::ast::IndexCallExpr{
            base: Box::new($base),
            bracket_span: Span::new($bracket_start, $bracket_end),
            all_span: Span::new($start, $end),
            params: crate::syntax::ast::ExprList{
                items: vec![$($parameter,)*],
            }
        })
    );
    (range full $start:literal:$end:literal) => (crate::syntax::ast::Expr::RangeFull(crate::syntax::ast::RangeFullExpr{
        all_span: Span::new($start, $end),
    }));
    (range left $start:literal:$end:literal $base:expr) => (crate::syntax::ast::Expr::RangeLeft(crate::syntax::ast::RangeLeftExpr{
        all_span: Span::new($start, $end),
        expr: Box::new($base),
    }));
    (range right $start:literal:$end:literal $base:expr) => (crate::syntax::ast::Expr::RangeRight(crate::syntax::ast::RangeRightExpr{
        all_span: Span::new($start, $end),
        expr: Box::new($base),
    }));
    (range both $start:literal:$end:literal dotdot $dotdot_start:literal:$dotdot_end:literal $left:expr, $right:expr) => (
        crate::syntax::ast::Expr::RangeBoth(crate::syntax::ast::RangeBothExpr{
            all_span: Span::new($start, $end),
            op_span: Span::new($dotdot_start, $dotdot_end),
            left_expr: Box::new($left),
            right_expr: Box::new($right),
        })
    );
}
#[cfg(test)]
pub(crate) use make_expr;

#[cfg(test)] #[test]
fn expr_parse() {

    case!{ "\"abc\"" as Expr, make_expr!(str #2 0:4) }
    case!{ "0xfffu64" as Expr, make_expr!(u64 0xFFF 0:7) }
    case!{ "'f'" as Expr, make_expr!(char 0:2 'f') }
    case!{ "true" as Expr, make_expr!(true 0:3) }
    case!{ "binary_expr" as Expr, make_name!(simple 0:10 #2) }
    case!{ "(  )" as Expr, make_expr!(unit 0:3) }
    
    // Case from fn_def_parse
    case!{ "println(this)" as Expr, 
        make_expr!(fn 0:12 paren 7:12
            make_name!(simple 0:6 #2),
            make_name!(simple 8:11 #3))
    }

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    case!{ "(1)" as Expr,
        make_expr!(paren 0:2
            make_expr!(i32 1 1:1))
    }
    // I can see future of Ok(())! 
    case!{ "(())" as Expr,
        make_expr!(paren 0:3
            make_expr!(unit 1:2))
    }

    // Tuple def
    case!{ "(a, b)" as Expr,
        make_expr!(tuple 0:5
            make_name!(simple 1:1 #2),
            make_name!(simple 4:4 #3))
    }        //  12345678901
    case!{ "(1, 2, 3, )" as Expr,
        make_expr!(tuple 0:10
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 3 7:7))
    }

    // Array def
    case!{ "[a]" as Expr,
        make_expr!(array 0:2
            make_name!(simple 1:1 #2))
    }        //  12345678
    case!{ "[1, 2, ]" as Expr,
        make_expr!(array 0:7
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4))
    }
    case!{ "[]" as Expr, make_expr!(array 0:1) }

    // Member access
    case!{ "a.b" as Expr,
        make_expr!(member 0:2 dot 1:1
            make_name!(simple 0:0 #2),
            make_name!(simple bare 2:2 #3)),
    }

    // function call
    case!{ "defg()" as Expr,
        make_expr!(fn 0:5 paren 4:5
            make_name!(simple 0:3 #2),)
    }
    case!{ "deg(a)" as Expr,
        make_expr!(fn 0:5 paren 3:5
            make_name!(simple 0:2 #2),
            make_name!(simple 4:4 #3))
    }
    case!{ "degg(a, b, )" as Expr,
        make_expr!(fn 0:11 paren 4:11
            make_name!(simple 0:3 #2),
            make_name!(simple 5:5 #3),
            make_name!(simple 8:8 #4))
    }
    //           0123456789
    case!{ "abc.defg()" as Expr,
        make_expr!(fn 0:9 paren 8:9
            make_expr!(member 0:7 dot 3:3
                make_name!(simple 0:2 #2),
                make_name!(simple bare 4:7 #3)),)
    }
    case!{ "abc.deg(a)" as Expr,
        make_expr!(fn 0:9 paren 7:9
            make_expr!(member 0:6 dot 3:3
                make_name!(simple 0:2 #2),
                make_name!(simple bare 4:6 #3)),
            make_name!(simple 8:8 #4))
    }        //  12345678901234
    case!{ "1.degg(a, b, )" as Expr,
        make_expr!(fn 0:13 paren 6:13
            make_expr!(member 0:5 dot 1:1
                make_expr!(i32 1 0:0),
                make_name!(simple bare 2:5 #2)),
            make_name!(simple 7:7 #3),
            make_name!(simple 10:10 #4))
    }   

    // get index       //  123456
    case!{ "deg[a]" as Expr,
        make_expr!(index 0:5 bracket 3:5
            make_name!(simple 0:2 #2),
            make_name!(simple 4:4 #3))
    }        //  123456789012
    case!{ "degg[a, b, ]" as Expr,
        make_expr!(index 0:11 bracket 4:11
            make_name!(simple 0:3 #2),
            make_name!(simple 5:5 #3),
            make_name!(simple 8:8 #4))
    }     

    //           123456
    case!{ "2[3].a" as Expr,
        make_expr!(member 0:5 dot 4:4
            make_expr!(index 0:3 bracket 1:3
                make_expr!(i32 2 0:0),
                make_expr!(i32 3 2:2)),
            make_name!(simple bare 5:5 #2))
    }   //  1234567890123456
    case!{ "print(233, ).bit" as Expr,
        make_expr!(member 0:15 dot 12:12
            make_expr!(fn 0:11 paren 5:11
                make_name!(simple 0:4 #2),
                make_expr!(i32 233 6:8)),
            make_name!(simple bare 13:15 #3))
    }            //  12345678901234
    case!{ "1.degg[a, b, ]" as Expr,
        make_expr!(index 0:13 bracket 6:13
            make_expr!(member 0:5 dot 1:1
                make_expr!(i32 1 0:0),
                make_name!(simple bare 2:5 #2)),
            make_name!(simple 7:7 #3),
            make_name!(simple 10:10 #4))
    }        

    case!{ "!~!1[1]" as Expr,
        make_expr!(unary 0:6 Not 0:0
            make_expr!(unary 1:6 Tilde 1:1
                make_expr!(unary 2:6 Not 2:2
                    make_expr!(index 3:6 bracket 4:6
                        make_expr!(i32 1 3:3),
                        make_expr!(i32 1 5:5)))))
    }

    //           1234567
    case!{ "!!1" as Expr,
        make_expr!(unary 0:2 Not 0:0
            make_expr!(unary 1:2 Not 1:1
                make_expr!(i32 1 2:2)))
    }

    // range
    case!{ ".." as Expr,
        make_expr!(range full 0:1)
    }

    case!{ "..1 + 2" as Expr,
        make_expr!(range right 0:6
            make_expr!(binary 2:6 Add 4:4
                make_expr!(i32 1 2:2),
                make_expr!(i32 2 6:6)))
    }

    case!{ "xxx .." as Expr,
        make_expr!(range left 0:5
            make_name!(simple 0:2 #2))
    }

    case!{ "1 + 2 .. [4, 5, 6][2]" as Expr,
        make_expr!(range both 0:20 dotdot 6:7
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 2 4:4)),
            make_expr!(index 9:20 bracket 18:20
                make_expr!(array 9:17
                    make_expr!(i32 4 10:10),
                    make_expr!(i32 5 13:13),
                    make_expr!(i32 6 16:16)),
                make_expr!(i32 2 19:19)))
    }

    case!{ "de(, )" as Expr,
        make_expr!(fn 0:5 paren 2:5
            make_name!(simple 0:1 #2),
        ), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(2, 5), strings::FnCallHere)
        )
    }

    //               0 12345678
    case!{ "\"\".de(, )" as Expr,
        make_expr!(fn 0:8 paren 5:8
            make_expr!(member 0:4 dot 2:2
                make_expr!(str #1 0:1),
                make_name!(simple bare 3:4 #2)),
        ), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(5, 8), strings::FnCallHere)
        )
    }

    case!{ "defg[]" as Expr,
        make_expr!(index 0:5 bracket 4:5
            make_name!(simple 0:3 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(4, 5), strings::IndexCallHere)
        )
    }

    //              123456
    case!{ "de[, ]" as Expr,
        make_expr!(index 0:5 bracket 2:5
            make_name!(simple 0:1 #2),
        ), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(2, 5), strings::IndexCallHere)
        )
    }
}
