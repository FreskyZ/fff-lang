///! syntax::expr

use super::prelude::*;
use super::*;

macro_rules! define_expr {
    ($($ty:ty => $variant:ident, $visit:ident,)+) => (
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Expr {
$(
    $variant($ty),
)+
}

$( impl From<$ty> for Expr {
    fn from(s: $ty) -> Expr { Expr::$variant(s) }
} )+

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
    LitExpr => Lit, visit_lit_expr,
    Name => Name, visit_name,
    ParenExpr => Paren, visit_paren_expr,
    TupleDef => Tuple, visit_tuple_def,
    ArrayDef => Array, visit_array_def,
    FnCallExpr => FnCall, visit_fn_call_expr,
    IndexCallExpr => IndexCall, visit_index_call_expr,
    MemberAccessExpr => MemberAccess, visit_member_access,
    UnaryExpr => Unary, visit_unary_expr,
    BinaryExpr => Binary, visit_binary_expr,
    RangeBothExpr => RangeBoth, visit_range_both_expr,
    RangeFullExpr => RangeFull, visit_range_full_expr,
    RangeLeftExpr => RangeLeft, visit_range_left_expr,
    RangeRightExpr => RangeRight, visit_range_right_expr,
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

impl Expr {

    pub fn get_all_span(&self) -> Span {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.span,
            &Expr::Name(ref name) => name.all_span,
            &Expr::Paren(ref paren_expr) => paren_expr.span,
            &Expr::Tuple(ref tuple_def) => tuple_def.paren_span, 
            &Expr::Array(ref array_def) => array_def.bracket_span,
            &Expr::FnCall(ref fn_call) => fn_call.all_span,
            &Expr::IndexCall(ref index_call) => index_call.all_span,
            &Expr::MemberAccess(ref member_access) => member_access.all_span,
            &Expr::Unary(ref unary_expr) => unary_expr.all_span,
            &Expr::Binary(ref binary_expr) => binary_expr.all_span,
            &Expr::RangeFull(ref range_full) => range_full.all_span,
            &Expr::RangeRight(ref range_right) => range_right.all_span,
            &Expr::RangeLeft(ref range_left) => range_left.all_span,
            &Expr::RangeBoth(ref range_both) => range_both.all_span,
        }
    }
}

#[cfg(test)]
macro_rules! make_lit {
    (unit, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Unit, span: Span::new($start, $end) });
    (true, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(true), span: Span::new($start, $end) });
    (false, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(false), span: Span::new($start, $end) });
    ($v:literal: char, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Char($v), span: Span::new($start, $end) });
    ($v:literal: str, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Str(IsId::new($v)), span: Span::new($start, $end) });
    // only i32 can omit type
    ($v:literal, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) });
    ($v:literal: u8, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) });
    ($v:literal: u32, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) });
    ($v:literal: u64, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) });
    ($v:literal: r32, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) });
    ($v:literal: r64, $start:expr, $end:expr) => (crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) });
}
#[cfg(test)]
pub(crate) use make_lit;

#[cfg(test)]
macro_rules! make_expr {
    // literals does not have (lit prefix because they are used frequently
    (unit, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Unit, span: Span::new($start, $end) }));
    (true, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(true), span: Span::new($start, $end) }));
    (false, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Bool(false), span: Span::new($start, $end) }));
    ($v:literal: char, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Char($v), span: Span::new($start, $end) }));
    ($v:literal: str, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Str(IsId::new($v)), span: Span::new($start, $end) }));
    ($v:literal: i32, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::I32($v)), span: Span::new($start, $end) }));
    ($v:literal: u8, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U8($v)), span: Span::new($start, $end) }));
    ($v:literal: u32, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::U32($v)), span: Span::new($start, $end) }));
    ($v:literal: u64, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{value: crate::syntax::LitValue::Num(Numeric::U64($v)), span: Span::new($start, $end) }));
    ($v:literal: r32, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R32($v)), span: Span::new($start, $end) }));
    ($v:literal: r64, $start:expr, $end:expr) => (
        crate::syntax::Expr::Lit(crate::syntax::LitExpr{ value: crate::syntax::LitValue::Num(Numeric::R64($v)), span: Span::new($start, $end) }));
    (binary $start:expr, $end:expr, $op:ident, $op_start:expr, $op_end:expr, $left:expr, $right:expr) => (crate::syntax::Expr::Binary(crate::syntax::BinaryExpr{
        left_expr: Box::new($left),
        right_expr: Box::new($right),
        operator: Separator::$op,
        operator_span: Span::new($op_start, $op_end),
        all_span: Span::new($start, $end),
    }));
    (member $start:literal:$end:literal dot $dot_start:literal:$dot_end:literal #$name:literal $name_start:literal:$name_end:literal $base:expr) => (
        crate::syntax::Expr::MemberAccess(crate::syntax::MemberAccessExpr{
            // TODO: remove the into when all expr variants change to this macro
            base: Box::new($base.into()),
            dot_span: Span::new($dot_start, $dot_end),
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            all_span: Span::new($start, $end),
        })
    );
}
#[cfg(test)]
pub(crate) use make_expr;

#[cfg(test)] #[test]
fn expr_parse() {
    use super::{ExprList};

    case!{ "\"abc\"" as Expr,
        Expr::Lit(make_lit!(2: str, 0, 4))
    }
    case!{ "0xfffu64" as Expr, 
        Expr::Lit(make_lit!(0xFFF: u64, 0, 7))
    }
    case!{ "'f'" as Expr, 
        Expr::Lit(make_lit!('f': char, 0, 2))
    }
    case!{ "true" as Expr,
        Expr::Lit(make_lit!(true, 0, 3))
    }

    case!{ "binary_expr" as Expr,
        make_name!(simple 0:10 #2),
    }

    case!{ "(  )" as Expr,
        Expr::Lit(make_lit!(unit, 0, 3))
    }
    
    // Case from fn_def_parse
    case!{ "println(this)" as Expr, 
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:6 #2),
            Span::new(7, 12), ExprList::new(vec![
                make_name!(simple 8:11 #3)
            ])
        ))
    }

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    case!{ "(1)" as Expr,
        Expr::Paren(ParenExpr::new(Span::new(0, 2), 
            make_lit!(1, 1, 1)
        ))
    }
    // I can see future of Ok(())! 
    case!{ "(())" as Expr, 
        Expr::Paren(ParenExpr::new(Span::new(0, 3), 
            make_lit!(unit, 1, 2)
        ))
    }

    // Tuple def
    case!{ "(a, b)" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 5), make_exprs![
            make_name!(simple 1:1 #2),
            make_name!(simple 4:4 #3),
        ]))
    }        //  12345678901
    case!{ "(1, 2, 3, )" as Expr,
        Expr::Tuple(TupleDef::new(Span::new(0, 10), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!(2, 4, 4),
            make_lit!(3, 7, 7),
        ]))
    }

    // Array def
    case!{ "[a]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            make_name!(simple 1:1 #2)
        ]))
    }        //  12345678
    case!{ "[1, 2, ]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            make_lit!(1, 1, 1),
            make_lit!(2, 4, 4)
        ]))
    }
    case!{ "[]" as Expr,
        Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![]))
    }

    // Member access
    case!{ "a.b" as Expr,
        make_expr!(member 0:2 dot 1:1 #3 2:2
            make_name!(simple 0:0 #2)),
    }

    // function call
    case!{ "defg()" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:3 #2),
            Span::new(4, 5),
            make_exprs![]
        ))
    }
    case!{ "deg(a)" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:2 #2),
            Span::new(3, 5), make_exprs![
                make_name!(simple 4:4 #3)
            ]
        ))
    }
    case!{ "degg(a, b, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:3 #2),
            Span::new(4, 11), make_exprs![
                make_name!(simple 5:5 #3),
                make_name!(simple 8:8 #4)
            ]
        ))
    }
    //           0123456789
    case!{ "abc.defg()" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_expr!(member 0:7 dot 3:3 #3 4:7
                make_name!(simple 0:2 #2)),
            Span::new(8, 9), 
            make_exprs![]
        ))
    }
    case!{ "abc.deg(a)" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_expr!(member 0:6 dot 3:3 #3 4:6
                make_name!(simple 0:2 #2)),
            Span::new(7, 9), make_exprs![
                make_name!(simple 8:8 #4)
            ]
        ))
    }        //  12345678901234
    case!{ "1.degg(a, b, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_expr!(member 0:5 dot 1:1 #2 2:5
                make_expr!(1: i32, 0, 0)),
            Span::new(6, 13), make_exprs![
                make_name!(simple 7:7 #3),
                make_name!(simple 10:10 #4)
            ]
        ))
    }   

    // get index       //  123456
    case!{ "deg[a]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:2 #2),
            Span::new(3, 5), make_exprs![
                make_name!(simple 4:4 #3)
            ]
        ))
    }        //  123456789012
    case!{ "degg[a, b, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:3 #2),
            Span::new(4, 11), make_exprs![
                make_name!(simple 5:5 #3),
                make_name!(simple 8:8 #4)
            ]
        ))
    }     

    //           123456
    case!{ "2[3].a" as Expr,
        make_expr!(member 0:5 dot 4:4 #2 5:5
            IndexCallExpr::new(
                make_lit!(2, 0, 0),
                Span::new(1, 3), make_exprs![
                    make_lit!(3, 2, 2)
                ]
            ))
    }   //  1234567890123456
    case!{ "print(233, ).bit" as Expr,
        make_expr!(member 0:15 dot 12:12 #3 13:15
            Expr::FnCall(FnCallExpr::new(
                make_name!(simple 0:4 #2),
                Span::new(5, 11), make_exprs![
                    make_lit!(233, 6, 8)
                ]
            )))
    }            //  12345678901234
    case!{ "1.degg[a, b, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            make_expr!(member 0:5 dot 1:1 #2 2:5
                make_expr!(1: i32, 0, 0)),
            Span::new(6, 13), make_exprs![
                make_name!(simple 7:7 #3),
                make_name!(simple 10:10 #4),
            ]
        ))
    }        

    case!{ "!~!1[1]" as Expr,
        Expr::Unary(UnaryExpr::new(
            Separator::Not, Span::new(0, 0),
            UnaryExpr::new(
                Separator::Tilde, Span::new(1, 1),
                UnaryExpr::new(
                    Separator::Not, Span::new(2, 2),
                    IndexCallExpr::new(
                        make_lit!(1, 3, 3),
                        Span::new(4, 6), make_exprs![
                            make_lit!(1, 5, 5)
                        ]
                    )
                )
            )
        ))
    }

    // increase and decrease
    //           1234567
    case!{ "!!1" as Expr,
        Expr::Unary(UnaryExpr::new(
            Separator::Not, Span::new(0, 0), 
            UnaryExpr::new(
                Separator::Not, Span::new(1, 1),
                make_lit!(1, 2, 2)
            )
        ))
    }

    // range
    case!{ ".." as Expr, 
        Expr::RangeFull(RangeFullExpr::new(Span::new(0, 1)))
    }

    case!{ "..1 + 2" as Expr,
        Expr::RangeRight(RangeRightExpr::new(Span::new(0, 6), BinaryExpr::new(
            make_lit!(1, 2, 2),
            Separator::Add, Span::new(4, 4),
            make_lit!(2, 6, 6)
        )))
    }

    case!{ "xxx .." as Expr,
        Expr::RangeLeft(RangeLeftExpr::new(Span::new(0, 5), 
            make_name!(simple 0:2 #2)
        ))
    }

    case!{ "1 + 2 .. [4, 5, 6][2]" as Expr,
        Expr::RangeBoth(RangeBothExpr::new(
            BinaryExpr::new(
                make_lit!(1, 0, 0),
                Separator::Add, Span::new(2, 2),
                make_lit!(2, 4, 4)
            ),
            Span::new(6, 7),
            IndexCallExpr::new(
                ArrayDef::new(Span::new(9, 17), make_exprs![
                    make_lit!(4, 10, 10),
                    make_lit!(5, 13, 13),
                    make_lit!(6, 16, 16)
                ]),
                Span::new(18, 20), make_exprs![
                    make_lit!(2, 19, 19)
                ]
            )
        ))
    }
}

#[cfg(test)] #[test]
fn expr_errors() {

    case!{ "de(, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_name!(simple 0:1 #2), 
            Span::new(2, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(2, 5), strings::FnCallHere)
        )
    }

    //               0 12345678
    case!{ "\"\".de(, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            make_expr!(member 0:4 dot 2:2 #2 3:4
                make_expr!(1: str, 0, 1)),
            Span::new(5, 8), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(5, 8), strings::FnCallHere)
        )
    }

    case!{ "defg[]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:3 #2),
            Span::new(4, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(4, 5), strings::IndexCallHere)
        )
    }

    //              123456
    case!{ "de[, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            make_name!(simple 0:1 #2),
            Span::new(2, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(2, 5), strings::IndexCallHere)
        )
    }
}
