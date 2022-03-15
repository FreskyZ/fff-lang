///! syntax::expr

use super::prelude::*;
use super::{LitExpr, LitValue, SimpleName, Name, ParenExpr, TupleDef, ArrayDef, FnCallExpr, IndexCallExpr, 
    MemberAccessExpr, UnaryExpr, BinaryExpr, RangeExpr, RangeFullExpr, RangeRightExpr, RangeLeftExpr, RangeBothExpr};

// 12 byte
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Expr {
    Lit(LitExpr),
    SimpleName(SimpleName),
    Name(Name),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
    FnCall(FnCallExpr),
    IndexCall(IndexCallExpr),
    MemberAccess(MemberAccessExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    RangeFull(RangeFullExpr),
    RangeRight(RangeRightExpr),
    RangeLeft(RangeLeftExpr),
    RangeBoth(RangeBothExpr),
}
impl Default for Expr {
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::Num(Numeric::I32(0)), Span::new(0, 0))) }
}
impl Expr {

    pub fn get_all_span(&self) -> Span {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.span,
            &Expr::SimpleName(ref ident_expr) => ident_expr.span,
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

    pub fn unbox(this: Box<Expr>) -> Self {
        let mut this = this;
        let mut temp = Expr::Lit(LitExpr::new(LitValue::Num(Numeric::I32(42)), Span::new(0, 0)));
        ::std::mem::swap(&mut *this, &mut temp);
        temp
    }
}

impl Node for Expr {
    type ParseOutput = Expr;

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
    fn parse(cx: &mut ParseContext) -> ParseResult<Expr> { 
        cx.expect_node::<RangeExpr>()
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        match self {
            Expr::Lit(e) => v.visit_lit_expr(e),
            Expr::SimpleName(e) => v.visit_simple_name(e),
            Expr::Name(e) => v.visit_name(e),
            Expr::Paren(e) => v.visit_paren_expr(e),
            Expr::Tuple(e) => v.visit_tuple_def(e),
            Expr::Array(e) => v.visit_array_def(e),
            Expr::FnCall(e) => v.visit_fn_call_expr(e),
            Expr::IndexCall(e) => v.visit_index_call_expr(e),
            Expr::MemberAccess(e) => v.visit_member_access(e),
            Expr::Unary(e) => v.visit_unary_expr(e),
            Expr::Binary(e) => v.visit_binary_expr(e),
            Expr::RangeFull(e) => v.visit_range_full_expr(e),
            Expr::RangeRight(e) => v.visit_range_right_expr(e),
            Expr::RangeLeft(e) => v.visit_range_left_expr(e),
            Expr::RangeBoth(e) => v.visit_range_both_expr(e),
        }
    }
}

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
        Expr::SimpleName(SimpleName::new(2, Span::new(0, 10)))
    }

    case!{ "(  )" as Expr,
        Expr::Lit(make_lit!(unit, 0, 3))
    }
    
    // Case from fn_def_parse
    case!{ "println(this)" as Expr, 
        Expr::FnCall(FnCallExpr::new(
            Expr::SimpleName(SimpleName::new(2, Span::new(0, 6))),
            Span::new(7, 12), ExprList::new(vec![
                Expr::SimpleName(SimpleName::new(3, Span::new(8, 11)))
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
            SimpleName::new(2, Span::new(1, 1)),
            SimpleName::new(3, Span::new(4, 4)),
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
            SimpleName::new(2, Span::new(1, 1))
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
        Expr::MemberAccess(MemberAccessExpr::new(
            SimpleName::new(2, Span::new(0, 0)),
            Span::new(1, 1),
            SimpleName::new(3, Span::new(2, 2))
        ))
    }

    // function call
    case!{ "defg()" as Expr,
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(2, Span::new(0, 3)),
            Span::new(4, 5),
            make_exprs![]
        ))
    }
    case!{ "deg(a)" as Expr,
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(2, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(3, Span::new(4, 4))
            ]
        ))
    }
    case!{ "degg(a, b, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(2, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(3, Span::new(5, 5)),
                SimpleName::new(4, Span::new(8, 8))
            ]
        ))
    }
    //           0123456789
    case!{ "abc.defg()" as Expr,
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(2, Span::new(0, 2)),
                Span::new(3, 3), 
                SimpleName::new(3, Span::new(4, 7))
            ),
            Span::new(8, 9), 
            make_exprs![]
        ))
    }
    case!{ "abc.deg(a)" as Expr,
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(2, Span::new(0, 2)),
                Span::new(3, 3),
                SimpleName::new(3, Span::new(4, 6))
            ),
            Span::new(7, 9), make_exprs![
                SimpleName::new(4, Span::new(8, 8))
            ]
        ))
    }        //  12345678901234
    case!{ "1.degg(a, b, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                make_lit!(1, 0, 0),
                Span::new(1, 1),
                SimpleName::new(2, Span::new(2, 5))
            ),
            Span::new(6, 13), make_exprs![
                SimpleName::new(3, Span::new(7, 7)),
                SimpleName::new(4, Span::new(10, 10))
            ]
        ))
    }   

    // get index       //  123456
    case!{ "deg[a]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(3, Span::new(4, 4))
            ]
        ))
    }        //  123456789012
    case!{ "degg[a, b, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(3, Span::new(5, 5)),
                SimpleName::new(4, Span::new(8, 8))
            ]
        ))
    }     

    //           123456
    case!{ "2[3].a" as Expr,
        Expr::MemberAccess(MemberAccessExpr::new(
            IndexCallExpr::new(
                make_lit!(2, 0, 0),
                Span::new(1, 3), make_exprs![
                    make_lit!(3, 2, 2)
                ]
            ),
            Span::new(4, 4), 
            SimpleName::new(2, Span::new(5, 5))
        ))
    }   //  1234567890123456
    case!{ "print(233, ).bit" as Expr,
        Expr::MemberAccess(MemberAccessExpr::new(
            Expr::FnCall(FnCallExpr::new(
                SimpleName::new(2, Span::new(0, 4)),
                Span::new(5, 11), make_exprs![
                    make_lit!(233, 6, 8)
                ]
            )),
            Span::new(12, 12),
            SimpleName::new(3, Span::new(13, 15))
        ))
    }            //  12345678901234
    case!{ "1.degg[a, b, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            MemberAccessExpr::new(
                make_lit!(1, 0, 0),
                Span::new(1, 1),
                SimpleName::new(2, Span::new(2, 5))
            ),
            Span::new(6, 13), make_exprs![
                SimpleName::new(3, Span::new(7, 7)),
                SimpleName::new(4, Span::new(10, 10)),
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
            SimpleName::new(2, Span::new(0, 2))
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
            SimpleName::new(2, Span::new(0, 1)), 
            Span::new(2, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(2, 5), strings::FnCallHere)
        )
    }

    //               0 12345678
    case!{ "\"\".de(, )" as Expr,
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                make_lit!(1: str, 0, 1),
                Span::new(2, 2), 
                SimpleName::new(2, Span::new(3, 4))
            ),
            Span::new(5, 8), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(5, 8), strings::FnCallHere)
        )
    }

    case!{ "defg[]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 3)),
            Span::new(4, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(4, 5), strings::IndexCallHere)
        )
    }

    //              123456
    case!{ "de[, ]" as Expr,
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(2, Span::new(0, 1)),
            Span::new(2, 5), make_exprs![]
        )), errors make_errors!(
            e: e.emit(strings::EmptyIndexCall).detail(Span::new(2, 5), strings::IndexCallHere)
        )
    }
}

#[cfg(test)] #[test]
fn expr_unbox() {

    assert_eq!{ 
        Expr::unbox(Box::new(Expr::SimpleName(SimpleName::new(42, Span::new(30, 31))))), 
        Expr::SimpleName(SimpleName::new(42, Span::new(30, 31)))
    }
}
