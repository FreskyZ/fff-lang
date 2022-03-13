///! fff-lang
///!
///! syntax/expr

use crate::syntax::prelude::*;

#[macro_use] mod expr_list; // make_exprs
mod array_def;
mod binary_expr;
mod fn_call;
mod index_call;
mod lit_expr;
mod member_access;
mod name;
mod priority_proxy;
mod range_expr;
mod tuple_def;
mod unary_expr;

pub use lit_expr::LitValue;
pub use lit_expr::LitExpr;
pub use expr_list::ExprList;
pub use expr_list::ExprListParseResult;
pub use tuple_def::ParenExpr;
pub use tuple_def::TupleDef;
pub use array_def::ArrayDef;
pub use fn_call::FnCallExpr;
pub use index_call::IndexCallExpr;
pub use member_access::MemberAccessExpr;
pub use range_expr::RangeFullExpr;
pub use range_expr::RangeRightExpr;
pub use range_expr::RangeLeftExpr;
pub use range_expr::RangeBothExpr;
use range_expr::RangeExpr;
pub use binary_expr::BinaryExpr;
pub use unary_expr::UnaryExpr;
pub use priority_proxy::PostfixExpr;
pub use priority_proxy::PrimaryExpr;
pub use name::SimpleName;
pub use name::Name;

// 12 byte
#[cfg_attr(test, derive(PartialEq))]
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
impl ISyntaxFormat for Expr {
    fn format(&self, f: Formatter) -> String {
        match self {
            &Expr::Lit(ref lit_expr) => f.apply(lit_expr).finish(),
            &Expr::SimpleName(ref ident_expr) => f.apply(ident_expr).finish(),
            &Expr::Name(ref name) => f.apply(name).finish(),
            &Expr::Paren(ref paren_expr) => f.apply(paren_expr).finish(),
            &Expr::Tuple(ref tuple_def) => f.apply(tuple_def).finish(),
            &Expr::Array(ref array_def) => f.apply(array_def).finish(),
            &Expr::FnCall(ref fn_call) => f.apply(fn_call).finish(),
            &Expr::IndexCall(ref index_call) => f.apply(index_call).finish(),
            &Expr::MemberAccess(ref member_access) => f.apply(member_access).finish(),
            &Expr::Unary(ref unary_expr) => f.apply(unary_expr).finish(),
            &Expr::Binary(ref binary_expr) => f.apply(binary_expr).finish(),
            &Expr::RangeFull(ref range_full) => f.apply(range_full).finish(),
            &Expr::RangeRight(ref range_right) => f.apply(range_right).finish(),
            &Expr::RangeLeft(ref range_left) => f.apply(range_left).finish(),
            &Expr::RangeBoth(ref range_both) => f.apply(range_both).finish(),
        }
    }
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl Default for Expr {
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::from(0i32), Span::new(0, 0))) }
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
        let mut temp = Expr::Lit(LitExpr::new(LitValue::from(42), Span::new(0, 0)));
        ::std::mem::swap(&mut *this, &mut temp);
        temp
    }
}
impl ISyntaxGrammar for Expr {
    fn matches_first(tokens: [&Token; 3]) -> bool { 
        LitExpr::matches_first(tokens)
        || Name::matches_first(tokens)
        || TupleDef::matches_first(tokens)
        || ArrayDef::matches_first(tokens)
        || UnaryExpr::matches_first(tokens) 
        || matches!(tokens[0], &Token::Sep(Separator::DotDot) | &Token::Keyword(Keyword::This))
    }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for Expr where F: FileSystem {
    type Output = Expr;
    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Expr> { RangeExpr::parse(sess) }
}

#[cfg(test)] #[test]
fn expr_parse() {
    use super::{make_node};

    assert_eq!{ make_node!("\"abc\"" as Expr),
        Expr::Lit(LitExpr::new(2u32, Span::new(0, 4)))
    }
    assert_eq!{ make_node!("0xfffu64" as Expr), 
        Expr::Lit(LitExpr::new(LitValue::Num(Numeric::U64(0xFFFu64)), Span::new(0, 7)))
    }
    assert_eq!{ make_node!("'f'" as Expr), 
        Expr::Lit(LitExpr::new(LitValue::from('f'), Span::new(0, 2)))
    }
    assert_eq!{ make_node!("true" as Expr),
        Expr::Lit(LitExpr::new(LitValue::from(true), Span::new(0, 3)))
    }

    assert_eq!{ make_node!("binary_expr" as Expr),
        Expr::SimpleName(SimpleName::new(1, Span::new(0, 10)))
    }

    assert_eq!{ make_node!("(  )" as Expr),
        Expr::Lit(LitExpr::new(LitValue::Unit, Span::new(0, 3)))
    }
    
    // Case from fn_def_parse
    assert_eq!{ make_node!("println(this)" as Expr, [Span::new(0, 6)], ["this"]), 
        Expr::FnCall(FnCallExpr::new(
            Expr::SimpleName(SimpleName::new(1, Span::new(0, 6))),
            Span::new(7, 12), ExprList::new(vec![
                Expr::SimpleName(SimpleName::new(2, Span::new(8, 11)))
            ])
        ))
    }

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    assert_eq!{ make_node!("(1)" as Expr),
        Expr::Paren(ParenExpr::new(Span::new(0, 2), 
            LitExpr::new(LitValue::from(1i32), Span::new(1, 1))
        ))
    }
    // I can see future of Ok(())! 
    assert_eq!{ make_node!("(())" as Expr), 
        Expr::Paren(ParenExpr::new(Span::new(0, 3), 
            LitExpr::new(LitValue::Unit, Span::new(1, 2))
        ))
    }

    // Tuple def
    assert_eq!{ make_node!("(a, b)" as Expr),
        Expr::Tuple(TupleDef::new(Span::new(0, 5), make_exprs![
            SimpleName::new(1, Span::new(1, 1)),
            SimpleName::new(2, Span::new(4, 4)),
        ]))
    }        //  12345678901
    assert_eq!{ make_node!("(1, 2, 3, )" as Expr),
        Expr::Tuple(TupleDef::new(Span::new(0, 10), make_exprs![
            LitExpr::new(LitValue::from(1i32), Span::new(1, 1)),
            LitExpr::new(LitValue::from(2i32), Span::new(4, 4)),
            LitExpr::new(LitValue::from(3i32), Span::new(7, 7)),
        ]))
    }

    // Array def
    assert_eq!{ make_node!("[a]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(2, Span::new(1, 1))
        ]))
    }        //  12345678
    assert_eq!{ make_node!("[1, 2, ]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1i32), Span::new(1, 1)),
            LitExpr::new(LitValue::from(2i32), Span::new(4, 4))
        ]))
    }
    assert_eq!{ make_node!("[]" as Expr),
        Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![]))
    }

    // Member access
    assert_eq!{ make_node!("a.b" as Expr),
        Expr::MemberAccess(MemberAccessExpr::new(
            SimpleName::new(1, Span::new(0, 0)),
            Span::new(1, 1),
            SimpleName::new(2, Span::new(2, 2))
        ))
    }

    // function call
    assert_eq!{ make_node!("defg()" as Expr),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 5),
            make_exprs![]
        ))
    }
    assert_eq!{ make_node!("deg(a)" as Expr),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(2, Span::new(4, 4))
            ]
        ))
    }
    assert_eq!{ make_node!("degg(a, b, )" as Expr),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(2, Span::new(5, 5)),
                SimpleName::new(3, Span::new(8, 8))
            ]
        ))
    }
    //           0123456789
    assert_eq!{ make_node!("abc.defg()" as Expr),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(1, Span::new(0, 2)),
                Span::new(3, 3), 
                SimpleName::new(2, Span::new(4, 7))
            ),
            Span::new(8, 9), 
            make_exprs![]
        ))
    }
    assert_eq!{ make_node!("abc.deg(a)" as Expr),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(1, Span::new(0, 2)),
                Span::new(3, 3),
                SimpleName::new(2, Span::new(4, 6))
            ),
            Span::new(7, 9), make_exprs![
                SimpleName::new(3, Span::new(8, 8))
            ]
        ))
    }        //  12345678901234
    assert_eq!{ make_node!("1.degg(a, b, )" as Expr),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1i32), Span::new(0, 0)),
                Span::new(1, 1),
                SimpleName::new(1, Span::new(2, 5))
            ),
            Span::new(6, 13), make_exprs![
                SimpleName::new(2, Span::new(7, 7)),
                SimpleName::new(3, Span::new(10, 10))
            ]
        ))
    }   

    // get index       //  123456
    assert_eq!{ make_node!("deg[a]" as Expr),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(2, Span::new(4, 4))
            ]
        ))
    }        //  123456789012
    assert_eq!{ make_node!("degg[a, b, ]" as Expr),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(2, Span::new(5, 5)),
                SimpleName::new(3, Span::new(8, 8))
            ]
        ))
    }     

    //           123456
    assert_eq!{ make_node!("2[3].a" as Expr),
        Expr::MemberAccess(MemberAccessExpr::new(
            IndexCallExpr::new(
                LitExpr::new(LitValue::from(2i32), Span::new(0, 0)),
                Span::new(1, 3), make_exprs![
                    LitExpr::new(LitValue::from(3i32), Span::new(2, 2))
                ]
            ),
            Span::new(4, 4), 
            SimpleName::new(1, Span::new(5, 5))
        ))
    }   //  1234567890123456
    assert_eq!{ make_node!("print(233, ).bit" as Expr),
        Expr::MemberAccess(MemberAccessExpr::new(
            Expr::FnCall(FnCallExpr::new(
                SimpleName::new(1, Span::new(0, 4)),
                Span::new(5, 11), make_exprs![
                    LitExpr::new(LitValue::from(233i32), Span::new(6, 8))
                ]
            )),
            Span::new(12, 12),
            SimpleName::new(2, Span::new(13, 15))
        ))
    }            //  12345678901234
    assert_eq!{ make_node!("1.degg[a, b, ]" as Expr),
        Expr::IndexCall(IndexCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1i32), Span::new(0, 0)),
                Span::new(1, 1),
                SimpleName::new(1, Span::new(2, 5))
            ),
            Span::new(6, 13), make_exprs![
                SimpleName::new(2, Span::new(7, 7)),
                SimpleName::new(3, Span::new(10, 10)),
            ]
        ))
    }        

    assert_eq!{ make_node!("!~!1[1]" as Expr),
        Expr::Unary(UnaryExpr::new(
            Separator::Not, Span::new(0, 0),
            UnaryExpr::new(
                Separator::Tilde, Span::new(1, 1),
                UnaryExpr::new(
                    Separator::Not, Span::new(2, 2),
                    IndexCallExpr::new(
                        LitExpr::new(LitValue::from(1i32), Span::new(3, 3)),
                        Span::new(4, 6), make_exprs![
                            LitExpr::new(LitValue::from(1i32), Span::new(5, 5))
                        ]
                    )
                )
            )
        ))
    }

    // increase and decrease
    //           1234567
    assert_eq!{ make_node!("!!1" as Expr),
        Expr::Unary(UnaryExpr::new(
            Separator::Not, Span::new(0, 0), 
            UnaryExpr::new(
                Separator::Not, Span::new(1, 1),
                LitExpr::new(LitValue::from(1i32), Span::new(2, 2))
            )
        ))
    }

    // range
    assert_eq!{ make_node!(".." as Expr), 
        Expr::RangeFull(RangeFullExpr::new(Span::new(0, 1)))
    }

    assert_eq!{ make_node!("..1 + 2" as Expr),
        Expr::RangeRight(RangeRightExpr::new(Span::new(0, 6), BinaryExpr::new(
            LitExpr::new(LitValue::from(1i32), Span::new(2, 2)),
            Separator::Add, Span::new(4, 4),
            LitExpr::new(LitValue::from(2i32), Span::new(6, 6))
        )))
    }

    assert_eq!{ make_node!("xxx .." as Expr),
        Expr::RangeLeft(RangeLeftExpr::new(Span::new(0, 5), 
            SimpleName::new(1, Span::new(0, 2))
        ))
    }

    assert_eq!{ make_node!("1 + 2 .. [4, 5, 6][2]" as Expr),
        Expr::RangeBoth(RangeBothExpr::new(
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1i32), Span::new(0, 0)),
                Separator::Add, Span::new(2, 2),
                LitExpr::new(LitValue::from(2i32), Span::new(4, 4))
            ),
            Span::new(6, 7),
            IndexCallExpr::new(
                ArrayDef::new(Span::new(9, 17), make_exprs![
                    LitExpr::new(LitValue::from(4i32), Span::new(10, 10)),
                    LitExpr::new(LitValue::from(5i32), Span::new(13, 13)),
                    LitExpr::new(LitValue::from(6i32), Span::new(16, 16))
                ]),
                Span::new(18, 20), make_exprs![
                    LitExpr::new(LitValue::from(2i32), Span::new(19, 19))
                ]
            )
        ))
    }
}

#[cfg(test)] #[test]
fn expr_errors() {
    use super::make_node;

    assert_eq!{ make_node!("de(, )" as Expr, and messages), 
        (Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 1)), 
            Span::new(2, 5), make_exprs![]
        )), make_messages![
            Message::new_by_str(strings::UnexpectedSingleComma, vec![(Span::new(2, 5), strings::FnCallHere)])
        ])
    }

    //               0 12345678
    assert_eq!{ make_node!("\"\".de(, )" as Expr, and messages),
        (Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(2u32, Span::new(0, 1)),
                Span::new(2, 2), 
                SimpleName::new(2, Span::new(3, 4))
            ),
            Span::new(5, 8), make_exprs![]
        )), make_messages![
            Message::new_by_str(strings::UnexpectedSingleComma, vec![(Span::new(5, 8), strings::FnCallHere)])
        ])
    }

    assert_eq!{ make_node!("defg[]" as Expr, and messages),
        (Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 5), make_exprs![]
        )), make_messages![
            Message::new_by_str(strings::EmptyIndexCall, vec![(Span::new(4, 5), strings::IndexCallHere)])
        ])
    }

    //              123456
    assert_eq!{ make_node!("de[, ]" as Expr, and messages),
        (Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 1)),
            Span::new(2, 5), make_exprs![]
        )), make_messages![
            Message::new_by_str(strings::EmptyIndexCall, vec![(Span::new(2, 5), strings::IndexCallHere)])
        ])
    }
}

#[cfg(test)] #[test]
fn expr_unbox() {

    assert_eq!{ 
        Expr::unbox(Box::new(Expr::SimpleName(SimpleName::new(42, Span::new(30, 31))))), 
        Expr::SimpleName(SimpleName::new(42, Span::new(30, 31)))
    }
}
