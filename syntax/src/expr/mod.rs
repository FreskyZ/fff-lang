///! fff-lang
///!
///! syntax/expr

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::LitValue;
use lexical::Keyword;
use lexical::Seperator;

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

pub use self::lit_expr::LitExpr;
pub use self::expr_list::ExprList;
pub use self::expr_list::ExprListParseResult;
pub use self::tuple_def::ParenExpr;
pub use self::tuple_def::TupleDef;
pub use self::array_def::ArrayDef;
pub use self::fn_call::FnCallExpr;
pub use self::index_call::IndexCallExpr;
pub use self::member_access::MemberAccessExpr;
pub use self::range_expr::RangeFullExpr;
pub use self::range_expr::RangeRightExpr;
pub use self::range_expr::RangeLeftExpr;
pub use self::range_expr::RangeBothExpr;
use self::range_expr::RangeExpr;
pub use self::binary_expr::BinaryExpr;
pub use self::unary_expr::UnaryExpr;
pub use self::priority_proxy::PostfixExpr;
pub use self::priority_proxy::PrimaryExpr;
pub use self::name::SimpleName;
pub use self::name::Name;

use super::Formatter;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;
use super::ISyntaxFormat;
use super::ISyntaxGrammar;

// 12 byte
#[cfg_attr(test, derive(Eq, PartialEq))]
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
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::from(0), Span::default())) }
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
        let mut temp = Expr::Lit(LitExpr::new(LitValue::from(42), Span::default()));
        ::std::mem::swap(&mut *this, &mut temp);
        temp
    }
}
impl ISyntaxGrammar for Expr {
    fn matches_first(tokens: &[&Token]) -> bool { 
        LitExpr::matches_first(tokens)
        || Name::matches_first(tokens)
        || TupleDef::matches_first(tokens)
        || ArrayDef::matches_first(tokens)
        || UnaryExpr::matches_first(tokens) 
        || tokens[0] == &Token::Sep(Seperator::Range)
        || tokens[0] == &Token::Keyword(Keyword::This)
    }
}
impl ISyntaxParse for Expr {
    type Output = Expr;
    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> { RangeExpr::parse(sess) }
}

#[cfg(test)] #[test]
fn expr_parse() {
    use codemap::Span;
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use lexical::Seperator;
    use super::WithTestInput;
    use super::TestInput;

    assert_eq!{ Expr::with_test_str("\"abc\""),
        Expr::Lit(LitExpr::new(make_lit!(str, 1), make_span!(0, 4)))
    }
    assert_eq!{ Expr::with_test_str("0xfffu64"), 
        Expr::Lit(LitExpr::new(LitValue::from(0xFFFu64), make_span!(0, 7)))
    }
    assert_eq!{ Expr::with_test_str("'f'"), 
        Expr::Lit(LitExpr::new(LitValue::from('f'), make_span!(0, 2)))
    }
    assert_eq!{ Expr::with_test_str("true"),
        Expr::Lit(LitExpr::new(LitValue::from(true), make_span!(0, 3)))
    }

    assert_eq!{ Expr::with_test_str("binary_expr"),
        Expr::SimpleName(SimpleName::new(make_id!(1), make_span!(0, 10)))
    }

    assert_eq!{ Expr::with_test_str("(  )"),
        Expr::Lit(LitExpr::new(LitValue::Unit, make_span!(0, 3)))
    }
    
    // Case from fn_def_parse
    TestInput::new("println(this)").set_syms(make_symbols!["println", "this"])
        .apply::<Expr, Expr>()
        .expect_no_message()
        .expect_result(Expr::FnCall(FnCallExpr::new(
            Expr::SimpleName(SimpleName::new(make_id!(1), make_span!(0, 6))),
            make_span!(7, 12), ExprList::new(vec![
                Expr::SimpleName(SimpleName::new(make_id!(2), make_span!(8, 11)))
            ])
        )))
        .finish();

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    assert_eq!{ Expr::with_test_str("(1)"),
        Expr::Paren(ParenExpr::new(make_span!(0, 2), 
            LitExpr::new(LitValue::from(1), make_span!(1, 1))
        ))
    }
    // I can see future of Ok(())! 
    assert_eq!{ Expr::with_test_str("(())"), 
        Expr::Paren(ParenExpr::new(make_span!(0, 3), 
            LitExpr::new(LitValue::Unit, make_span!(1, 2))
        ))
    }

    // Tuple def
    assert_eq!{ Expr::with_test_str("(a, b)"),
        Expr::Tuple(TupleDef::new(make_span!(0, 5), make_exprs![
            SimpleName::new(make_id!(1), make_span!(1, 1)),
            SimpleName::new(make_id!(2), make_span!(4, 4)),
        ]))
    }        //  12345678901
    assert_eq!{ Expr::with_test_str("(1, 2, 3, )"),
        Expr::Tuple(TupleDef::new(make_span!(0, 10), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 1)),
            LitExpr::new(LitValue::from(2), make_span!(4, 4)),
            LitExpr::new(LitValue::from(3), make_span!(7, 7)),
        ]))
    }

    // Array def
    assert_eq!{ Expr::with_test_str("[a]"),
        Expr::Array(ArrayDef::new(make_span!(0, 2), make_exprs![
            SimpleName::new(make_id!(1), make_span!(1, 1))
        ]))
    }        //  12345678
    assert_eq!{ Expr::with_test_str("[1, 2, ]"),
        Expr::Array(ArrayDef::new(make_span!(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1), make_span!(1, 1)),
            LitExpr::new(LitValue::from(2), make_span!(4, 4))
        ]))
    }
    assert_eq!{ Expr::with_test_str("[]"),
        Expr::Array(ArrayDef::new(make_span!(0, 1), make_exprs![]))
    }

    // Member access
    assert_eq!{ Expr::with_test_str("a.b"),
        Expr::MemberAccess(MemberAccessExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 0)),
            make_span!(1, 1),
            SimpleName::new(make_id!(2), make_span!(2, 2))
        ))
    }

    // function call
    assert_eq!{ Expr::with_test_str("defg()"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 5),
            make_exprs![]
        ))
    }
    assert_eq!{ Expr::with_test_str("deg(a)"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 2)),
            make_span!(3, 5), make_exprs![
                SimpleName::new(make_id!(2), make_span!(4, 4))
            ]
        ))
    }
    assert_eq!{ Expr::with_test_str("degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 11), make_exprs![
                SimpleName::new(make_id!(2), make_span!(5, 5)),
                SimpleName::new(make_id!(3), make_span!(8, 8))
            ]
        ))
    }
    //           0123456789
    assert_eq!{ Expr::with_test_str("abc.defg()"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(make_id!(1), make_span!(0, 2)),
                make_span!(3, 3), 
                SimpleName::new(make_id!(2), make_span!(4, 7))
            ),
            make_span!(8, 9), 
            make_exprs![]
        ))
    }
    assert_eq!{ Expr::with_test_str("abc.deg(a)"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                SimpleName::new(make_id!(1), make_span!(0, 2)),
                make_span!(3, 3),
                SimpleName::new(make_id!(2), make_span!(4, 6))
            ),
            make_span!(7, 9), make_exprs![
                SimpleName::new(make_id!(3), make_span!(8, 8))
            ]
        ))
    }        //  12345678901234
    assert_eq!{ Expr::with_test_str("1.degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                make_span!(1, 1),
                SimpleName::new(make_id!(1), make_span!(2, 5))
            ),
            make_span!(6, 13), make_exprs![
                SimpleName::new(make_id!(2), make_span!(7, 7)),
                SimpleName::new(make_id!(3), make_span!(10, 10))
            ]
        ))
    }   

    // get index       //  123456
    assert_eq!{ Expr::with_test_str("deg[a]"),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 2)),
            make_span!(3, 5), make_exprs![
                SimpleName::new(make_id!(2), make_span!(4, 4))
            ]
        ))
    }        //  123456789012
    assert_eq!{ Expr::with_test_str("degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 11), make_exprs![
                SimpleName::new(make_id!(2), make_span!(5, 5)),
                SimpleName::new(make_id!(3), make_span!(8, 8))
            ]
        ))
    }     

    //           123456
    assert_eq!{ Expr::with_test_str("2[3].a"),
        Expr::MemberAccess(MemberAccessExpr::new(
            IndexCallExpr::new(
                LitExpr::new(LitValue::from(2), make_span!(0, 0)),
                make_span!(1, 3), make_exprs![
                    LitExpr::new(LitValue::from(3), make_span!(2, 2))
                ]
            ),
            make_span!(4, 4), 
            SimpleName::new(make_id!(1), make_span!(5, 5))
        ))
    }   //  1234567890123456
    assert_eq!{ Expr::with_test_str("print(233, ).bit"),
        Expr::MemberAccess(MemberAccessExpr::new(
            Expr::FnCall(FnCallExpr::new(
                SimpleName::new(make_id!(1), make_span!(0, 4)),
                make_span!(5, 11), make_exprs![
                    LitExpr::new(LitValue::from(233), make_span!(6, 8))
                ]
            )),
            make_span!(12, 12),
            SimpleName::new(make_id!(2), make_span!(13, 15))
        ))
    }            //  12345678901234
    assert_eq!{ Expr::with_test_str("1.degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                make_span!(1, 1),
                SimpleName::new(make_id!(1), make_span!(2, 5))
            ),
            make_span!(6, 13), make_exprs![
                SimpleName::new(make_id!(2), make_span!(7, 7)),
                SimpleName::new(make_id!(3), make_span!(10, 10)),
            ]
        ))
    }        

    assert_eq!{ Expr::with_test_str("!~!1[1]"),
        Expr::Unary(UnaryExpr::new(
            Seperator::LogicalNot, make_span!(0, 0),
            UnaryExpr::new(
                Seperator::BitNot, make_span!(1, 1),
                UnaryExpr::new(
                    Seperator::LogicalNot, make_span!(2, 2),
                    IndexCallExpr::new(
                        LitExpr::new(LitValue::from(1), make_span!(3, 3)),
                        make_span!(4, 6), make_exprs![
                            LitExpr::new(LitValue::from(1), make_span!(5, 5))
                        ]
                    )
                )
            )
        ))
    }

    // increase and decrease
    //           1234567
    assert_eq!{ Expr::with_test_str("!!1"),
        Expr::Unary(UnaryExpr::new(
            Seperator::LogicalNot, make_span!(0, 0), 
            UnaryExpr::new(
                Seperator::LogicalNot, make_span!(1, 1),
                LitExpr::new(LitValue::from(1), make_span!(2, 2))
            )
        ))
    }

    // range
    assert_eq!{ Expr::with_test_str(".."), 
        Expr::RangeFull(RangeFullExpr::new(make_span!(0, 1)))
    }

    assert_eq!{ Expr::with_test_str("..1 + 2"),
        Expr::RangeRight(RangeRightExpr::new(make_span!(0, 6), BinaryExpr::new(
            LitExpr::new(LitValue::from(1), make_span!(2, 2)),
            Seperator::Add, make_span!(4, 4),
            LitExpr::new(LitValue::from(2), make_span!(6, 6))
        )))
    }

    assert_eq!{ Expr::with_test_str("xxx .."),
        Expr::RangeLeft(RangeLeftExpr::new(make_span!(0, 5), 
            SimpleName::new(make_id!(1), make_span!(0, 2))
        ))
    }

    assert_eq!{ Expr::with_test_str("1 + 2 .. [4, 5, 6][2]"),
        Expr::RangeBoth(RangeBothExpr::new(
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                Seperator::Add, make_span!(2, 2),
                LitExpr::new(LitValue::from(2), make_span!(4, 4))
            ),
            make_span!(6, 7),
            IndexCallExpr::new(
                ArrayDef::new(make_span!(9, 17), make_exprs![
                    LitExpr::new(LitValue::from(4), make_span!(10, 10)),
                    LitExpr::new(LitValue::from(5), make_span!(13, 13)),
                    LitExpr::new(LitValue::from(6), make_span!(16, 16))
                ]),
                make_span!(18, 20), make_exprs![
                    LitExpr::new(LitValue::from(2), make_span!(19, 19))
                ]
            )
        ))
    }
}

#[cfg(test)] #[test]
fn expr_errors() {
    use codemap::Span;
    use message::Message;
    use message::MessageCollection;
    use super::error_strings;
    use super::TestInput;

    TestInput::new("de(, )")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(2, 5), error_strings::FnCallHere)])
        ])
        .expect_result(Expr::FnCall(FnCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 1)), 
            make_span!(2, 5), make_exprs![]
        )))
        .finish();

    //               0 12345678
    TestInput::new("\"\".de(, )")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(5, 8), error_strings::FnCallHere)])
        ])
        .expect_result(Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(make_lit!(str, 1), make_span!(0, 1)),
                make_span!(2, 2), 
                SimpleName::new(make_id!(2), make_span!(3, 4))
            ),
            make_span!(5, 8), make_exprs![]
        )))
        .finish();

    TestInput::new("defg[]")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(4, 5), error_strings::IndexCallHere)])
        ])
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 5), make_exprs![]
        )))
        .finish();

    //              123456
    TestInput::new("de[, ]")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(2, 5), error_strings::IndexCallHere)])
        ])
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(make_id!(1), make_span!(0, 1)),
            make_span!(2, 5), make_exprs![]
        )))
        .finish();
}

#[cfg(test)] #[test]
fn expr_unbox() {

    assert_eq!{ 
        Expr::unbox(Box::new(Expr::SimpleName(SimpleName::new(make_id!(42), make_span!(30, 31))))), 
        Expr::SimpleName(SimpleName::new(make_id!(42), make_span!(30, 31)))
    }
}
