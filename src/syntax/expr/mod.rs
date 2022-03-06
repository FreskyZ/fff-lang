///! fff-lang
///!
///! syntax/expr

use std::fmt;
use crate::source::Span;
use crate::lexical::{Token, Keyword, Separator};

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
    use crate::source::Span;
    use crate::source::SymbolCollection;
    use crate::lexical::LitValue;
    use crate::lexical::Seperator;
    use super::WithTestInput;
    use super::TestInput;

    assert_eq!{ make_node!("\"abc\""),
        Expr::Lit(LitExpr::new(make_lit!(str, 1), Span::new(0, 4)))
    }
    assert_eq!{ make_node!("0xfffu64"), 
        Expr::Lit(LitExpr::new(LitValue::from(0xFFFu64), Span::new(0, 7)))
    }
    assert_eq!{ make_node!("'f'"), 
        Expr::Lit(LitExpr::new(LitValue::from('f'), Span::new(0, 2)))
    }
    assert_eq!{ make_node!("true"),
        Expr::Lit(LitExpr::new(LitValue::from(true), Span::new(0, 3)))
    }

    assert_eq!{ make_node!("binary_expr"),
        Expr::SimpleName(SimpleName::new(1, Span::new(0, 10)))
    }

    assert_eq!{ make_node!("(  )"),
        Expr::Lit(LitExpr::new(LitValue::Unit, Span::new(0, 3)))
    }
    
    // Case from fn_def_parse
    TestInput::new("println(this)").set_syms(make_symbols!["println", "this"])
        .apply::<Expr, Expr>()
        .expect_no_message()
        .expect_result(Expr::FnCall(FnCallExpr::new(
            Expr::SimpleName(SimpleName::new(1, Span::new(0, 6))),
            Span::new(7, 12), ExprList::new(vec![
                Expr::SimpleName(SimpleName::new(2, Span::new(8, 11)))
            ])
        )))
        .finish();

    // Very very legacy expr tests which originally contains ExpressionBase and ExpressionOperator
    // update them to current syntax to help improve coverage and make me happy

    // Unit
    assert_eq!{ make_node!("(1)"),
        Expr::Paren(ParenExpr::new(Span::new(0, 2), 
            LitExpr::new(LitValue::from(1), Span::new(1, 1))
        ))
    }
    // I can see future of Ok(())! 
    assert_eq!{ make_node!("(())"), 
        Expr::Paren(ParenExpr::new(Span::new(0, 3), 
            LitExpr::new(LitValue::Unit, Span::new(1, 2))
        ))
    }

    // Tuple def
    assert_eq!{ make_node!("(a, b)"),
        Expr::Tuple(TupleDef::new(Span::new(0, 5), make_exprs![
            SimpleName::new(1, Span::new(1, 1)),
            SimpleName::new(2, Span::new(4, 4)),
        ]))
    }        //  12345678901
    assert_eq!{ make_node!("(1, 2, 3, )"),
        Expr::Tuple(TupleDef::new(Span::new(0, 10), make_exprs![
            LitExpr::new(LitValue::from(1), Span::new(1, 1)),
            LitExpr::new(LitValue::from(2), Span::new(4, 4)),
            LitExpr::new(LitValue::from(3), Span::new(7, 7)),
        ]))
    }

    // Array def
    assert_eq!{ make_node!("[a]"),
        Expr::Array(ArrayDef::new(Span::new(0, 2), make_exprs![
            SimpleName::new(1, Span::new(1, 1))
        ]))
    }        //  12345678
    assert_eq!{ make_node!("[1, 2, ]"),
        Expr::Array(ArrayDef::new(Span::new(0, 7), make_exprs![
            LitExpr::new(LitValue::from(1), Span::new(1, 1)),
            LitExpr::new(LitValue::from(2), Span::new(4, 4))
        ]))
    }
    assert_eq!{ make_node!("[]"),
        Expr::Array(ArrayDef::new(Span::new(0, 1), make_exprs![]))
    }

    // Member access
    assert_eq!{ make_node!("a.b"),
        Expr::MemberAccess(MemberAccessExpr::new(
            SimpleName::new(1, Span::new(0, 0)),
            Span::new(1, 1),
            SimpleName::new(2, Span::new(2, 2))
        ))
    }

    // function call
    assert_eq!{ make_node!("defg()"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 5),
            make_exprs![]
        ))
    }
    assert_eq!{ make_node!("deg(a)"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(2, Span::new(4, 4))
            ]
        ))
    }
    assert_eq!{ make_node!("degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(2, Span::new(5, 5)),
                SimpleName::new(3, Span::new(8, 8))
            ]
        ))
    }
    //           0123456789
    assert_eq!{ make_node!("abc.defg()"),
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
    assert_eq!{ make_node!("abc.deg(a)"),
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
    assert_eq!{ make_node!("1.degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), Span::new(0, 0)),
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
    assert_eq!{ make_node!("deg[a]"),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 2)),
            Span::new(3, 5), make_exprs![
                SimpleName::new(2, Span::new(4, 4))
            ]
        ))
    }        //  123456789012
    assert_eq!{ make_node!("degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 11), make_exprs![
                SimpleName::new(2, Span::new(5, 5)),
                SimpleName::new(3, Span::new(8, 8))
            ]
        ))
    }     

    //           123456
    assert_eq!{ make_node!("2[3].a"),
        Expr::MemberAccess(MemberAccessExpr::new(
            IndexCallExpr::new(
                LitExpr::new(LitValue::from(2), Span::new(0, 0)),
                Span::new(1, 3), make_exprs![
                    LitExpr::new(LitValue::from(3), Span::new(2, 2))
                ]
            ),
            Span::new(4, 4), 
            SimpleName::new(1, Span::new(5, 5))
        ))
    }   //  1234567890123456
    assert_eq!{ make_node!("print(233, ).bit"),
        Expr::MemberAccess(MemberAccessExpr::new(
            Expr::FnCall(FnCallExpr::new(
                SimpleName::new(1, Span::new(0, 4)),
                Span::new(5, 11), make_exprs![
                    LitExpr::new(LitValue::from(233), Span::new(6, 8))
                ]
            )),
            Span::new(12, 12),
            SimpleName::new(2, Span::new(13, 15))
        ))
    }            //  12345678901234
    assert_eq!{ make_node!("1.degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), Span::new(0, 0)),
                Span::new(1, 1),
                SimpleName::new(1, Span::new(2, 5))
            ),
            Span::new(6, 13), make_exprs![
                SimpleName::new(2, Span::new(7, 7)),
                SimpleName::new(3, Span::new(10, 10)),
            ]
        ))
    }        

    assert_eq!{ make_node!("!~!1[1]"),
        Expr::Unary(UnaryExpr::new(
            Seperator::LogicalNot, Span::new(0, 0),
            UnaryExpr::new(
                Seperator::BitNot, Span::new(1, 1),
                UnaryExpr::new(
                    Seperator::LogicalNot, Span::new(2, 2),
                    IndexCallExpr::new(
                        LitExpr::new(LitValue::from(1), Span::new(3, 3)),
                        Span::new(4, 6), make_exprs![
                            LitExpr::new(LitValue::from(1), Span::new(5, 5))
                        ]
                    )
                )
            )
        ))
    }

    // increase and decrease
    //           1234567
    assert_eq!{ make_node!("!!1"),
        Expr::Unary(UnaryExpr::new(
            Seperator::LogicalNot, Span::new(0, 0), 
            UnaryExpr::new(
                Seperator::LogicalNot, Span::new(1, 1),
                LitExpr::new(LitValue::from(1), Span::new(2, 2))
            )
        ))
    }

    // range
    assert_eq!{ make_node!(".."), 
        Expr::RangeFull(RangeFullExpr::new(Span::new(0, 1)))
    }

    assert_eq!{ make_node!("..1 + 2"),
        Expr::RangeRight(RangeRightExpr::new(Span::new(0, 6), BinaryExpr::new(
            LitExpr::new(LitValue::from(1), Span::new(2, 2)),
            Seperator::Add, Span::new(4, 4),
            LitExpr::new(LitValue::from(2), Span::new(6, 6))
        )))
    }

    assert_eq!{ make_node!("xxx .."),
        Expr::RangeLeft(RangeLeftExpr::new(Span::new(0, 5), 
            SimpleName::new(1, Span::new(0, 2))
        ))
    }

    assert_eq!{ make_node!("1 + 2 .. [4, 5, 6][2]"),
        Expr::RangeBoth(RangeBothExpr::new(
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), Span::new(0, 0)),
                Seperator::Add, Span::new(2, 2),
                LitExpr::new(LitValue::from(2), Span::new(4, 4))
            ),
            Span::new(6, 7),
            IndexCallExpr::new(
                ArrayDef::new(Span::new(9, 17), make_exprs![
                    LitExpr::new(LitValue::from(4), Span::new(10, 10)),
                    LitExpr::new(LitValue::from(5), Span::new(13, 13)),
                    LitExpr::new(LitValue::from(6), Span::new(16, 16))
                ]),
                Span::new(18, 20), make_exprs![
                    LitExpr::new(LitValue::from(2), Span::new(19, 19))
                ]
            )
        ))
    }
}

#[cfg(test)] #[test]
fn expr_errors() {
    use crate::source::Span;
    use crate::diagnostics::Message;
    use crate::diagnostics::MessageCollection;
    use super::error_strings;
    use super::TestInput;

    TestInput::new("de(, )")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(Span::new(2, 5), error_strings::FnCallHere)])
        ])
        .expect_result(Expr::FnCall(FnCallExpr::new(
            SimpleName::new(1, Span::new(0, 1)), 
            Span::new(2, 5), make_exprs![]
        )))
        .finish();

    //               0 12345678
    TestInput::new("\"\".de(, )")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(Span::new(5, 8), error_strings::FnCallHere)])
        ])
        .expect_result(Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(make_lit!(str, 1), Span::new(0, 1)),
                Span::new(2, 2), 
                SimpleName::new(2, Span::new(3, 4))
            ),
            Span::new(5, 8), make_exprs![]
        )))
        .finish();

    TestInput::new("defg[]")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(Span::new(4, 5), error_strings::IndexCallHere)])
        ])
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 3)),
            Span::new(4, 5), make_exprs![]
        )))
        .finish();

    //              123456
    TestInput::new("de[, ]")
        .apply::<Expr, Expr>()
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(Span::new(2, 5), error_strings::IndexCallHere)])
        ])
        .expect_result(Expr::IndexCall(IndexCallExpr::new(
            SimpleName::new(1, Span::new(0, 1)),
            Span::new(2, 5), make_exprs![]
        )))
        .finish();
}

#[cfg(test)] #[test]
fn expr_unbox() {

    assert_eq!{ 
        Expr::unbox(Box::new(Expr::SimpleName(SimpleName::new(42, Span::new(30, 31))))), 
        Expr::SimpleName(SimpleName::new(42, Span::new(30, 31)))
    }
}
