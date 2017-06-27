///! fff-lang
///!
///! syntax/expr

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::LitValue;
use lexical::Keyword;

#[macro_use] mod expr_list; // make_exprs
mod lit_expr;
mod ident_expr;
mod tuple_def;
mod array_def;
mod member_access;
mod fn_call;
mod index_call;
mod unary_expr;
mod binary_expr;
mod priority_proxy;

pub use self::lit_expr::LitExpr;
pub use self::ident_expr::IdentExpr;
pub use self::expr_list::ExprList;
pub use self::expr_list::ExprListParseResult;
pub use self::tuple_def::ParenExpr;
pub use self::tuple_def::TupleDef;
pub use self::array_def::ArrayDef;
pub use self::fn_call::FnCallExpr;
pub use self::index_call::IndexCallExpr;
pub use self::member_access::MemberAccessExpr;

pub use self::binary_expr::BinaryExpr;
pub use self::unary_expr::UnaryExpr;
pub use self::priority_proxy::PostfixExpr;
pub use self::priority_proxy::PrimaryExpr;

use super::ParseSession;
use super::ParseResult;
use super::ISyntaxItemParse;
use super::ISyntaxItemFormat;
use super::ISyntaxItemGrammar;

// 12 byte
#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum Expr {
    Lit(LitExpr),
    Ident(IdentExpr),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
    FnCall(FnCallExpr),
    IndexCall(IndexCallExpr),
    MemberAccess(MemberAccessExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
}
impl ISyntaxItemFormat for Expr {
    fn format(&self, indent: u32) -> String {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.format(indent),
            &Expr::Ident(ref ident_expr) => ident_expr.format(indent),
            &Expr::Paren(ref paren_expr) => paren_expr.format(indent),
            &Expr::Tuple(ref tuple_def) => tuple_def.format(indent),
            &Expr::Array(ref array_def) => array_def.format(indent),
            &Expr::FnCall(ref fn_call) => fn_call.format(indent),
            &Expr::IndexCall(ref index_call) => index_call.format(indent),
            &Expr::MemberAccess(ref member_access) => member_access.format(indent),
            &Expr::Unary(ref unary_expr) => unary_expr.format(indent),
            &Expr::Binary(ref binary_expr) => binary_expr.format(indent),
        }
    }
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl Default for Expr {
    fn default() -> Expr { Expr::Lit(LitExpr::new(LitValue::from(0), Span::default())) }
}
impl Expr {

    pub fn get_all_span(&self) -> Span {
        match self {
            &Expr::Lit(ref lit_expr) => lit_expr.span,
            &Expr::Ident(ref ident_expr) => ident_expr.span,
            &Expr::Paren(ref paren_expr) => paren_expr.span,
            &Expr::Tuple(ref tuple_def) => tuple_def.paren_span, 
            &Expr::Array(ref array_def) => array_def.bracket_span,
            &Expr::FnCall(ref fn_call) => fn_call.all_span,
            &Expr::IndexCall(ref index_call) => index_call.all_span,
            &Expr::MemberAccess(ref member_access) => member_access.all_span,
            &Expr::Unary(ref unary_expr) => unary_expr.all_span,
            &Expr::Binary(ref binary_expr) => binary_expr.all_span,
        }
    }

    pub fn unbox(this: Box<Expr>) -> Self {
        let mut this = this;
        let mut temp = Expr::Lit(LitExpr::new(LitValue::from(42), Span::default()));
        ::std::mem::swap(&mut *this, &mut temp);
        temp
    }
}
impl ISyntaxItemGrammar for Expr {
    fn is_first_final(sess: &ParseSession) -> bool { 
        LitExpr::is_first_final(sess)
        || IdentExpr::is_first_final(sess)
        || TupleDef::is_first_final(sess)
        || ArrayDef::is_first_final(sess)
        || sess.tk == &Token::Keyword(Keyword::This)
        || UnaryExpr::is_first_final(sess) 
        // || PostfixExpr::is_first_final(sess) // same as Expr
        // || BinaryExpr::is_first_final(sess)  // same as Expr
    }
}
impl ISyntaxItemParse for Expr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        return BinaryExpr::parse(sess);
    }
}

#[cfg(test)] #[test]
fn expr_parse() {
    use codemap::Span;
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use lexical::Seperator;
    use super::ISyntaxItemWithStr;

    assert_eq!{ Expr::with_test_str("\"abc\""),
        Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(0, 4)))
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
        Expr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 10)))
    }

    assert_eq!{ Expr::with_test_str("(  )"),
        Expr::Lit(LitExpr::new(LitValue::Unit, make_span!(0, 3)))
    }
    
    // Case from fn_def_parse
    assert_eq!{ Expr::with_test_input("println(this)", &mut make_symbols!["println", "this"]),
        Expr::FnCall(FnCallExpr::new(
            Expr::Ident(IdentExpr::new(make_id!(1), make_span!(0, 6))),
            make_span!(7, 12), ExprList::new(vec![
                Expr::Ident(IdentExpr::new(make_id!(2), make_span!(8, 11)))
            ])
        ))
    }

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
            IdentExpr::new(make_id!(1), make_span!(1, 1)),
            IdentExpr::new(make_id!(2), make_span!(4, 4)),
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
            IdentExpr::new(make_id!(1), make_span!(1, 1))
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
            IdentExpr::new(make_id!(1), make_span!(0, 0)),
            make_span!(1, 1),
            IdentExpr::new(make_id!(2), make_span!(2, 2))
        ))
    }

    // function call
    assert_eq!{ Expr::with_test_str("defg()"),
        Expr::FnCall(FnCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 5),
            make_exprs![]
        ))
    }
    assert_eq!{ Expr::with_test_str("deg(a)"),
        Expr::FnCall(FnCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 2)),
            make_span!(3, 5), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(4, 4))
            ]
        ))
    }
    assert_eq!{ Expr::with_test_str("degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 11), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(5, 5)),
                IdentExpr::new(make_id!(3), make_span!(8, 8))
            ]
        ))
    }
    //           0123456789
    assert_eq!{ Expr::with_test_str("abc.defg()"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                IdentExpr::new(make_id!(1), make_span!(0, 2)),
                make_span!(3, 3), 
                IdentExpr::new(make_id!(2), make_span!(4, 7))
            ),
            make_span!(8, 9), 
            make_exprs![]
        ))
    }
    assert_eq!{ Expr::with_test_str("abc.deg(a)"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                IdentExpr::new(make_id!(1), make_span!(0, 2)),
                make_span!(3, 3),
                IdentExpr::new(make_id!(2), make_span!(4, 6))
            ),
            make_span!(7, 9), make_exprs![
                IdentExpr::new(make_id!(3), make_span!(8, 8))
            ]
        ))
    }        //  12345678901234
    assert_eq!{ Expr::with_test_str("1.degg(a, b, )"),
        Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                make_span!(1, 1),
                IdentExpr::new(make_id!(1), make_span!(2, 5))
            ),
            make_span!(6, 13), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(7, 7)),
                IdentExpr::new(make_id!(3), make_span!(10, 10))
            ]
        ))
    }   

    // get index       //  123456
    assert_eq!{ Expr::with_test_str("deg[a]"),
        Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 2)),
            make_span!(3, 5), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(4, 4))
            ]
        ))
    }        //  123456789012
    assert_eq!{ Expr::with_test_str("degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 11), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(5, 5)),
                IdentExpr::new(make_id!(3), make_span!(8, 8))
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
            IdentExpr::new(make_id!(1), make_span!(5, 5))
        ))
    }   //  1234567890123456
    assert_eq!{ Expr::with_test_str("print(233, ).bit"),
        Expr::MemberAccess(MemberAccessExpr::new(
            Expr::FnCall(FnCallExpr::new(
                IdentExpr::new(make_id!(1), make_span!(0, 4)),
                make_span!(5, 11), make_exprs![
                    LitExpr::new(LitValue::from(233), make_span!(6, 8))
                ]
            )),
            make_span!(12, 12),
            IdentExpr::new(make_id!(2), make_span!(13, 15))
        ))
    }            //  12345678901234
    assert_eq!{ Expr::with_test_str("1.degg[a, b, ]"),
        Expr::IndexCall(IndexCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                make_span!(1, 1),
                IdentExpr::new(make_id!(1), make_span!(2, 5))
            ),
            make_span!(6, 13), make_exprs![
                IdentExpr::new(make_id!(2), make_span!(7, 7)),
                IdentExpr::new(make_id!(3), make_span!(10, 10)),
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
}

#[cfg(test)] #[test]
fn expr_errors() {
    use codemap::Span;
    use message::Message;
    use message::MessageCollection;
    use super::error_strings;
    use super::ISyntaxItemWithStr;

    assert_eq!{ Expr::with_test_str_ret_messages("de(, )"), (
        Some(Expr::FnCall(FnCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 1)), 
            make_span!(2, 5), make_exprs![]
        ))),
        make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(2, 5), error_strings::FnCallHere)])
        ]
    )} //                                          0 12345678
    assert_eq!{ Expr::with_test_str_ret_messages("\"\".de(, )"), (
        Some(Expr::FnCall(FnCallExpr::new(
            MemberAccessExpr::new(
                LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(0, 1)),
                make_span!(2, 2), 
                IdentExpr::new(make_id!(2), make_span!(3, 4))
            ),
            make_span!(5, 8), make_exprs![]
        ))),
        make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(5, 8), error_strings::FnCallHere)])
        ]
    )}
    assert_eq!{ Expr::with_test_str_ret_messages("defg[]"), (
        Some(Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 3)),
            make_span!(4, 5), make_exprs![]
        ))),
        make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(4, 5), error_strings::IndexCallHere)])
        ]
    )}    //  123456
    assert_eq!{ Expr::with_test_str_ret_messages("de[, ]"), (
        Some(Expr::IndexCall(IndexCallExpr::new(
            IdentExpr::new(make_id!(1), make_span!(0, 1)),
            make_span!(2, 5), make_exprs![]
        ))),
        make_messages![
            Message::new_by_str(error_strings::EmptyIndexCall, vec![(make_span!(2, 5), error_strings::IndexCallHere)])
        ]
    )}
}

#[cfg(test)] #[test]
fn expr_unbox() {

    assert_eq!{ 
        Expr::unbox(Box::new(Expr::Ident(IdentExpr::new(make_id!(42), make_span!(30, 31))))), 
        Expr::Ident(IdentExpr::new(make_id!(42), make_span!(30, 31)))
    }
}