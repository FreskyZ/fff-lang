///! fff-lang
///!
///! syntax/fn_def
///! fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use message::Message;
use lexical::Token;
use lexical::Keyword;
use lexical::Seperator;

use super::super::Block;
use super::super::TypeUse;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub name_span: Span,
    pub decltype: TypeUse,
}
impl FnParam {
    pub fn new(name: SymbolID, name_span: Span, decltype: TypeUse) -> FnParam { FnParam{ decltype, name, name_span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    pub name: SymbolID,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub params_paren_span: Span,
    pub ret_type: Option<TypeUse>,
    pub body: Block,
    pub all_span: Span,   // fn_span = all_span.slice(0..2)
}
impl ISyntaxFormat for FnDef {
    fn format(&self, f: Formatter) -> String {

        let f = f.indent().header_text_or("fn-def").space().span(self.all_span).endl()
            .indent1().sym(self.name).space().span(self.name_span);
        let f = match self.ret_type { 
            Some(ref ret_type) => f.endl().set_header_text("return-type").apply1(ret_type).unset_header_text().endl(),
            None => f.endl().indent1().lit("no-return-type").endl(),
        };
        let mut f = f.indent1().lit("parenthenes").space().span(self.params_paren_span);
        if self.params.len() == 0 {
            f = f.endl().indent1().lit("no-parameter");
        }
        for &FnParam{ ref decltype, ref name, ref name_span } in &self.params {
            f = f.endl().indent1().lit("param").space().sym(*name).space().span(*name_span).endl().apply2(decltype)
        }
        f.endl().set_header_text("body").apply1(&self.body).finish()
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl FnDef {

    pub fn new(all_span: Span, 
        name: SymbolID, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name, name_span, params, params_paren_span, ret_type, body, all_span }
    }
}
impl ISyntaxGrammar for FnDef {   
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::Fn) }
}
impl ISyntaxParse for FnDef {
    type Output = FnDef;

    fn parse(sess: &mut ParseSession) -> ParseResult<FnDef> {
        #[cfg(feature = "trace_fn_def_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[FnDef: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_fn_def_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }
        
        let fn_span = sess.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = sess.expect_ident()?;
        let mut params_paren_span = sess.expect_sep(Seperator::LeftParenthenes)?;
        trace!("fndef name span: {:?}", fn_name_span);

        let mut params = Vec::new();
        loop {
            if let Some((_comma_span, right_paren_span)) = sess.try_expect_2_sep(Seperator::Comma, Seperator::RightParenthenes) {
                params_paren_span = params_paren_span.merge(&right_paren_span);
                if params.is_empty() {
                    sess.push_message(Message::new_by_str("Single comma in function definition argument list", vec![
                        (fn_name_span, "function definition here"),
                        (params_paren_span, "param list here")
                    ]));
                }
                break;
            } else if let Some(right_paren_span) = sess.try_expect_sep(Seperator::RightParenthenes) {
                params_paren_span = params_paren_span.merge(&right_paren_span);
                break;
            } else if let Some(_comma_span) = sess.try_expect_sep(Seperator::Comma) {
                continue;
            }

            let (param_name, param_span) = sess.expect_ident_or(vec![Keyword::Underscore, Keyword::This])?;
            let _ = sess.expect_sep(Seperator::Colon)?;
            let decltype = TypeUse::parse(sess)?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let maybe_ret_type = if let Some(_right_arrow_span) = sess.try_expect_sep(Seperator::NarrowRightArrow) { Some(TypeUse::parse(sess)?) } else { None };
        let body = Block::parse(sess)?;

        return Ok(FnDef::new(fn_span.merge(&body.all_span), fn_name, fn_name_span, params_paren_span, params, maybe_ret_type, body));
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use codemap::SymbolCollection;
    use super::super::SimpleName;
    use super::super::WithTestInput;
    use super::super::TypeUse;
    use super::super::Statement;
    use super::super::SimpleExprStatement;
    use super::super::Expr;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::TestInput;

    //                                012345678901
    assert_eq!{ FnDef::with_test_str("fn main() {}"),
        FnDef::new(make_span!(0, 11),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 8), vec![], 
            None,
            Block::new(make_span!(10, 11), vec![])
        )
    }

    //              0        1
    //              0123456789012345678
    TestInput::new("fn main(ac: i32) {}")
        .set_syms(make_symbols!["main", "ac", "i32"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(make_span!(0, 18), 
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 15), vec![
                FnParam::new(
                    make_id!(2), make_span!(8, 9),
                    TypeUse::new_simple(make_id!(3), make_span!(12, 14))
                ),
            ],
            None,
            Block::new(make_span!(17, 18), vec![])
        ))
    .finish();

    //              0        1         2         3         4         5         6         7         8
    //              012345678901234567890123456789012345678901234567890123456789012345678901234567890
    TestInput::new(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }")
        //                       1          2       3        4         5       6      7             8       9
        .set_syms(make_symbols!["mainxxx", "argv", "array", "string", "this", "i32", "some_other", "char", "println"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(make_span!(1, 80),
            make_id!(1), make_span!(4, 10),
            make_span!(11, 60), vec![
                FnParam::new(make_id!(2), make_span!(12, 15),
                    TypeUse::new_template(make_id!(3), Span::default(), make_span!(17, 27), vec![
                        TypeUse::new_template(make_id!(3), Span::default(), make_span!(18, 25), vec![
                            TypeUse::new_simple(make_id!(4), make_span!(19, 24))
                        ])
                    ])
                ),
                FnParam::new(make_id!(5), make_span!(32, 35),
                    TypeUse::new_simple(make_id!(6), make_span!(37, 39))
                ),
                FnParam::new(make_id!(7), make_span!(42, 51),
                    TypeUse::new_simple(make_id!(8), make_span!(54, 57))
                )
            ],
            None,
            Block::new(make_span!(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(make_span!(65, 78),
                    FnCallExpr::new(
                        Expr::SimpleName(SimpleName::new(make_id!(9), make_span!(65, 71))),
                        make_span!(72, 77), make_exprs![
                            SimpleName::new(make_id!(5), make_span!(73, 76))
                        ]
                    )
                ))
            ])
        ))
    .finish();

    //                                0        1               
    //                                1234567890123456789
    assert_eq!{ FnDef::with_test_str("fn main() -> i32 {}"),
        FnDef::new(make_span!(0, 18),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 8), vec![],
            Some(TypeUse::new_simple(make_id!(2), make_span!(13, 15))),
            Block::new(make_span!(17, 18), vec![])
        )
    }
    //              0        1         2         3         4         5         6
    //              01234567890123456789012345678901234567890123456789012345678901234567
    TestInput::new("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}")
        //             1       2       3      4       5        6         7
        .set_syms(make_symbols!["ffff", "argc", "i32", "argv", "array", "string", "envv"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(make_span!(0, 67),
            make_id!(1), make_span!(3, 6), 
            make_span!(7, 50), vec![
                FnParam::new(make_id!(2), make_span!(8, 11),
                    TypeUse::new_simple(make_id!(3), make_span!(14, 16))
                ),
                FnParam::new(make_id!(4), make_span!(19, 22),
                    TypeUse::new_template(make_id!(5), Span::default(), make_span!(25, 32), vec![
                        TypeUse::new_simple(make_id!(6), make_span!(26, 31))
                    ])
                ),
                FnParam::new(make_id!(7), make_span!(35, 38),
                    TypeUse::new_template(make_id!(5), Span::default(), make_span!(41, 48), vec![
                        TypeUse::new_simple(make_id!(6), make_span!(42, 47))
                    ])
                )
            ],
            Some(TypeUse::new_template(make_id!(5), Span::default(), make_span!(55, 64), vec![   
                TypeUse::new_template(make_id!(5), Span::default(), make_span!(56, 63), vec![
                    TypeUse::new_simple(make_id!(6), make_span!(57, 62))
                ])
            ])),
            Block::new(make_span!(66, 67), vec![])
        ))
    .finish();
}