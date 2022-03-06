///! fff-lang
///!
///! syntax/fn_def
///! fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block

use std::fmt;
use crate::source::Span;
use crate::source::Sym;
use crate::diagnostics::Message;
use crate::lexical::Token;
use crate::lexical::Keyword;
use crate::lexical::Seperator;
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
    pub name: Sym,
    pub name_span: Span,
    pub decltype: TypeUse,
}
impl FnParam {
    pub fn new(name: Sym, name_span: Span, decltype: TypeUse) -> FnParam { FnParam{ decltype, name, name_span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    pub name: Sym,
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
        name: Sym, name_span: Span,
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
                params_paren_span = params_paren_span + right_paren_span;
                if params.is_empty() {
                    sess.push_message(Message::new_by_str("Single comma in function definition argument list", vec![
                        (fn_name_span, "function definition here"),
                        (params_paren_span, "param list here")
                    ]));
                }
                break;
            } else if let Some(right_paren_span) = sess.try_expect_sep(Seperator::RightParenthenes) {
                params_paren_span = params_paren_span + right_paren_span;
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
    use crate::source::SymbolCollection;
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
    assert_eq!{ make_node!("fn main() {}"),
        FnDef::new(Span::new(0, 11),
            1, Span::new(3, 6), 
            Span::new(7, 8), vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //              0        1
    //              0123456789012345678
    TestInput::new("fn main(ac: i32) {}")
        .set_syms(make_symbols!["main", "ac", "i32"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(Span::new(0, 18), 
            1, Span::new(3, 6), 
            Span::new(7, 15), vec![
                FnParam::new(
                    2, Span::new(8, 9),
                    TypeUse::new_simple(3, Span::new(12, 14))
                ),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        ))
    .finish();

    //              0        1         2         3         4         5         6         7         8
    //              012345678901234567890123456789012345678901234567890123456789012345678901234567890
    TestInput::new(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }")
        //                       1          2       3        4         5       6      7             8       9
        .set_syms(make_symbols!["mainxxx", "argv", "array", "string", "this", "i32", "some_other", "char", "println"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(Span::new(1, 80),
            1, Span::new(4, 10),
            Span::new(11, 60), vec![
                FnParam::new(2, Span::new(12, 15),
                    TypeUse::new_template(3, Span::default(), Span::new(17, 27), vec![
                        TypeUse::new_template(3, Span::default(), Span::new(18, 25), vec![
                            TypeUse::new_simple(4, Span::new(19, 24))
                        ])
                    ])
                ),
                FnParam::new(5, Span::new(32, 35),
                    TypeUse::new_simple(6, Span::new(37, 39))
                ),
                FnParam::new(7, Span::new(42, 51),
                    TypeUse::new_simple(8, Span::new(54, 57))
                )
            ],
            None,
            Block::new(Span::new(63, 80), vec![
                Statement::SimpleExpr(SimpleExprStatement::new(Span::new(65, 78),
                    FnCallExpr::new(
                        Expr::SimpleName(SimpleName::new(9, Span::new(65, 71))),
                        Span::new(72, 77), make_exprs![
                            SimpleName::new(5, Span::new(73, 76))
                        ]
                    )
                ))
            ])
        ))
    .finish();

    //                                0        1               
    //                                1234567890123456789
    assert_eq!{ make_node!("fn main() -> i32 {}"),
        FnDef::new(Span::new(0, 18),
            1, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(TypeUse::new_simple(2, Span::new(13, 15))),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //              0        1         2         3         4         5         6
    //              01234567890123456789012345678901234567890123456789012345678901234567
    TestInput::new("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}")
        //             1       2       3      4       5        6         7
        .set_syms(make_symbols!["ffff", "argc", "i32", "argv", "array", "string", "envv"])
        .apply::<FnDef, _>()
        .expect_no_message()
        .expect_result(FnDef::new(Span::new(0, 67),
            1, Span::new(3, 6), 
            Span::new(7, 50), vec![
                FnParam::new(2, Span::new(8, 11),
                    TypeUse::new_simple(3, Span::new(14, 16))
                ),
                FnParam::new(4, Span::new(19, 22),
                    TypeUse::new_template(5, Span::default(), Span::new(25, 32), vec![
                        TypeUse::new_simple(6, Span::new(26, 31))
                    ])
                ),
                FnParam::new(7, Span::new(35, 38),
                    TypeUse::new_template(5, Span::default(), Span::new(41, 48), vec![
                        TypeUse::new_simple(6, Span::new(42, 47))
                    ])
                )
            ],
            Some(TypeUse::new_template(5, Span::default(), Span::new(55, 64), vec![   
                TypeUse::new_template(5, Span::default(), Span::new(56, 63), vec![
                    TypeUse::new_simple(6, Span::new(57, 62))
                ])
            ])),
            Block::new(Span::new(66, 67), vec![])
        ))
    .finish();
}