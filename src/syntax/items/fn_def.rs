///! fff-lang
///!
///! syntax/fn_def
///! fn-def = 'fn' identifier '(' [ identifier ':' type-use { ',' identifier ':' type-use [ ',' ] } ] ')' [ '->' type-use ] block

use crate::syntax::prelude::*;
use super::super::{Block, TypeUse};

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: IsId,
    pub name_span: Span,
    pub decltype: TypeUse,
}
impl FnParam {
    pub fn new(name: impl Into<IsId>, name_span: Span, decltype: TypeUse) -> FnParam { FnParam{ decltype, name: name.into(), name_span } }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnDef {
    pub name: IsId,
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
            .indent1().isid(self.name).space().span(self.name_span);
        let f = match self.ret_type { 
            Some(ref ret_type) => f.endl().set_header_text("return-type").apply1(ret_type).unset_header_text().endl(),
            None => f.endl().indent1().lit("no-return-type").endl(),
        };
        let mut f = f.indent1().lit("parenthenes").space().span(self.params_paren_span);
        if self.params.len() == 0 {
            f = f.endl().indent1().lit("no-parameter");
        }
        for &FnParam{ ref decltype, ref name, ref name_span } in &self.params {
            f = f.endl().indent1().lit("param").space().isid(*name).space().span(*name_span).endl().apply2(decltype)
        }
        f.endl().set_header_text("body").apply1(&self.body).finish()
    }
}
impl fmt::Debug for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl FnDef {

    pub fn new(all_span: Span, 
        name: impl Into<IsId>, name_span: Span,
        params_paren_span: Span, params: Vec<FnParam>, 
        ret_type: Option<TypeUse>, body: Block) -> FnDef {
        FnDef{ name: name.into(), name_span, params, params_paren_span, ret_type, body, all_span }
    }
}
impl ISyntaxGrammar for FnDef {   
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Keyword(Keyword::Fn)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for FnDef where F: FileSystem {
    type Output = FnDef;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<FnDef> {
        #[cfg(feature = "trace_fn_def_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[FnDef: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_fn_def_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }
        
        let fn_span = sess.expect_keyword(Keyword::Fn)?;
        let (fn_name, fn_name_span) = sess.expect_ident()?;
        let mut params_paren_span = sess.expect_sep(Separator::LeftParen)?;
        trace!("fndef name span: {:?}", fn_name_span);

        let mut params = Vec::new();
        loop {
            if let Some((_comma_span, right_paren_span)) = sess.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                if params.is_empty() {
                    sess.push_message(Message::new_by_str("Single comma in function definition argument list", vec![
                        (fn_name_span, "function definition here"),
                        (params_paren_span, "param list here")
                    ]));
                }
                break;
            } else if let Some(right_paren_span) = sess.try_expect_sep(Separator::RightParen) {
                params_paren_span = params_paren_span + right_paren_span;
                break;
            } else if let Some(_comma_span) = sess.try_expect_sep(Separator::Comma) {
                continue;
            }

            let (param_name, param_span) = sess.expect_ident_or(&[Keyword::Underscore, Keyword::This])?;
            let _ = sess.expect_sep(Separator::Colon)?;
            let decltype = TypeUse::parse(sess)?;
            params.push(FnParam::new(param_name, param_span, decltype));
        }

        let maybe_ret_type = if let Some(_right_arrow_span) = sess.try_expect_sep(Separator::Arrow) { Some(TypeUse::parse(sess)?) } else { None };
        let body = Block::parse(sess)?;

        return Ok(FnDef::new(fn_span + body.all_span, fn_name, fn_name_span, params_paren_span, params, maybe_ret_type, body));
    }
}

#[cfg(test)] #[test]
fn fn_def_parse() {
    use super::super::{make_node, SimpleName, TypeUse, Statement, SimpleExprStatement, Expr, FnCallExpr};

    //                                012345678901
    assert_eq!{ make_node!("fn main() {}" as FnDef),
        FnDef::new(Span::new(0, 11),
            1, Span::new(3, 6), 
            Span::new(7, 8), vec![], 
            None,
            Block::new(Span::new(10, 11), vec![])
        )
    }

    //                      0        1
    //                      0123456789012345678
    assert_eq!{ make_node!("fn main(ac: i32) {}" as FnDef, [Span::new(3, 6), Span::new(8, 9)], ["i32"]),
        FnDef::new(Span::new(0, 18), 
            2, Span::new(3, 6), 
            Span::new(7, 15), vec![
                FnParam::new(
                    3, Span::new(8, 9),
                    TypeUse::new_simple(4, Span::new(12, 14))
                ),
            ],
            None,
            Block::new(Span::new(17, 18), vec![])
        )
    }

    //                      0         1         2         3         4         5         6         7         8
    //                      012345678901234567890123456789012345678901234567890123456789012345678901234567890
    assert_eq!{ make_node!(" fn mainxxx(argv:[[string] ]   ,this:i32, some_other: char, )  { println(this); }" as FnDef, 
        [Span::new(4, 10), Span::new(12, 15), Span::new(19, 24), Span::new(42, 51), Span::new(65, 61)], ["this", "i32"]),
        FnDef::new(Span::new(1, 80),
            1, Span::new(4, 10),
            Span::new(11, 60), vec![
                FnParam::new(2, Span::new(12, 15),
                    TypeUse::new_template(3, Span::new(0, 0), Span::new(17, 27), vec![
                        TypeUse::new_template(3, Span::new(0, 0), Span::new(18, 25), vec![
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
        )
    }

    //                                0        1               
    //                                1234567890123456789
    assert_eq!{ make_node!("fn main() -> i32 {}" as FnDef),
        FnDef::new(Span::new(0, 18),
            1, Span::new(3, 6), 
            Span::new(7, 8), vec![],
            Some(TypeUse::new_simple(2, Span::new(13, 15))),
            Block::new(Span::new(17, 18), vec![])
        )
    }
    //                      0        1         2         3         4         5         6
    //                      01234567890123456789012345678901234567890123456789012345678901234567
    assert_eq!{ make_node!("fn ffff(argc: i32, argv: [string], envv: [string],) -> [[string]] {}" as FnDef, [],  ["ffff", "argc", "i32", "argv", "array", "string", "envv"]),
        FnDef::new(Span::new(0, 67),
            1, Span::new(3, 6), 
            Span::new(7, 50), vec![
                FnParam::new(2, Span::new(8, 11),
                    TypeUse::new_simple(3, Span::new(14, 16))
                ),
                FnParam::new(4, Span::new(19, 22),
                    TypeUse::new_template(5, Span::new(0, 0), Span::new(25, 32), vec![
                        TypeUse::new_simple(6, Span::new(26, 31))
                    ])
                ),
                FnParam::new(7, Span::new(35, 38),
                    TypeUse::new_template(5, Span::new(0, 0), Span::new(41, 48), vec![
                        TypeUse::new_simple(6, Span::new(42, 47))
                    ])
                )
            ],
            Some(TypeUse::new_template(5, Span::new(0, 0), Span::new(55, 64), vec![   
                TypeUse::new_template(5, Span::new(0, 0), Span::new(56, 63), vec![
                    TypeUse::new_simple(6, Span::new(57, 62))
                ])
            ])),
            Block::new(Span::new(66, 67), vec![])
        )
    }
}
