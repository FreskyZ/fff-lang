///! fff-lang
///!
///! syntax/primary_expr
// PrimaryExpr = 
//     ident_expr   // TODO: change to name 
//     | lit_expr | unit_lit | paren_expr | tuple_def
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

// TODO: split ArrayDef TupleDef out

use std::fmt;

use codemap::Span;

use lexical::Token;
use lexical::KeywordKind;

use super::LitExpr;
use super::IdentExpr;
use super::ParenExpr;
use super::TupleDef;
use super::ArrayDef;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum PrimaryExpr {
    Lit(LitExpr),
    Ident(IdentExpr),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
}
impl ISyntaxItemFormat for PrimaryExpr {
    fn format(&self, indent: u32) -> String {
        match self {
            &PrimaryExpr::Lit(ref lit_expr) => lit_expr.format(indent),
            &PrimaryExpr::Ident(ref ident_expr) => ident_expr.format(indent),
            &PrimaryExpr::Paren(ref paren_expr) => paren_expr.format(indent),
            &PrimaryExpr::Tuple(ref tuple_def) => tuple_def.format(indent),
            &PrimaryExpr::Array(ref array_def) => array_def.format(indent),
        }
    }
}
impl fmt::Debug for PrimaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl PrimaryExpr { // New
    
    pub fn get_all_span(&self) -> Span { 
        match self {
            &PrimaryExpr::Lit(ref lit_expr) => lit_expr.span,
            &PrimaryExpr::Ident(ref ident_expr) => ident_expr.span,
            &PrimaryExpr::Paren(ref paren_expr) => paren_expr.span,
            &PrimaryExpr::Tuple(ref tuple_def) => tuple_def.paren_span, 
            &PrimaryExpr::Array(ref array_def) => array_def.bracket_span,
        }
    }
}
impl ISyntaxItemGrammar for PrimaryExpr {
    fn is_first_final(sess: &ParseSession) -> bool {
        LitExpr::is_first_final(sess)
        || IdentExpr::is_first_final(sess)
        || TupleDef::is_first_final(sess)
        || ArrayDef::is_first_final(sess)
        || sess.tk == &Token::Keyword(KeywordKind::This)
    }
}
impl ISyntaxItemParse for PrimaryExpr {
    type Target = PrimaryExpr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {
        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("start parsing, current token: {:?}", sess.tk);

        if LitExpr::is_first_final(sess) {
            return LitExpr::parse(sess);
        } else if IdentExpr::is_first_final(sess) {
            return IdentExpr::parse(sess);
        } else if TupleDef::is_first_final(sess) {
            return TupleDef::parse(sess);
        } else if ArrayDef::is_first_final(sess) {
            return ArrayDef::parse(sess);
        }

        if let (&Token::Keyword(KeywordKind::This), this_span) = (sess.tk, sess.pos) {
            sess.move_next();
            return Ok(PrimaryExpr::Ident(IdentExpr::new(sess.symbols.intern_str("this"), this_span)));
        } else {
            return sess.push_unexpect("primary expr");
        }
    }
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn primary_expr_parse() {
    use super::super::ISyntaxItemWithStr;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),   
        PrimaryExpr::new_array(
            make_span!(0, 2), vec![
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(1, 1)))
            ]
        )
    }

    //                                        0        1         2         3         4
    //                                        01234567890123456789012345678901234567890123456
    assert_eq!{ PrimaryExpr::with_test_input("(463857, IEfN, atau8M, (fNAE, ((cAeJN4)), nHg))", 
        //                  1       2         3       4         5
        &mut make_symbols!["IEfN", "atau8M", "fNAE", "cAeJN4", "nHG"]), 
        PrimaryExpr::new_tuple(make_span!(0, 46), vec![
            Expr::new_primary(PrimaryExpr::new_lit(LitValue::from(463857), make_span!(1, 6))),
            Expr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(9, 12))),
            Expr::new_primary(PrimaryExpr::new_ident(make_id!(2), make_span!(15, 20))),
            Expr::new_primary(PrimaryExpr::new_tuple(make_span!(23, 45), vec![
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(3), make_span!(24, 27))),
                Expr::new_primary(PrimaryExpr::new_paren(make_span!(30, 39), 
                    Expr::new_primary(PrimaryExpr::new_paren(make_span!(31, 38), 
                        Expr::new_primary(PrimaryExpr::new_ident(make_id!(4), make_span!(32, 37)))
                    ))
                )),
                Expr::new_primary(PrimaryExpr::new_ident(make_id!(5), make_span!(42, 44)))
            ]))
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("10363"), PrimaryExpr::new_lit(LitValue::from(10363), make_span!(0, 4)) }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        PrimaryExpr::new_array(make_span!(0, 134), vec![
            Expr::new_paren(make_span!(1, 6), 
                Expr::new_lit(LitValue::from(0x7E), make_span!(2, 5))
            ),
            Expr::new_ident("FFGqfJe".to_owned(), make_span!(9, 15)),
            Expr::new_ident("I4".to_owned(), make_span!(18, 19)), 
            Expr::new_array(make_span!(22, 133), vec![
                Expr::new_tuple(make_span!(23, 105), vec![
                    Expr::new_ident("m7A".to_owned(), make_span!(24, 26)),
                    Expr::new_tuple(make_span!(29, 90), vec![
                        Expr::new_lit(LitValue::from(41), make_span!(30, 31)),
                        Expr::new_paren(make_span!(34, 68), 
                            Expr::new_array(make_span!(35, 67), vec![
                                Expr::new_tuple(make_span!(36, 60), vec![
                                    Expr::new_ident("jL".to_owned(), make_span!(37, 38)), 
                                    Expr::new_ident("rAn".to_owned(), make_span!(41, 43)),
                                    Expr::new_ident("K0FgLc7h".to_owned(), make_span!(46, 53)),
                                    Expr::new_lit(LitValue::from(true), make_span!(56, 59))
                                ]),
                                Expr::new_ident("C".to_owned(), make_span!(63, 63)),
                                Expr::new_ident("w".to_owned(), make_span!(66, 66)),
                            ])
                        ),
                        Expr::new_tuple(make_span!(71, 89), vec![
                            Expr::new_ident("J3cEFDG".to_owned(), make_span!(72, 78)),
                            Expr::new_ident("d".to_owned(), make_span!(81, 81)),
                            Expr::new_paren(make_span!(84, 88), 
                                Expr::new_ident("j8h".to_owned(), make_span!(85, 87))
                            )
                        ])
                    ]),
                    Expr::new_unit(make_span!(93, 94)),
                    Expr::new_ident("eIuArjF".to_owned(), make_span!(98, 104))
                ]),
                Expr::new_lit(LitValue::from(400), make_span!(108, 110)),
                Expr::new_lit(LitValue::from(0o535147505), make_span!(113, 123)),
                Expr::new_lit(LitValue::from(0xDB747), make_span!(126, 132))
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), PrimaryExpr::new_ident(make_id!(1), make_span!(0, 4)) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), PrimaryExpr::new_lit(LitValue::from(false), make_span!(0, 4)) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        PrimaryExpr::new_array(make_span!(0, 101), vec![
            Expr::new_ident("uy6".to_owned(), make_span!(1, 3)),
            Expr::new_lit(LitValue::from(4373577), make_span!(6, 12)),
            Expr::new_array(make_span!(15, 100), vec![
                Expr::new_tuple(make_span!(16, 36), vec![
                    Expr::new_ident("q".to_owned(), make_span!(17, 17)),
                    Expr::new_ident("AJBN0n".to_owned(), make_span!(20, 25)), 
                    Expr::new_ident("MDEgKh5".to_owned(), make_span!(28, 34))
                ]), 
                Expr::new_ident("KG".to_owned(), make_span!(39, 40)),
                Expr::new_tuple(make_span!(43, 74), vec![
                    Expr::new_ident("NsL".to_owned(), make_span!(44, 46)),
                    Expr::new_tuple(make_span!(49, 67), vec![
                        Expr::new_unit(make_span!(50, 51)),
                        Expr::new_ident("D".to_owned(), make_span!(54, 54)),
                        Expr::new_lit(LitValue::from(false), make_span!(57, 61)),
                        Expr::new_ident("d".to_owned(), make_span!(64, 64)),
                    ]),
                    Expr::new_lit(LitValue::from("H="), make_span!(70, 73))
                ]),
                Expr::new_lit(LitValue::from(true), make_span!(77, 80)),
                Expr::new_paren(make_span!(83, 99), 
                    Expr::new_tuple(make_span!(84, 98), vec![
                        Expr::new_ident("vvB3".to_owned(), make_span!(85, 88)),
                        Expr::new_lit(LitValue::from(true), make_span!(91, 94)),
                        Expr::new_lit(LitValue::from(5), make_span!(97, 97))
                    ])
                )
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("(() )"), PrimaryExpr::new_paren(make_span!(0, 4), Expr::new_unit(make_span!(1, 2))) }
    assert_eq!{ PrimaryExpr::with_test_str("((),)"), PrimaryExpr::new_tuple(make_span!(0, 4), vec![Expr::new_unit(make_span!(1, 2))]) }

    //                                      1234567890123
    assert_eq!{ PrimaryExpr::with_test_str("[Fhi;vjIj0Dt]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 12), 
            Expr::new_ident("Fhi".to_owned(), make_span!(1, 3)),
            Expr::new_ident("vjIj0Dt".to_owned(), make_span!(5, 11))
        )
    }

    assert_eq!{ PrimaryExpr::with_test_str("(\"o5\")"), 
        PrimaryExpr::new_paren(make_span!(0, 5), 
            Expr::new_lit(LitValue::from("o5"), make_span!(1, 4))
        )
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false;true]), 183455)"),
        PrimaryExpr::new_tuple(make_span!(0, 27), vec![
            Expr::new_ident("nn".to_owned(), make_span!(1, 2)),
            Expr::new_paren(make_span!(5, 18), 
                Expr::new_array_dup(make_span!(6, 17), 
                    Expr::new_lit(LitValue::from(false), make_span!(7, 11)),
                    Expr::new_lit(LitValue::from(true), make_span!(13, 16))
                )
            ),
            Expr::new_lit(LitValue::from(183455), make_span!(21, 26))
        ])
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        PrimaryExpr::new_tuple(make_span!(0, 77), vec![
            Expr::new_tuple(make_span!(1, 68), vec![
                Expr::new_lit(LitValue::from(true), make_span!(2, 5)),
                Expr::new_tuple(make_span!(8, 49), vec![
                    Expr::new_ident("mO".to_owned(), make_span!(9, 10)), 
                    Expr::new_array_dup(make_span!(13, 21), 
                        Expr::new_paren(make_span!(14, 18), 
                            Expr::new_ident("q5k".to_owned(), make_span!(15, 17))
                        ),
                        Expr::new_ident("a".to_owned(), make_span!(20, 20))
                    ),
                    Expr::new_paren(make_span!(24, 33), 
                        Expr::new_paren(make_span!(25, 32), 
                            Expr::new_paren(make_span!(26, 31), 
                                Expr::new_ident("KttG".to_owned(), make_span!(27, 30))
                            )
                        )
                    ),
                    Expr::new_tuple(make_span!(36, 48), vec![
                        Expr::new_ident("K5DJ".to_owned(), make_span!(37, 40)), 
                        Expr::new_ident("r".to_owned(), make_span!(43, 43)),
                        Expr::new_unit(make_span!(46, 47))
                    ]),
                ]),
                Expr::new_tuple(make_span!(52, 67), vec![
                    Expr::new_ident("McsaEdfdfalse".to_owned(), make_span!(53, 65))
                ])
            ]),
            Expr::new_ident("rIOKt".to_owned(), make_span!(71, 75))
        ])
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[\"il\", 0o52u32, sO04n]"),
        PrimaryExpr::new_array(make_span!(0, 21), vec![
            Expr::new_lit(LitValue::from("il"), make_span!(1, 4)),
            Expr::new_lit(LitValue::from(0o52u32), make_span!(7, 13)), 
            Expr::new_ident("sO04n".to_owned(), make_span!(16, 20))
        ])
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f';()]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 7), 
            Expr::new_lit(LitValue::from('f'), make_span!(1, 3)),
            Expr::new_unit(make_span!(5, 6))
        )
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), PrimaryExpr::new_array(make_span!(0, 1), vec![]) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        PrimaryExpr::new_array(make_span!(0, 43), vec![
            Expr::new_lit(LitValue::from(8), make_span!(1, 1)),
            Expr::new_lit(LitValue::from("@=?GF"), make_span!(4, 10)), 
            Expr::new_lit(LitValue::from(87f32), make_span!(13, 17)),
            Expr::new_lit(LitValue::from(1340323.74f64), make_span!(20, 32)),
            Expr::new_ident("FKOxAvx5".to_owned(), make_span!(35, 42))
        ])
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ PrimaryExpr::with_test_str(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        PrimaryExpr::new_array(make_span!(2, 62), vec![
            Expr::new_array(make_span!(3, 21), vec![
                Expr::new_ident("dnr4".to_owned(), make_span!(4, 7)),
                Expr::new_ident("lGFd3yL".to_owned(), make_span!(10, 16)),
                Expr::new_ident("tJ".to_owned(), make_span!(19, 20))
            ]),
            Expr::new_array(make_span!(24, 48), vec![
                Expr::new_lit(LitValue::from('\\'), make_span!(25, 28)), 
                Expr::new_ident("p".to_owned(), make_span!(31, 31)),
                Expr::new_tuple(make_span!(34, 43), vec![
                    Expr::new_ident("xGaBwiL".to_owned(), make_span!(35, 41))
                ]),
                Expr::new_ident("DE".to_owned(), make_span!(46, 47))
            ]),
            Expr::new_lit(LitValue::from(true), make_span!(51, 54)),
            Expr::new_ident("aB8aE".to_owned(), make_span!(57, 61))
        ])
    } 

    // Previous manual tests
    //                                      0        1           2          3         4         5           6
    //                                      12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ PrimaryExpr::with_test_str("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
        PrimaryExpr::new_array(make_span!(0, 65), vec![
            Expr::new_ident("abc".to_owned(), make_span!(1, 3)),
            Expr::new_lit(LitValue::from(123u32), make_span!(6, 11)),
            Expr::new_lit(LitValue::from("456"), make_span!(14, 18)),
            Expr::new_lit(LitValue::from('\u{0065}'), make_span!(21, 28)),
            Expr::new_lit(LitValue::from(false), make_span!(31, 35)),
            Expr::new_unit(make_span!(38, 39)),
            Expr::new_paren(make_span!(42, 44), 
                Expr::new_ident("a".to_owned(), make_span!(43, 43))
            ),
            Expr::new_tuple(make_span!(47, 62), vec![
                Expr::new_ident("abc".to_owned(), make_span!(48, 50)),
                Expr::new_lit(LitValue::from("hello"), make_span!(53, 59)),
            ])
        ])
    }       

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), PrimaryExpr::new_unit(make_span!(0, 30)) }

    //                                      0        1         2
    //                                      123456789012345678901
    assert_eq!{ PrimaryExpr::with_test_str("[[123u32, abc]; 4567]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 20), 
            Expr::new_array(make_span!(1, 13), vec![
                Expr::new_lit(LitValue::from(123u32), make_span!(2, 7)), 
                Expr::new_ident("abc".to_owned(), make_span!(10, 12))
            ]),
            Expr::new_lit(LitValue::from(4567), make_span!(16, 19))
        )
    }
}

#[cfg(test)] #[test] #[ignore]
fn primary_expr_errors() {
    use super::super::ISyntaxItemWithStr;
    use message::Message;
    use message::MessageCollection;

    const UNEXPECTED_SYMBOL: &'static str = "Unexpected symbol";

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(1, ]"), (
        None, 
        3,
        make_messages![
            Message::with_help_by_str(UNEXPECTED_SYMBOL, 
                vec![(make_span!(4, 4), "Meet Seperator `]`(RightBracket) <0>1:5-1:5")],
                vec![&("Expect ".to_owned() + "error_strings::ExpectExpression")]
            )
        ], 
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(,)"), (
        None, 
        1,
        make_messages![],
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("[1, )"), (
        None,
        1,
        make_messages![],
    )}
}