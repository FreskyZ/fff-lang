
// PrimaryExpr = 
//     fIdentifier 
//     | fLiteral 
//     | fLeftParen fRighParen                                            // `()`, unit type and unit literal
//     | fLeftParen Expression fRightParen
//     | fLeftParen Expression [fComma Expression]+ fRightParen           // var tuple = (1, "abc", 'd') 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;
use lexical::LitValue;
use lexical::NumLitValue;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::binary::BinaryExpr;

#[derive(Eq, PartialEq)]
enum ActualPrimaryExpr {
    Ident(String),
    Lit(LitValue),
    ParenExpr(BinaryExpr),
    TupleDef(Vec<BinaryExpr>),
    ArrayDef(Vec<BinaryExpr>),
    ArrayDupDef(BinaryExpr, BinaryExpr),   // Now position of `;` is not recorded because I think I don't use it
}
#[derive(Eq, PartialEq)]
pub struct PrimaryExpr(ActualPrimaryExpr, StringPosition);

impl ISyntaxItemFormat for PrimaryExpr {
    fn format(&self, indent: u32) -> String {
        match (&self.0, self.1) {
            (&ActualPrimaryExpr::Ident(ref ident_name), strpos) =>
                format!("{}Ident '{}' <{:?}>", 
                    PrimaryExpr::indent_str(indent), ident_name, strpos),
            (&ActualPrimaryExpr::Lit(ref lit_value), strpos) => 
                format!("{}Literal {} <{:?}>", 
                    PrimaryExpr::indent_str(indent), lit_value, strpos),
            (&ActualPrimaryExpr::ParenExpr(ref inner_expr), strpos) =>
                format!("{}ParenExpr <{:?}>\n{}", 
                    PrimaryExpr::indent_str(indent), strpos, 
                    inner_expr.format(indent + 1)),
            (&ActualPrimaryExpr::ArrayDupDef(ref expr1, ref expr2), strpos) =>
                format!("{}DefineArrayDuply <{:?}>\n{}\n{}",
                    PrimaryExpr::indent_str(indent), strpos, 
                    expr1.format(indent + 1),
                    expr2.format(indent + 2)),
            (&ActualPrimaryExpr::TupleDef(ref exprs), strpos) =>
                format!("{}DefineTuple <{:?}>{}", 
                    PrimaryExpr::indent_str(indent), strpos,
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 1)); buf })),
            (&ActualPrimaryExpr::ArrayDef(ref exprs), strpos) =>
                format!("{}DefineArray <{:?}>{}", 
                    PrimaryExpr::indent_str(indent), strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 1)); buf })),
        }
    }
}
impl fmt::Debug for PrimaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl PrimaryExpr { // New

    pub fn new_ident(ident_name: String, ident_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Ident(ident_name), ident_strpos)
    }
    pub fn new_lit(lit_value: LitValue, lit_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(lit_value), lit_strpos)
    }
    pub fn new_lit_num(num_val: NumLitValue, num_val_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Num(Some(num_val))), num_val_strpos)
    }
    pub fn new_lit_str(value: String, str_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Str(Some(value))), str_strpos)
    }
    pub fn new_lit_char(ch: char, ch_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Char(Some(ch))), ch_strpos)
    }
    pub fn new_lit_bool(value: bool, bool_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Bool(value)), bool_strpos)
    }
    pub fn new_unit(unit_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Unit), unit_strpos)
    }
    pub fn new_paren(paren_strpos: StringPosition, inner: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ParenExpr(inner), paren_strpos)
    }
    pub fn new_array_dup(bracket_strpos: StringPosition, expr1: BinaryExpr, expr2: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDupDef(expr1, expr2), bracket_strpos)
    }
    pub fn new_tuple(paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::TupleDef(exprs), paren_strpos)
    }
    pub fn new_array(bracket_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDef(exprs), bracket_strpos)
    }
}
impl PrimaryExpr { // Get

    pub fn get_strpos(&self) -> StringPosition {
        self.1
    }

    pub fn is_ident(&self) -> bool {
        match self.0 { ActualPrimaryExpr::Ident(_) => true, _ => false }
    }    
    pub fn is_lit(&self) -> bool {
        match self.0 { ActualPrimaryExpr::Lit(_) => true, _ => false }
    }    
    pub fn is_paren(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ParenExpr(_) => true, _ => false }
    }    
    pub fn is_tuple(&self) -> bool {
        match self.0 { ActualPrimaryExpr::TupleDef(_) => true, _ => false }
    }
    pub fn is_array(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ArrayDef(_) => true, _ => false }
    }
    pub fn is_array_dup(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(_, _) => true, _ => false }
    }

    pub fn get_ident_name(&self) -> Option<&String> {
        match self.0 { ActualPrimaryExpr::Ident(ref ident_name) => Some(ident_name), _ => None }
    }
    pub fn get_lit_value(&self) -> Option<&LitValue> {
        match self.0 { ActualPrimaryExpr::Lit(ref lit_value) => Some(lit_value), _ => None }
    }
    pub fn get_paren_inner(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ParenExpr(ref inner) => Some(inner), _ => None }
    }
    pub fn get_tuple_inners(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0 { ActualPrimaryExpr::TupleDef(ref exprs) => Some(exprs), _ => None }
    }
    pub fn get_array_inners(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0 { ActualPrimaryExpr::ArrayDef(ref exprs) => Some(exprs), _ => None }
    }

    pub fn get_array_dup_element(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(ref expr1, _) => Some(expr1), _ => None }
    }
    pub fn get_array_dup_count(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(_, ref expr2) => Some(expr2), _ => None }
    }
}
impl ISyntaxItem for PrimaryExpr {
    
    fn pos_all(&self) -> StringPosition { self.1 }
    
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_ident()
        || tokens.nth(index).is_lit()
        || tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes)
        || tokens.nth(index).is_seperator(SeperatorKind::LeftBracket)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<PrimaryExpr>, usize) {

        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr]"); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("in this method to prove you are running this, current token: {:?}", tokens.nth(index));

        if tokens.nth(index).is_lit() {
            return (Some(PrimaryExpr::new_lit(tokens.nth(index).get_lit_val().unwrap(), tokens.pos(index))), 1);
        }
        match tokens.nth(index).get_identifier() {
            Some(ident) => {
                trace!("yes this is a identifier: {:?}, going to return", ident);
                return (Some(PrimaryExpr::new_ident(ident.clone(), tokens.pos(index))), 1);
            }
            None => (),
        }
        if tokens.nth(index).is_keyword(KeywordKind::This) {
            return (Some(PrimaryExpr::new_ident("this".to_owned(), tokens.pos(index))), 1);
        }

        trace!("parsing primary not literal or identifier");
        if tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            if tokens.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {
                return (Some(PrimaryExpr::new_unit(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2);
            }

            let mut current_len = 1;
            let mut exprs = Vec::new();
            loop {
                match BinaryExpr::parse(tokens, messages, index + current_len) {
                    (None, length) => {
                        trace!("parsing paren expression get expression failed");
                        return (None, current_len + length);
                    }
                    (Some(expr), expr_len) => {
                        current_len += expr_len;
                        exprs.push(expr);
                    }
                }
                if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    current_len += 1;
                    break; // Finished
                } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    current_len += 2; 
                    break; // Finished
                } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    continue;
                } else {
                    return push_unexpect!(tokens, messages, "Right paren", index + current_len, current_len);
                }
            }

            let pos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len - 1));
            if exprs.len() == 0 {
                unreachable!()
            } else if exprs.len() == 1 {
                return (Some(PrimaryExpr::new_paren(pos, exprs.into_iter().last().unwrap())), current_len);
            } else {
                return (Some(PrimaryExpr::new_tuple(pos, exprs)), current_len);
            }
        }

        if tokens.nth(index).is_seperator(SeperatorKind::LeftBracket) {
             // Empty array literal, accept in syntax parse, currently denied in codegen
            if tokens.nth(index + 1).is_seperator(SeperatorKind::RightBracket) {
                return (
                    Some(PrimaryExpr::new_array(
                        StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)),
                        Vec::new(), 
                    )), 
                    2
                );
            }
            match BinaryExpr::parse(tokens, messages, index + 1) {
                (None, length) => {
                    trace!("parsing array (dup) def failed, parse expr1 return none");
                    return (None, length);  // recover by find paired right bracket
                }
                (Some(expr1), expr1_len) => {
                    trace!("parsing array (dup) def get expr1: {} with length {} and next is {:?}", expr1, expr1_len, tokens.nth(index + 1 + expr1_len));
                    if tokens.nth(index + 1 + expr1_len).is_seperator(SeperatorKind::SemiColon) {
                        // let semicolon_pos = tokens.pos(index + 1 + expr1_len); // semicolon is not recorded now
                        match BinaryExpr::parse(tokens, messages, index + 2 + expr1_len) {
                            (None, length) => {
                                trace!("parsing array dup def failed, parse expr2 failed");
                                return (None, expr1_len + 2 + length);
                            } 
                            (Some(expr2), expr2_len) => {
                                if tokens.nth(index + 2 + expr1_len + expr2_len).is_seperator(SeperatorKind::RightBracket) {
                                    trace!("parsing array dup def succeed, expr1: {}, expr2: {}", expr1, expr2);
                                    return (
                                        Some(PrimaryExpr::new_array_dup(
                                            StringPosition::merge(tokens.pos(index), tokens.pos(index + expr1_len + expr2_len + 2)), 
                                            expr1, expr2,
                                        )),
                                        expr1_len + expr2_len + 3
                                    );
                                } else {
                                    trace!("parsing array dup def failed, not followed right bracket");
                                    return push_unexpect!(tokens, messages, "Right bracket after array dup def", index + 3 + expr1_len + expr2_len, expr1_len + expr2_len + 1);
                                }
                            }
                        }
                    }

                    trace!("parsing array def, before loop");
                    let mut current_len = 1 + expr1_len; // 1 for left bracket
                    let mut exprs = vec![expr1];
                    loop {
                        trace!("parsing array def, in loop, current: {:?}", tokens.nth(index + current_len));
                        if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                            trace!("parsing array def succeed, exprs: {:?}", exprs);
                            return (
                                Some(PrimaryExpr::new_array(
                                    StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len)),
                                    exprs, 
                                )), 
                                current_len + 1
                            );
                        } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma)  // Accept [1, 2, 3, abc, ] 
                            && tokens.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                            trace!("parsing array def succeed, exprs: {:?}", exprs);
                            return (
                                Some(PrimaryExpr::new_array(
                                    StringPosition::merge(tokens.pos(index), tokens.pos(index + 1 + current_len)),
                                    exprs, 
                                )), 
                                current_len + 2
                            );
                        } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                            current_len += 1;
                            match BinaryExpr::parse(tokens, messages, index + current_len) {
                                (Some(exprn), exprn_len) => {
                                    trace!("parsing array def, get expression n {}", exprn);
                                    current_len += exprn_len;
                                    exprs.push(exprn);
                                }
                                (None, length) => {
                                    trace!("parsing array def failed, parse expression return none");
                                    return (None, current_len + length);
                                }
                            }
                        } else {
                            return push_unexpect!(tokens, messages, ["comma", "left bracket", ], index + current_len, current_len);
                        }
                    }
                }
            }
        }

        trace!("Failed in prim expr parse, not start with left paren or left bracket");
        return push_unexpect!(tokens, messages, ["identifier", "literal", "array def", ], index, 0);
    }
} 

#[cfg(test)] #[test]
fn primary_expr_parse() {
    use super::super::ISyntaxItemWithStr;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),   
        PrimaryExpr::new_array(
            make_strpos!(1, 1, 1, 3), vec![
                BinaryExpr::new_primary(PrimaryExpr::new_ident("a".to_owned(), make_strpos!(1, 2, 1, 2)))
            ]
        )
    }

    // "(463857, IEfN, atau8M, (fNAE, ((cAeJN4)), nHg))" 
    // "10363" 
    // "264616.15474f32" 
    // "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), eIuArjF), 400, 0o535148505153515453, 0xDB747]]" 
    // "CMDoF" 
    // "false"
    // "[uy6, 4373577, [(q, AJBN0n, MDEgKh5), KG, (NsL, ((), D, false, d), "H="), true, ((vvB3, true, 5))]]" 
    // "(())" 
    // "(fLyl4He)" 
    // "[Fhi;vjIj0Dt]" 
    // "("o5")" 
    // "(nn, ([false;true]), 183455)" 
    // "((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,))rIOKt,)" 
    // "["il", 0o52u32, sO04n]" 
    // "['f';()]" 
    // "[]" 
    // "[8, "@=?GF", 87f32, 1340323.74f64, FKOxAvx5]" 
    // "[[dnr4, lGFd3yL, tJ], ['\', p, ('G'BwiL,), DE], true, aB8aE]" 

    // // Case 0
    // assert_eq!(//12345678901234567890
    //     parse!( "[1, 2, 3f128, 0u64]"),
    //     expr_array_def!{[
    //         expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 2, 1, 2)),
    //         expr_num_lit!(NumLitValue::I32(2), make_str_pos!(1, 5, 1, 5)), 
    //         expr_num_lit!(make_str_pos!(1, 8, 1, 12)),
    //         expr_num_lit!(NumLitValue::U64(0), make_str_pos!(1, 15, 1, 18)),]
    //         make_str_pos!(1, 1, 1, 19)
    //     }
    // );

    // // Case 1          0        1         2         3
    // //                 12345678901234567890123456789012345
    // assert_eq!(parse!("[[(1)], [abc, (3)], [4, this, [6]]]"),
    //     expr_array_def!{[
    //         expr_array_def!{[
    //             expr_paren_expr!(expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 4, 1, 4)), make_str_pos!(1, 3, 1, 5)),]
    //             make_str_pos!(1, 2, 1, 6) 
    //         },
    //         expr_array_def!{[
    //             expr_ident!("abc", make_str_pos!(1, 10, 1, 12)),
    //             expr_paren_expr!(expr_num_lit!(NumLitValue::I32(3), make_str_pos!(1, 16, 1, 16)), make_str_pos!(1, 15, 1, 17)),]
    //             make_str_pos!(1, 9, 1, 18)
    //         },
    //         expr_array_def!{[
    //             expr_num_lit!(NumLitValue::I32(4), make_str_pos!(1, 22, 1, 22)),
    //             expr_ident!("this", make_str_pos!(1, 25, 1, 28)),
    //             expr_array_def!{[
    //                 expr_num_lit!(NumLitValue::I32(6), make_str_pos!(1, 32, 1, 32)),]
    //                 make_str_pos!(1, 31, 1, 33)
    //             },]
    //             make_str_pos!(1, 21, 1, 34)
    //         },]
    //         make_str_pos!(1, 1, 1, 35)
    //     }
    // );

    // // Case 2, empty array literal
    // assert_eq!(
    //     parse!("[]"),
    //     expr_array_def!{
    //         []
    //         make_str_pos!(1, 1, 1, 2)
    //     }
    // );

    // // Case 3    0        1           2          3         4         5           6
    // assert_eq!(//12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    //     parse!( "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
    //     expr_array_def!{[
    //         expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
    //         expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 7, 1, 12)),
    //         expr_str_lit!("456", make_str_pos!(1, 15, 1, 19)),
    //         expr_char_lit!('\u{0065}', make_str_pos!(1, 22, 1, 29)),
    //         expr_bool_lit!(false, make_str_pos!(1, 32, 1, 36)),
    //         expr_to_primary!(PrimaryExpr::Lit(LitValue::Unit, make_str_pos!(1, 39, 1, 40))),
    //         expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 44, 1, 44)), make_str_pos!(1, 43, 1, 45)),
    //         expr_to_primary!(PrimaryExpr::TupleDef(
    //             vec![
    //                 expr_ident!("abc", make_str_pos!(1, 49, 1, 51)),
    //                 expr_str_lit!("hello", make_str_pos!(1, 54, 1, 60)),
    //             ], 
    //             make_str_pos!(1, 48, 1, 63)
    //         )), ]
    //         make_str_pos!(1, 1, 1, 66)
    //     }
    // );        
    
    // // Case 4    0        1            2          3         4
    // assert_eq!(//123456789012 3456 78 9012 345678901234567890123456
    //     parse!( "[abc, 123f, \"456\\u\", '\\u00', false, (a), (  )]"),
    //     expr_array_def!{[
    //         expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
    //         expr_num_lit!(make_str_pos!(1, 7, 1, 10)),
    //         expr_str_lit!(make_str_pos!(1, 13, 1, 19)),
    //         expr_char_lit!( make_str_pos!(1, 22, 1, 27)),
    //         expr_bool_lit!(false, make_str_pos!(1, 30, 1, 34)),
    //         expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 38, 1, 38)), make_str_pos!(1, 37, 1, 39)),
    //         expr_to_primary!(PrimaryExpr::Lit(LitValue::Unit, make_str_pos!(1, 42, 1, 45))),]
    //         make_str_pos!(1, 1, 1, 46)
    //     }
    // );

    // // Case 5    0        1         2
    // assert_eq!(//1234567890123456789012
    //     parse!( "[[123u32, abc]; 4567]"),
    //     expr_array_dup_def!{
    //         expr_array_def!{[
    //             expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 3, 1, 8)),
    //             expr_ident!("abc", make_str_pos!(1, 11, 1, 13)),]
    //             make_str_pos!(1, 2, 1, 14)
    //         },
    //         expr_num_lit!(NumLitValue::I32(4567), make_str_pos!(1, 17, 1, 20)),
    //         [make_str_pos!(1, 1, 1, 21), make_str_pos!(1, 15, 1, 15)]
    //     }
    // );   
}