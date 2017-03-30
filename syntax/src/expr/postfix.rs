
// PostfixExpr = 
//     PrimaryExpression [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs TypeUse
//         ]*

use std::fmt;

use codepos::StringPosition;
use message::SyntaxMessage;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::binary::BinaryExpr;
use super::super::TypeUse;

use super::primary::PrimaryExpression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Postfix {
    Subscription(Vec<BinaryExpr>, StringPosition),  // '[', ']' position
    FunctionCall(Vec<BinaryExpr>, StringPosition),  // '(', ')' position
    MemberAccess(String, StringPosition),             // '.xxx' position
    TypeCast(TypeUse, StringPosition),                 // 'as' position
}
impl Postfix {
    pub fn pos(&self) -> StringPosition {
        match *self {
            Postfix::Subscription(ref _exprs, ref pos) => *pos,
            Postfix::FunctionCall(ref _exprs, ref pos) => *pos,
            Postfix::MemberAccess(ref _name, ref pos) => *pos,
            Postfix::TypeCast(ref ty, ref pos) => StringPosition::merge(*pos, ty.pos_all()),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct PostfixExpr {
    pub prim: PrimaryExpression,
    pub postfixs: Vec<Postfix>,
}
impl ISyntaxItemFormat for PostfixExpr {
    fn format(&self, indent: u32) -> String {
        if self.postfixs.len() == 0 {
            format!("{}", self.prim.format(indent))
        } else {
            format!("{}PostfixExpr:\n{}{}", 
                PostfixExpr::indent_str(indent),
                self.prim.format(indent + 1),
                self.postfixs.iter().fold(String::new(), |mut buf, postfix| { buf.push_str("\n"); buf.push_str(&match postfix {
                    &Postfix::Subscription(ref exprs, ref strpos) => 
                        format!("{}Subscription\n{}Bracket at <{:?}>{}", 
                            PostfixExpr::indent_str(indent + 1), PostfixExpr::indent_str(indent + 2),
                            strpos, exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
                    &Postfix::FunctionCall(ref exprs, ref strpos) => 
                        format!("{}FunctionCall\n{}Paren at <{:?}>{}", 
                            PostfixExpr::indent_str(indent + 1), PostfixExpr::indent_str(indent + 2),
                            strpos, exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
                    &Postfix::MemberAccess(ref name, ref strpos) => 
                        format!("{}AccessMember: {} <{:?}>", PostfixExpr::indent_str(indent + 1), name, strpos),
                    &Postfix::TypeCast(ref type_use, ref strpos) => 
                        format!("{}CastAs\n{}As at <{:?}>\n{}", 
                            PostfixExpr::indent_str(indent + 1), PostfixExpr::indent_str(indent + 2), strpos, type_use.format(indent + 2)),
                }); buf }),
            )
        }
    }
}
impl fmt::Debug for PostfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}

impl PostfixExpr {

    pub fn pos_prim(&self) -> StringPosition { self.prim.pos_all() }

    pub fn new_primary(primary_expr: PrimaryExpression) -> PostfixExpr {
        PostfixExpr{ prim: primary_expr, postfixs: Vec::new() }
    }
}

impl ISyntaxItem for PostfixExpr {
    
    fn pos_all(&self) -> StringPosition { 
        StringPosition::merge(
            self.prim.pos_all(), 
            match self.postfixs.iter().last() {
                Some(&Postfix::Subscription(ref _exprs, ref pos)) | Some(&Postfix::FunctionCall(ref _exprs, ref pos)) => *pos,
                Some(&Postfix::MemberAccess(ref _name, ref pos)) => *pos,
                Some(&Postfix::TypeCast(ref ty, ref _pos)) => ty.pos_all(),
                None => self.prim.pos_all(),
            }
        )
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        PrimaryExpression::is_first_final(lexer, index)
    }
    
    #[allow(unused_assignments)]
    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<PostfixExpr>, usize) {
        
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PostfixExpr]"); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let (primary, primary_len) = match PrimaryExpression::parse(lexer, messages, index) {
            (Some(prim), prim_len) => (prim, prim_len),
            (None, prim_len) => return (None, prim_len), // no recover
        };
        trace!("parsing postfix, primary is {}", primary);

        let mut postfixs = Vec::new();
        let mut current_len = primary_len;
        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Dot) {
                match lexer.nth(index + current_len + 1).get_identifier() {
                    Some(ident) => {
                        trace!{ "get one postfix, member access {:?}", ident, }
                        postfixs.push(Postfix::MemberAccess(
                            ident.clone(), 
                            StringPosition::merge(
                                lexer.pos(index + current_len),
                                lexer.pos(index + current_len + 1)
                            )
                        ));
                        current_len += 2;
                        continue 'postfix;
                    }
                    None => {
                        trace!{ "get postfix failed, member access not followed ident" }
                        return push_unexpect!(lexer, messages, "member identifier", index + current_len + 1, current_len + 1);
                    }
                }
            } else if lexer.nth(index + current_len).is_keyword(KeywordKind::As) {
                match TypeUse::parse(lexer, messages, index + current_len + 1) {
                    (Some(ty), ty_len) => {
                        trace!{ "get one postfix, type cast as {:?}", ty, }
                        postfixs.push(Postfix::TypeCast(ty, lexer.pos(index + current_len)));
                        current_len += ty_len + 1;
                        continue 'postfix;
                    }
                    (None, length) => {
                        trace!{ "get postfix failed, type cast not followed typedef" }
                        return (None, current_len + 1 + length);  
                    } 
                }
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftParenthenes) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    trace!{ "get one postfix, none parameter function call" }
                    postfixs.push(Postfix::FunctionCall(
                        Vec::new(), 
                        StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 1))
                    ));
                    current_len += 2;
                    continue 'postfix; // no param function call
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightParenthenes) {
                        let pos1 = StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 2));
                        let pos2 = lexer.pos(index + current_len + 1).start_pos();
                        messages.push(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: pos1, comma_pos: pos2 });
                        postfixs.push(Postfix::FunctionCall(
                            Vec::new(),
                            pos1,
                        ));
                        current_len += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if lexer.nth(index + current_len).is_seperator(SeperatorKind::LeftBracket) {
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                    // first recoverable error here!!!, it is recoverable because later with type infer it can be used
                    let pos = lexer.pos(index + current_len);
                    messages.push(SyntaxMessage::EmptySubscription{ pos: pos });
                    postfixs.push(Postfix::Subscription(
                        Vec::new(),
                        StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 1)),
                    ));
                    current_len += 2;
                    continue 'postfix;
                }
                if lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::Comma) 
                    && lexer.nth(index + current_len + 2).is_seperator(SeperatorKind::RightBracket) {
                        let pos1 = StringPosition::merge(lexer.pos(index + current_len), lexer.pos(index + current_len + 2));
                        let pos2 = lexer.pos(index + current_len + 1).start_pos();
                        messages.push(SyntaxMessage::SingleCommaInSubscription{ sub_pos: pos1, comma_pos: pos2 });
                        postfixs.push(Postfix::Subscription(
                            Vec::new(),
                            pos1,
                        ));
                        current_len += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightBracket;
            } else {
                break; // other final token, break
            }

            // Get the expression list
            trace!{ "parsing postfix, start processing expression list of {}", 
                if expect_end_sep == SeperatorKind::RightParenthenes { "function call".to_owned() } else { "subscription".to_owned() }, }
            current_len += 1; 
            match BinaryExpr::parse(lexer, messages, index + current_len) {
                (None, length) => { 
                    trace!{ "parsing postfix's expression list, expression parse failed" }
                    return (None, current_len + length);
                }
                (Some(expr1), expr1_len) => {
                    let mut exprs_len = expr1_len;
                    let mut exprs = vec![expr1];
                    'expr: loop { 
                        match expect_end_sep {
                            SeperatorKind::RightParenthenes 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightParenthenes) 
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightParenthenes))  => {
                                trace!{ "parsing postfix function call's expression list finished" }
                                let pos_left_paren = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_paren = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::FunctionCall(
                                    exprs,
                                    StringPosition::merge(pos_left_paren, pos_right_paren)
                                ));
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket 
                                if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::RightBracket)
                                    || (lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && lexer.nth(index + current_len + exprs_len + 1).is_seperator(SeperatorKind::RightBracket)) => {
                                trace!{ "parsing postfix subscription's expression list finished" }
                                let pos_left_bracket = lexer.pos(index + current_len - 1);
                                current_len += exprs_len + if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_bracket = lexer.pos(index + current_len - 1);
                                postfixs.push(Postfix::Subscription(
                                    exprs,
                                    StringPosition::merge(pos_left_bracket, pos_right_bracket)
                                ));
                                continue 'postfix;
                            }
                            SeperatorKind::RightParenthenes | SeperatorKind::RightBracket => (),
                            _ => unreachable!()
                        }
                        if lexer.nth(index + current_len + exprs_len).is_seperator(SeperatorKind::Comma) {
                            exprs_len += 1;
                            match BinaryExpr::parse(lexer, messages, index + current_len + exprs_len) {
                                (Some(expr), expr_len) => {
                                    trace!{ "parsing postfix's expression list, get expression: {}", expr, }
                                    exprs_len += expr_len;
                                    exprs.push(expr);
                                }
                                (None, length) => {
                                    trace!{ "parsing postfix's expression list failed, get none expression" }
                                    return (None, current_len + exprs_len + length);
                                }
                            }
                        }
                    }
                }
            }
        }

        trace!{ "parsing postfixs finished, get postfixes: {:?}", postfixs, }
        (Some(PostfixExpr{ prim: primary, postfixs: postfixs }), current_len)
    }
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    
    //                                      0        1         2         3         4         5         6         7         8     
    //                                      1234567890123456789012345678901234567890123456789012345678901234567890123456789
    // let left = BinaryExpr::with_test_str("abc.defg[[1](klm, [123, 456,], )](opq, 456.)() as [i32].rst[uvw, xyz, ABC]");
    // let right = expr_to_postfix!{
    //     PrimaryExpression::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
    //     Postfix::MemberAccess("defg".to_owned(), make_str_pos!(1, 4, 1, 8))
    //     Postfix::Subscription(vec![
    //         expr_to_postfix!(
    //             PrimaryExpression::ArrayDef(
    //                 vec![expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 11, 1, 11))],
    //                 make_str_pos!(1, 10, 1, 12)
    //             ), 
    //             Postfix::FunctionCall(vec![
    //                 expr_ident!("klm", make_str_pos!(1, 14, 1, 16)),
    //                 expr_array_def!([
    //                     expr_num_lit!(NumLitValue::I32(123), make_str_pos!(1, 20, 1, 22)),
    //                     expr_num_lit!(NumLitValue::I32(456), make_str_pos!(1, 25, 1, 27)), ]
    //                     make_str_pos!(1, 19, 1, 29)
    //                 )], 
    //                 make_str_pos!(1, 13, 1, 32)
    //             )
    //         )], 
    //         make_str_pos!(1, 9, 1, 33)
    //     )
    //     Postfix::FunctionCall(vec![
    //         expr_ident!("opq", make_str_pos!(1, 35, 1, 37)),
    //         expr_num_lit!(NumLitValue::F64(456f64), make_str_pos!(1, 40, 1, 43))],
    //         make_str_pos!(1, 34, 1, 44) 
    //     )
    //     Postfix::FunctionCall(
    //         Vec::new(),
    //         make_str_pos!(1, 45, 1, 46)
    //     )
    //     Postfix::TypeCast(
    //         TypeUse::Array(Box::new(
    //             TypeUse::Base("i32".to_owned(), make_str_pos!(1, 52, 1, 54))
    //         ), make_str_pos!(1, 51, 1, 55)),
    //         make_str_pos!(1, 48, 1, 49)
    //     )
    //     Postfix::MemberAccess("rst".to_owned(), make_str_pos!(1, 56, 1, 59))
    //     Postfix::Subscription(vec![
    //         expr_ident!("uvw", make_str_pos!(1, 61, 1, 63)),
    //         expr_ident!("xyz", make_str_pos!(1, 66, 1, 68)),
    //         expr_ident!("ABC", make_str_pos!(1, 71, 1, 73)),],
    //         make_str_pos!(1, 60, 1, 74),
    //     )
    // };

    // assert_eq!(left, right);
    // let left_desc = &format!("{:?}", left);
    // let right_desc = &format!("{:?}", right);
    // for (index, (ch1, ch2)) in left_desc.chars().into_iter().zip(right_desc.chars().into_iter()).enumerate() {
    //     if ch1 != ch2 {
    //         panic!("ch pair diff at {}: {}, {}", index, ch1, ch2);
        
    //     }
    // }

    // perrorln!("{:?}", BinaryExpr::with_test_str("writeln(\"helloworld\")"));
}