///! fff-lang
///!
///! syntax/postfix_expr
// PostfixExpr = 
//     PrimaryExpr [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs TypeUse
//         ]*

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use message::Message;
use lexical::Token;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::BinaryExpr;

use super::primary::PrimaryExpr;

#[cfg_attr(test, derive(Eq, PartialEq))]
enum ActualPostfix {
    MemberAccess(SymbolID, [Span; 2]),          // dot's position, xxx's position
    Subscription(Vec<BinaryExpr>, Span),      // []'s position
    FunctionCall(Vec<BinaryExpr>, Span),      // ()'s position
    MemberFunctionCall(SymbolID, Vec<BinaryExpr>, [Span; 3]), // dot's position, xxx's position ()'s position
}
#[cfg_attr(test, derive(Eq, PartialEq))]
enum PostfixExprImpl {
    Primary(PrimaryExpr),
    Postfix(PostfixExpr, ActualPostfix, Span), // all_strpos
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct PostfixExpr(Box<PostfixExprImpl>);

impl ISyntaxItemFormat for PostfixExpr {
    fn format(&self, indent: u32) -> String {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(ref primary_expr) => primary_expr.format(indent),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::Subscription(ref exprs, ref bracket_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}Subscription {}<{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), if exprs.is_empty() { "(empty) " } else { "" }, bracket_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::FunctionCall(ref exprs, ref paren_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}FunctionCall {}<{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), if exprs.is_empty() { "(empty) " } else { "" }, paren_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, 
                ActualPostfix::MemberFunctionCall(ref name, ref exprs, ref strposs), ref all_strpos) =>
                format!("{}PostfixExpr <{:?}>\n{}\n{}MemberFunctionCall {}<{:?}>\n{}'.' <{:?}>\n{}Ident {:?} <{:?}>\n{}paren <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), if exprs.is_empty() { "(empty) " } else { "" }, strposs[0].merge(&strposs[2]),
                    PostfixExpr::indent_str(indent + 2), strposs[0],
                    PostfixExpr::indent_str(indent + 2), name, strposs[1],
                    PostfixExpr::indent_str(indent + 2), strposs[2], 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })), 
            &PostfixExprImpl::Postfix(ref postfix_expr, 
                ActualPostfix::MemberAccess(ref name, ref strposs), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}MemberAccess <{:?}>\n{}'.' <{:?}>\n{}Ident {:?} <{:?}>",
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1), 
                    PostfixExpr::indent_str(indent + 1), strposs[0].merge(&strposs[1]),
                    PostfixExpr::indent_str(indent + 2), strposs[0],
                    PostfixExpr::indent_str(indent + 2), name, strposs[1]),
        }
    }
}
impl fmt::Debug for PostfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl PostfixExpr { // New

    pub fn new_primary(primary_expr: PrimaryExpr) -> PostfixExpr {
        PostfixExpr(Box::new(PostfixExprImpl::Primary(primary_expr)))
    }

    pub fn new_subscription(postfix_expr: PostfixExpr, bracket_strpos: Span, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = postfix_expr.get_all_span().merge(&bracket_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::Subscription(exprs, bracket_strpos), all_strpos)))
    }
    pub fn new_function_call(postfix_expr: PostfixExpr, paren_strpos: Span, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = postfix_expr.get_all_span().merge(&paren_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::FunctionCall(exprs, paren_strpos), all_strpos)))
    }
    pub fn new_member_function_call(postfix_expr: PostfixExpr, 
        dot_strpos: Span, name: SymbolID, ident_strpos: Span, paren_strpos: Span, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = postfix_expr.get_all_span().merge(&paren_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, 
            ActualPostfix::MemberFunctionCall(name, exprs, [dot_strpos, ident_strpos, paren_strpos]), all_strpos)))
    }
    pub fn new_member_access(postfix_expr: PostfixExpr, dot_strpos: Span, name: SymbolID, name_strpos: Span) -> PostfixExpr {
        let all_strpos = postfix_expr.get_all_span().merge(&name_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, 
            ActualPostfix::MemberAccess(name, [dot_strpos, name_strpos]), all_strpos)))
    }

    // parse helper, don't want to handle this there
    fn new_function_call_auto_merge_prev_member_access(postfix_expr: PostfixExpr, paren_strpos: Span, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        use std::mem; // strange method to move boxed out from immutable ref self
        unsafe { 
            match *Box::into_raw(postfix_expr.0) {
                PostfixExprImpl::Postfix(ref mut prev_prev_postfix_expr, ActualPostfix::MemberAccess(ref name, ref dot_and_name_strpos), _) => {
                    let mut owned_prev_prev_postfix_expr = mem::uninitialized();
                    mem::swap(prev_prev_postfix_expr, &mut owned_prev_prev_postfix_expr);
                    return PostfixExpr::new_member_function_call(owned_prev_prev_postfix_expr, dot_and_name_strpos[0], *name, dot_and_name_strpos[1], paren_strpos, exprs);
                }
                ref mut other_impl => return PostfixExpr::new_function_call(PostfixExpr(Box::from_raw(other_impl as *mut PostfixExprImpl)), paren_strpos, exprs),
            }
        }
    }
}
impl PostfixExpr { // Get

    // TODO: remove it
    pub fn get_all_span(&self) -> Span {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(ref primary_expr) => primary_expr.get_all_span(),
            &PostfixExprImpl::Postfix(ref _1, ref _2, ref all_strpos) => *all_strpos,
        }
    }
}

impl ISyntaxItemGrammar for PostfixExpr {
    fn is_first_final(sess: &ParseSession) -> bool { PrimaryExpr::is_first_final(sess) }
}
impl ISyntaxItemParse for PostfixExpr {
    type Target = PostfixExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PostfixExpr> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = PostfixExpr::new_primary(PrimaryExpr::parse(sess)?);
        trace!("parsed primary, current is {:?}", current_retval);

        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let (starting_strpos, expect_end_sep) = match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                (&Token::Sep(SeperatorKind::Dot), ref dot_strpos, &Token::Ident(ref ident), ref ident_strpos) => {
                    sess.move_next2();
                    current_retval = PostfixExpr::new_member_access(current_retval, *dot_strpos, *ident, *ident_strpos);
                    trace!("after member access finished");
                    continue 'postfix;
                }
                (&Token::Sep(SeperatorKind::Dot), ref _dot_strpos, _, _) => {
                    trace!("get postfix failed, member access not followed ident");
                    return sess.push_unexpect("identifier");
                }
                (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_strpos, &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) => {
                    trace!("get one postfix, none parameter function call");
                    sess.move_next2();
                    current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(
                        current_retval, left_paren_strpos.merge(right_paren_strpos), Vec::new()
                    );
                    continue 'postfix;
                }
                (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_strpos, &Token::Sep(SeperatorKind::Comma), _) => {
                    sess.move_next2();
                    if sess.tk == &Token::Sep(SeperatorKind::RightParenthenes) {
                        let paren_strpos = left_paren_strpos.merge(&sess.pos);
                        sess.push_message(Message::new_by_str("Single comma in function call", vec![(paren_strpos, "function call here")]));
                        current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(current_retval, paren_strpos, Vec::new());
                        sess.move_next();
                        continue 'postfix;
                    }
                    (*left_paren_strpos, SeperatorKind::RightParenthenes)
                }
                (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_strpos, _, _) => {
                    (*left_paren_strpos, SeperatorKind::RightParenthenes)
                }
                (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_strpos, &Token::Sep(SeperatorKind::RightBracket), ref right_bracket_strpos) => {
                    sess.move_next2();
                    let bracket_strpos = left_bracket_strpos.merge(right_bracket_strpos);
                    sess.push_message(Message::new_by_str("Empty subscription", vec![(bracket_strpos, "subscription here")]));
                    current_retval = PostfixExpr::new_subscription(current_retval, bracket_strpos, Vec::new());
                    continue 'postfix;
                }
                (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_strpos, &Token::Sep(SeperatorKind::Comma), _) => {
                    sess.move_next2();
                    if sess.tk == &Token::Sep(SeperatorKind::RightBracket) {
                        let bracket_strpos = left_bracket_strpos.merge(&sess.pos);
                        sess.push_message(Message::new_by_str("Empty subscription", vec![(bracket_strpos, "subscription here")]));
                        current_retval = PostfixExpr::new_subscription(current_retval, bracket_strpos, Vec::new());
                        sess.move_next();
                        continue 'postfix;
                    }
                    (*left_bracket_strpos, SeperatorKind::RightBracket)
                }
                (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_strpos, _, _) => {
                    (*left_bracket_strpos, SeperatorKind::RightBracket)
                }
                _ => break,
            };

            // Get the expression list
            sess.move_next();
            let expr1 = BinaryExpr::parse(sess)?;
            let mut exprs = vec![expr1];
            'expr: loop { 
                match (&expect_end_sep, sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                    (&SeperatorKind::RightParenthenes, 
                        &Token::Sep(SeperatorKind::Comma), _, 
                        &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) => {
                        sess.move_next2();
                        current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(
                            current_retval, starting_strpos.merge(right_paren_strpos), exprs
                        );
                        continue 'postfix;
                    }
                    (&SeperatorKind::RightParenthenes, 
                        &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos, _, _) => {
                        sess.move_next();
                        current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(
                            current_retval, starting_strpos.merge(right_paren_strpos), exprs
                        );
                        continue 'postfix;
                    }
                    (&SeperatorKind::RightBracket, 
                        &Token::Sep(SeperatorKind::Comma), _, 
                        &Token::Sep(SeperatorKind::RightBracket), ref right_bracket_strpos) => {
                        sess.move_next2();
                        current_retval = PostfixExpr::new_subscription(current_retval, starting_strpos.merge(right_bracket_strpos), exprs);
                        continue 'postfix;
                    }
                    (&SeperatorKind::RightBracket, 
                        &Token::Sep(SeperatorKind::RightBracket), ref right_bracket_strpos, _, _) => {
                        sess.move_next();
                        current_retval = PostfixExpr::new_subscription(current_retval, starting_strpos.merge(right_bracket_strpos), exprs);
                        continue 'postfix;
                    }
                    (_, &Token::Sep(SeperatorKind::Comma), _, _, _) => {
                        sess.move_next();
                        exprs.push(BinaryExpr::parse(sess)?);
                    }
                    _ => (),
                }
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_retval);
        return Ok(current_retval);
    }
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn postfix_expr_format() {
    use super::super::ISyntaxItemWithStr;

    macro_rules! test_case {
        ($left: expr, $right: expr) => {
            if $left != $right {
                let left_owned = $left.to_owned();
                let left_lines = left_owned.lines();
                let right_lines = $right.lines();
                for (index, (left_line, right_line)) in left_lines.zip(right_lines).enumerate() {
                    if left_line != right_line {
                        panic!("assertion failed at index {}\nleft: {}\nright: {}", index, $left, $right);
                    }
                }
                panic!("assertion failed, but cannot detected by compare each line\nleft: {}\nright: {}", $left, $right);
            }
        }
    }

    // Attention that this source code line's LF is also the string literal (test oracle)'s LF
    //                                     0         1         2         3         4         5        
    //                                     0123456789012345678901234567890123456789012345678901234567
    test_case!(PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]").format(0), r##"PostfixExpr <<0>0-57>
  PostfixExpr <<0>0-47>
    PostfixExpr <<0>0-38>
      PostfixExpr <<0>0-36>
        PostfixExpr <<0>0-32>
          PostfixExpr <<0>0-29>
            PostfixExpr <<0>0-27>
              PostfixExpr <<0>0-23>
                PostfixExpr <<0>0-11>
                  Ident #1 <<0>0-0>
                  MemberFunctionCall <<0>1-11>
                    '.' <<0>1-1>
                    Ident #2 <<0>2-2>
                    paren <<0>3-11>
                    Ident #3 <<0>4-4>
                    Ident #4 <<0>7-7>
                    Ident #5 <<0>10-10>
                MemberFunctionCall <<0>12-23>
                  '.' <<0>12-12>
                  Ident '#6 <<0>13-13>
                  paren <<0>14-23>
                  Ident #7 <<0>15-15>
                  Ident #8 <<0>18-18>
                  Ident #9 <<0>21-21>
              FunctionCall <<0>24-27>
                Ident #10 <<0>25-25>
            MemberAccess <<0>28-29>
              '.' <<0>28-28>
              Ident #11 <<0>29-29>
          Subscription <<0>30-32>
            Ident #12 <<0>31-31>
        MemberFunctionCall (empty) <<0>33-36>
          '.' <<0>33-33>
          Ident #13 <<0>34-34>
          paren <<0>35-36>
      MemberAccess <<0>37-38>
        '.' <<0>37-37>
        Ident #14 <<0>38-38>
    Subscription <<0>39-47>
      Ident #15 <<0>40-40>
      Ident #16 <<0>43-43>
      Ident #17 <<0>46-46>
  Subscription <<0>48-57>
    Ident #18 <<0>49-49>
    Ident #19 <<0>52-52>
    Ident #20 <<0>55-55>"##
    );
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use super::super::ISyntaxItemWithStr;
    use message::MessageCollection;
    use super::IdentExpr;

    macro_rules! ident {
        ($name: expr, $strpos: expr) => (BinaryExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new($name, $strpos))));
        (post, $name: expr, $strpos: expr) => (PostfixExpr::new_primary(PrimaryExpr::Ident(IdentExpr::new($name, $strpos))))
    }

    //                                      0        1         2         3         4         5     
    // plain                                1234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]"),
        PostfixExpr::new_subscription(
            PostfixExpr::new_subscription(
                PostfixExpr::new_member_access(
                    PostfixExpr::new_member_function_call(
                        PostfixExpr::new_subscription(
                            PostfixExpr::new_member_access(
                                PostfixExpr::new_function_call(
                                    PostfixExpr::new_member_function_call(
                                        PostfixExpr::new_member_function_call(
                                            ident!(post, make_id!(1), make_span!(0, 0)),
                                            make_span!(1, 1),
                                            make_id!(2), make_span!(2, 2),
                                            make_span!(3, 11), vec![
                                                ident!(make_id!(3), make_span!(4, 4)),
                                                ident!(make_id!(4), make_span!(7, 7)),
                                                ident!(make_id!(5), make_span!(10, 10)),
                                            ]
                                        ),
                                        make_span!(12, 12),
                                        make_id!(6), make_span!(13, 13),
                                        make_span!(14, 23), vec![
                                            ident!(make_id!(7), make_span!(15, 15)),
                                            ident!(make_id!(8), make_span!(18, 18)),
                                            ident!(make_id!(9), make_span!(21, 21)),
                                        ]
                                    ),
                                    make_span!(24, 27), vec![
                                        ident!(make_id!(10), make_span!(25, 25))
                                    ]
                                ),
                                make_span!(28, 28),
                                make_id!(11), make_span!(29, 29)
                            ),
                            make_span!(30, 32), vec![
                                ident!(make_id!(12), make_span!(31, 31))
                            ]
                        ),
                        make_span!(33, 33),
                        make_id!(13), make_span!(34, 34),
                        make_span!(35, 36),
                        vec![]
                    ),
                    make_span!(37, 37),
                    make_id!(14), make_span!(38, 38)
                ),
                make_span!(39, 47), vec![
                    ident!(make_id!(15), make_span!(40, 40)),
                    ident!(make_id!(16), make_span!(43, 43)),
                    ident!(make_id!(17), make_span!(46, 46))
                ]
            ),
            make_span!(48, 57), vec![
                ident!(make_id!(18), make_span!(49, 49)),
                ident!(make_id!(19), make_span!(52, 52)),
                ident!(make_id!(20), make_span!(55, 55))
            ]
        )
    }
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, make_id!(1), make_span!(0, 0)), 
            make_span!(1, 2), vec![]
        )), 
        3,
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_span!(1, 2), "subscription here")])
        ],
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[, ]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, make_id!(1), make_span!(0, 0)), 
            make_span!(1, 4), vec![]
        )), 
        4,
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_span!(1, 4), "subscription here")])
        ],
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a(, )"), (
        Some(PostfixExpr::new_function_call(
            ident!(post, make_id!(1), make_span!(0, 0)),
            make_span!(1, 4), vec![]
        )), 
        4,
        make_messages![
            Message::new_by_str("Single comma in function call", vec![(make_span!(1, 4), "function call here")])
        ],
    )}
}