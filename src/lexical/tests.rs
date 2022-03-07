use crate::source::{IsId, make_source};
use super::*;

#[test]
fn v4_base() { // remain the name of v4 here for memory

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let mut scx = make_source!("123 abc 'd', [1]");
    let chars = scx.entry("1");
    let mut messages = MessageCollection::new();
    let tokens = TokenStream::new(chars, &mut messages);

    assert_eq!(tokens.nth_token(0), &Token::Num(Numeric::I32(123)));
    assert_eq!(tokens.nth_span(0), Span::new(0, 2));

    assert_eq!(tokens.nth_token(1), &Token::Ident(IsId::new(2)));
    assert_eq!(tokens.nth_span(1), Span::new(4, 6));

    assert_eq!(tokens.nth_token(2), &Token::Char('d'));
    assert_eq!(tokens.nth_span(2), Span::new(8, 10));

    assert_eq!(tokens.nth_token(3), &Token::Sep(Separator::Comma));
    assert_eq!(tokens.nth_span(3), Span::new(11, 11));

    assert_eq!(tokens.nth_token(4), &Token::Sep(Separator::LeftBracket));
    assert_eq!(tokens.nth_span(4), Span::new(13, 13));

    assert_eq!(tokens.nth_token(5), &Token::Num(Numeric::I32(1)));
    assert_eq!(tokens.nth_span(5), Span::new(14, 14));

    assert_eq!(tokens.nth_token(6), &Token::Sep(Separator::RightBracket));
    assert_eq!(tokens.nth_span(6), Span::new(15, 15));

    assert_eq!(tokens.nth_token(7), &Token::EOF);
    assert_eq!(tokens.nth_span(7), Span::new(16, 16));

    assert_eq!(tokens.nth_token(8), &Token::EOF);
    assert_eq!(tokens.nth_span(8), Span::new(16, 16));

    assert_eq!(tokens.nth_token(9), &Token::EOF);
    assert_eq!(tokens.nth_span(9), Span::new(16, 16));

    assert_eq!(tokens.nth_token(42), &Token::EOF);
    assert_eq!(tokens.nth_span(42), Span::new(16, 16));
}
