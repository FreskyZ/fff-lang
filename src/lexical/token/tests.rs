///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

use super::*;

#[test]
fn separator_basic() {

    assert_eq!(Separator::GtGtEq.display(), ">>=");
    assert_eq!(Separator::AndAnd.display(), "&&");
    assert_eq!(Separator::Dot.display(), ".");
    assert_eq!(Separator::ColonColon.display(), "::");
    assert_eq!(Separator::Eq.display(), "=");
    assert!(Separator::LeftBracket.kind(SeparatorKind::Separator));
    assert!(!Separator::LeftBracket.kind(SeparatorKind::Shift));
    assert!(Separator::Caret.kind(SeparatorKind::BitXor));
    assert!(!Separator::Caret.kind(SeparatorKind::Multiplicative));
    assert!(Separator::MulEq.kind(SeparatorKind::Assign));
    assert!(!Separator::MulEq.kind(SeparatorKind::Shift));
    assert!(Separator::MulEq.kind(SeparatorKind::Assign));
    assert!(!Separator::MulEq.kind(SeparatorKind::LogicalOr));
    assert!(Separator::Caret.kind(SeparatorKind::BitXor));
    assert!(!Separator::Caret.kind(SeparatorKind::Separator));
}

#[test]
fn separator_parse() {

    assert_eq!(Separator::parse('<', '<', '='), Some((Separator::LtLtEq, 3)));
    assert_eq!(Separator::parse('>', '>', '='), Some((Separator::GtGtEq, 3)));
    assert_eq!(Separator::parse('+', ' ', '1'), Some((Separator::Add, 1)));
    assert_eq!(Separator::parse('!', '[', '('), Some((Separator::Not, 1)));
    assert_eq!(Separator::parse('{', ' ', 'a'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse('&', '&', ' '), Some((Separator::AndAnd, 2)));
    assert_eq!(Separator::parse('Х', '9', ' '), None);
    assert_eq!(Separator::parse('^', '(', '-'), Some((Separator::Caret, 1)));
    assert_eq!(Separator::parse('&', ',', '|'), Some((Separator::And, 1)));
    assert_eq!(Separator::parse('-', '=', ')'), Some((Separator::SubEq, 2)));
    assert_eq!(Separator::parse('*', '.', '^'), Some((Separator::Mul, 1)));
    assert_eq!(Separator::parse('=', '-', ';'), Some((Separator::Eq, 1)));
    assert_eq!(Separator::parse('/', '!', '.'), Some((Separator::Div, 1)));
    assert_eq!(Separator::parse(')', '(', '('), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse('|', '}', ':'), Some((Separator::Or, 1)));
    assert_eq!(Separator::parse('>', '(', '*'), Some((Separator::Gt, 1)));
    assert_eq!(Separator::parse('/', '%', '='), Some((Separator::Div, 1)));
    assert_eq!(Separator::parse('{', '.', '.'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse(')', '|', '&'), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse(';', '<', '/'), Some((Separator::SemiColon, 1)));
    assert_eq!(Separator::parse('!', '=', '/'), Some((Separator::NotEq, 2)));
    assert_eq!(Separator::parse('|', '.', '.'), Some((Separator::Or, 1)));
    assert_eq!(Separator::parse('{', '=', ':'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse(':', '=', '/'), Some((Separator::Colon, 1)));
    assert_eq!(Separator::parse('^', '=', '&'), Some((Separator::CaretEq, 2)));
    assert_eq!(Separator::parse('[', '=', ';'), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse('^', '=', '('), Some((Separator::CaretEq, 2)));
    assert_eq!(Separator::parse('o', 'n', ')'), None);
    assert_eq!(Separator::parse('(', 'c', 'h'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('p', 'e', ':'), None);
    assert_eq!(Separator::parse('l', 'f', '.'), None);
    assert_eq!(Separator::parse('w', '(', ')'), None);
    assert_eq!(Separator::parse('e', 'r', ')'), None);
    assert_eq!(Separator::parse('s', 'e', '('), None);
    assert_eq!(Separator::parse('(', '3', '8'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('l', 't', ','), None);
    assert_eq!(Separator::parse('n', '!', '('), None);
    assert_eq!(Separator::parse('d', '_', 'b'), None);
    assert_eq!(Separator::parse('t', '(', 'v'), None);
    assert_eq!(Separator::parse('-', '-', '-'), Some((Separator::Sub, 1)));
    assert_eq!(Separator::parse('a', 'm', '0'), None);
    assert_eq!(Separator::parse('2', '6', '7'), None);
    assert_eq!(Separator::parse('l', 't', '<'), None);
    assert_eq!(Separator::parse(':', ':', 'I'), Some((Separator::ColonColon, 2)));
    assert_eq!(Separator::parse('2', '0', '0'), None);
    assert_eq!(Separator::parse('"', 'G', 'R'), None);
    assert_eq!(Separator::parse('(', ')', ';'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('l', 'f', '('), None);
}

#[test]
fn keyword_basic() {

    assert_eq!(Keyword::Namespace.display(), "namespace");
    assert_eq!(Keyword::Loop.display(), "loop");
    assert_eq!(Keyword::Struct.display(), "struct");
    assert_eq!(Keyword::Null.display(), "null");
    assert_eq!(Keyword::Internal.display(), "internal");
    assert!(Keyword::I8.kind(KeywordKind::Primitive));
    assert!(!Keyword::I8.kind(KeywordKind::MaybeIdentifier));
    assert!(Keyword::Module.kind(KeywordKind::Normal));
    assert!(Keyword::Module.kind(KeywordKind::Normal));
    assert!(Keyword::Public.kind(KeywordKind::Reserved));
    assert!(!Keyword::Public.kind(KeywordKind::Primitive));
    assert!(Keyword::Bits64.kind(KeywordKind::Reserved));
    assert!(!Keyword::Bits64.kind(KeywordKind::Primitive));
    assert!(Keyword::Fn.kind(KeywordKind::Normal));
    assert!(!Keyword::Fn.kind(KeywordKind::Reserved));
}

#[test]
fn keyword_parse() {

    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));
    assert_eq!(Keyword::parse("await"), Some(Keyword::Await));
    assert_eq!(Keyword::parse("一个chinese变量"), None);
    assert_eq!(Keyword::parse("a_中文_var"), None);
    assert_eq!(Keyword::parse("as"), Some(Keyword::As));
    assert_eq!(Keyword::parse("GRA"), None);
    assert_eq!(Keyword::parse("pub"), Some(Keyword::Pub));
    assert_eq!(Keyword::parse("Unicode"), None);
    assert_eq!(Keyword::parse("directory"), None);
    assert_eq!(Keyword::parse("peratorKi"), None);
    assert_eq!(Keyword::parse("UnknownC"), None);
    assert_eq!(Keyword::parse("match"), Some(Keyword::Match));
    assert_eq!(Keyword::parse("owe"), None);
    assert_eq!(Keyword::parse("haredDefS"), None);
    assert_eq!(Keyword::parse("bec"), None);
    assert_eq!(Keyword::parse("allyN"), None);
    assert_eq!(Keyword::parse("ch"), None);
    assert_eq!(Keyword::parse("rnTypeMi"), None);
    assert_eq!(Keyword::parse("Keyword"), None);
    assert_eq!(Keyword::parse("ret"), Some(Keyword::Ret));
    assert_eq!(Keyword::parse("Fie"), None);
    assert_eq!(Keyword::parse("self"), Some(Keyword::Self_));
    assert_eq!(Keyword::parse("To"), None);
}
