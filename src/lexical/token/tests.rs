///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

use super::*;

#[test]
fn separator_basic() {

    assert_eq!(Separator::Div.display(), "/");
    assert_eq!(Separator::GtGtEq.display(), ">>=");
    assert_eq!(Separator::ColonColon.display(), "::");
    assert_eq!(Separator::LeftBrace.display(), "{");
    assert_eq!(Separator::AndAnd.display(), "&&");
    assert!(Separator::SemiColon.kind(SeparatorKind::Separator));
    assert!(!Separator::SemiColon.kind(SeparatorKind::Assign));
    assert!(Separator::LeftBracket.kind(SeparatorKind::Separator));
    assert!(!Separator::LeftBracket.kind(SeparatorKind::LogicalOr));
    assert!(Separator::DivEq.kind(SeparatorKind::Assign));
    assert!(!Separator::DivEq.kind(SeparatorKind::Range));
    assert!(Separator::LtEq.kind(SeparatorKind::Relational));
    assert!(!Separator::LtEq.kind(SeparatorKind::Separator));
    assert!(Separator::GtGtEq.kind(SeparatorKind::Assign));
    assert!(!Separator::GtGtEq.kind(SeparatorKind::Shift));
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
    assert_eq!(Separator::parse('+', '=', '<'), Some((Separator::AddEq, 2)));
    assert_eq!(Separator::parse(')', '/', '<'), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse('|', '/', ')'), Some((Separator::Or, 1)));
    assert_eq!(Separator::parse('{', '=', '<'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse('(', '{', ','), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('%', '[', '-'), Some((Separator::Rem, 1)));
    assert_eq!(Separator::parse('!', ']', '|'), Some((Separator::Not, 1)));
    assert_eq!(Separator::parse('|', '/', '>'), Some((Separator::Or, 1)));
    assert_eq!(Separator::parse(',', '=', '!'), Some((Separator::Comma, 1)));
    assert_eq!(Separator::parse('[', '=', ']'), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse('/', '=', '<'), Some((Separator::DivEq, 2)));
    assert_eq!(Separator::parse('^', '+', '|'), Some((Separator::Caret, 1)));
    assert_eq!(Separator::parse('*', '}', '~'), Some((Separator::Mul, 1)));
    assert_eq!(Separator::parse('!', '+', '['), Some((Separator::Not, 1)));
    assert_eq!(Separator::parse('>', '=', '.'), Some((Separator::GtEq, 2)));
    assert_eq!(Separator::parse(')', '/', '{'), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse('/', '=', '~'), Some((Separator::DivEq, 2)));
    assert_eq!(Separator::parse('.', '=', '&'), Some((Separator::Dot, 1)));
    assert_eq!(Separator::parse('!', '=', ','), Some((Separator::NotEq, 2)));
    assert_eq!(Separator::parse('{', '=', '*'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse(':', ':', 'n'), Some((Separator::ColonColon, 2)));
    assert_eq!(Separator::parse('9', '0', '1'), None);
    assert_eq!(Separator::parse('(', '8', ')'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('u', '1', '6'), None);
    assert_eq!(Separator::parse('r', '!', '('), None);
    assert_eq!(Separator::parse('́', '\'', ','), None);
    assert_eq!(Separator::parse('6', '7', '4'), None);
    assert_eq!(Separator::parse('(', 'l', 'e'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('n', 'e', ')'), None);
    assert_eq!(Separator::parse('r', '!', '('), None);
    assert_eq!(Separator::parse('(', '(', '\''), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('0', ')', ','), None);
    assert_eq!(Separator::parse('e', 'r', ':'), None);
    assert_eq!(Separator::parse('e', 'n', ':'), None);
    assert_eq!(Separator::parse('(', 'v', 'a'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('l', 'l', '_'), None);
    assert_eq!(Separator::parse('(', 't', 'e'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('e', '(', '&'), None);
    assert_eq!(Separator::parse('x', ')', '.'), None);
    assert_eq!(Separator::parse('(', 'm', 'a'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('f', 'g', '['), None);
    assert_eq!(Separator::parse('(', 's', 'e'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse('t', '.', 'r'), None);
    assert_eq!(Separator::parse('.', 'i', 'n'), Some((Separator::Dot, 1)));
    assert_eq!(Separator::parse('e', 'c', ':'), None);
    assert_eq!(Separator::parse('l', ')', '.'), None);
    assert_eq!(Separator::parse('s', ')', ';'), None);
    assert_eq!(Separator::parse('0', ')', ','), None);
    assert_eq!(Separator::parse('n', 'e', '.'), None);
    assert_eq!(Separator::parse('"', 'A', 'S'), None);
    assert_eq!(Separator::parse('t', '_', 'b'), None);
    assert_eq!(Separator::parse('r', '(', '\''), None);
}

#[test]
fn keyword_basic() {

    assert_eq!(Keyword::Continue.display(), "continue");
    assert_eq!(Keyword::Is.display(), "is");
    assert_eq!(Keyword::Else.display(), "else");
    assert_eq!(Keyword::Interface.display(), "interface");
    assert_eq!(Keyword::F64.display(), "f64");
    assert!(Keyword::I64.kind(KeywordKind::Primitive));
    assert!(!Keyword::I64.kind(KeywordKind::Reserved));
    assert!(Keyword::I128.kind(KeywordKind::Reserved));
    assert!(!Keyword::I128.kind(KeywordKind::Normal));
    assert!(Keyword::R32.kind(KeywordKind::Reserved));
    assert!(!Keyword::R32.kind(KeywordKind::Normal));
    assert!(Keyword::I32.kind(KeywordKind::Primitive));
    assert!(!Keyword::I32.kind(KeywordKind::MaybeIdentifier));
    assert!(Keyword::Const.kind(KeywordKind::Normal));
    assert!(!Keyword::Const.kind(KeywordKind::MaybeIdentifier));
}

#[test]
fn keyword_parse() {

    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));
    assert_eq!(Keyword::parse("await"), Some(Keyword::Await));
    assert_eq!(Keyword::parse("一个chinese变量"), None);
    assert_eq!(Keyword::parse("a_中文_var"), None);
    assert_eq!(Keyword::parse("as"), Some(Keyword::As));
    assert_eq!(Keyword::parse("ma"), None);
    assert_eq!(Keyword::parse("ase"), None);
    assert_eq!(Keyword::parse("NegativeO"), None);
    assert_eq!(Keyword::parse("AMP"), None);
    assert_eq!(Keyword::parse("ne"), None);
    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));
    assert_eq!(Keyword::parse("Stat"), None);
    assert_eq!(Keyword::parse("har"), None);
    assert_eq!(Keyword::parse("dotd"), None);
    assert_eq!(Keyword::parse("sse"), None);
    assert_eq!(Keyword::parse("ass"), None);
    assert_eq!(Keyword::parse("yDupDefS"), None);
    assert_eq!(Keyword::parse("xpec"), None);
    assert_eq!(Keyword::parse("Ex"), None);
    assert_eq!(Keyword::parse("static"), Some(Keyword::Static));
    assert_eq!(Keyword::parse("trin"), None);
}
