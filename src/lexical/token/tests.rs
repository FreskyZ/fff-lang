///! --------------------------------------------------------------------------------
///! This code is auto generated by a tool
///! Changes may cause incorrect behavior and will be lost if the code is regenerated
///! --------------------------------------------------------------------------------

use super::*;

#[test]
fn separator_basic() {

    assert_eq!(Separator::Dot.display(), ".");
    assert_eq!(Separator::Rem.display(), "%");
    assert_eq!(Separator::Comma.display(), ",");
    assert_eq!(Separator::DotDot.display(), "..");
    assert_eq!(Separator::CaretEq.display(), "^=");
    assert!(Separator::DotDot.kind(SeparatorKind::Range));
    assert!(!Separator::DotDot.kind(SeparatorKind::Assign));
    assert!(Separator::Dot.kind(SeparatorKind::Separator));
    assert!(!Separator::Dot.kind(SeparatorKind::BitXor));
    assert!(Separator::AndAnd.kind(SeparatorKind::LogicalAnd));
    assert!(!Separator::AndAnd.kind(SeparatorKind::BitXor));
    assert!(Separator::NotEq.kind(SeparatorKind::Equality));
    assert!(!Separator::NotEq.kind(SeparatorKind::LogicalAnd));
    assert!(Separator::And.kind(SeparatorKind::BitAnd));
    assert!(!Separator::And.kind(SeparatorKind::Relational));
}

#[test]
fn separator_parse() {

    assert_eq!(Separator::parse3('<', '<', '='), Some((Separator::LtLtEq, 3)));
    assert_eq!(Separator::parse3('>', '>', '='), Some((Separator::GtGtEq, 3)));
    assert_eq!(Separator::parse3('+', ' ', '1'), Some((Separator::Add, 1)));
    assert_eq!(Separator::parse3('!', '[', '('), Some((Separator::Not, 1)));
    assert_eq!(Separator::parse3('{', ' ', 'a'), Some((Separator::LeftBrace, 1)));
    assert_eq!(Separator::parse3('&', '&', ' '), Some((Separator::AndAnd, 2)));
    assert_eq!(Separator::parse3('Х', '9', ' '), None);
    assert_eq!(Separator::parse3('[', '=', ';'), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse3('/', '!', '&'), Some((Separator::Div, 1)));
    assert_eq!(Separator::parse3('%', '=', '~'), Some((Separator::RemEq, 2)));
    assert_eq!(Separator::parse3(';', '!', '!'), Some((Separator::SemiColon, 1)));
    assert_eq!(Separator::parse3('^', '{', '*'), Some((Separator::Caret, 1)));
    assert_eq!(Separator::parse3('[', ',', '('), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse3(':', '=', ';'), Some((Separator::Colon, 1)));
    assert_eq!(Separator::parse3(':', '=', '('), Some((Separator::Colon, 1)));
    assert_eq!(Separator::parse3('^', '!', ']'), Some((Separator::Caret, 1)));
    assert_eq!(Separator::parse3('(', '=', '&'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse3('[', '+', '('), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse3(')', '~', '/'), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse3('~', '=', ','), Some((Separator::Tilde, 1)));
    assert_eq!(Separator::parse3('+', '*', '}'), Some((Separator::Add, 1)));
    assert_eq!(Separator::parse3(')', '{', '!'), Some((Separator::RightParen, 1)));
    assert_eq!(Separator::parse3(':', ';', '*'), Some((Separator::Colon, 1)));
    assert_eq!(Separator::parse3('<', '[', '^'), Some((Separator::Lt, 1)));
    assert_eq!(Separator::parse3('&', '=', '<'), Some((Separator::AndEq, 2)));
    assert_eq!(Separator::parse3('.', '=', '%'), Some((Separator::Dot, 1)));
    assert_eq!(Separator::parse3('!', '=', ']'), Some((Separator::NotEq, 2)));
    assert_eq!(Separator::parse1('f'), None);
    assert_eq!(Separator::parse1('t'), None);
    assert_eq!(Separator::parse1('u'), None);
    assert_eq!(Separator::parse3(':', 'w', 'l'), Some((Separator::Colon, 1)));
    assert_eq!(Separator::parse1('m'), None);
    assert_eq!(Separator::parse1('o'), None);
    assert_eq!(Separator::parse1('n'), None);
    assert_eq!(Separator::parse3('t', '_', 'e'), None);
    assert_eq!(Separator::parse3('d', ':', ':'), None);
    assert_eq!(Separator::parse3('(', 'n', 'o'), Some((Separator::LeftParen, 1)));
    assert_eq!(Separator::parse1('F'), None);
    assert_eq!(Separator::parse1('n'), None);
    assert_eq!(Separator::parse1('s'), None);
    assert_eq!(Separator::parse1('h'), None);
    assert_eq!(Separator::parse1('e'), None);
    assert_eq!(Separator::parse3('x', '(', 'n'), None);
    assert_eq!(Separator::parse1('t'), None);
    assert_eq!(Separator::parse1('t'), None);
    assert_eq!(Separator::parse1('('), Some(Separator::LeftParen));
    assert_eq!(Separator::parse1(':'), Some(Separator::Colon));
    assert_eq!(Separator::parse3('2', '8', '>'), None);
    assert_eq!(Separator::parse3('s', 't', ')'), None);
    assert_eq!(Separator::parse3('[', '0', ']'), Some((Separator::LeftBracket, 1)));
    assert_eq!(Separator::parse1('u'), None);
    assert_eq!(Separator::parse1('m'), None);
    assert_eq!(Separator::parse1('t'), None);
    assert_eq!(Separator::parse1('t'), None);
    assert_eq!(Separator::parse1('r'), None);
    assert_eq!(Separator::parse1('S'), None);
    assert_eq!(Separator::parse3('0', '7', '3'), None);
    assert_eq!(Separator::parse1('d'), None);
    assert_eq!(Separator::parse3('a', 'n', ';'), None);
    assert_eq!(Separator::parse3('P', '"', ')'), None);
    assert_eq!(Separator::parse1('p'), None);
    assert_eq!(Separator::parse1('N'), None);
}

#[test]
fn keyword_basic() {

    assert_eq!(Keyword::Bool.display(), "bool");
    assert_eq!(Keyword::Self_.display(), "self");
    assert_eq!(Keyword::Bits16.display(), "bits16");
    assert_eq!(Keyword::Goto.display(), "goto");
    assert_eq!(Keyword::Yield.display(), "yield");
    assert!(Keyword::Override.kind(KeywordKind::Reserved));
    assert!(!Keyword::Override.kind(KeywordKind::Primitive));
    assert!(Keyword::While.kind(KeywordKind::Normal));
    assert!(!Keyword::While.kind(KeywordKind::Reserved));
    assert!(Keyword::Ret.kind(KeywordKind::Reserved));
    assert!(Keyword::Ret.kind(KeywordKind::Reserved));
    assert!(Keyword::R64.kind(KeywordKind::Reserved));
    assert!(!Keyword::R64.kind(KeywordKind::Primitive));
    assert!(Keyword::Break.kind(KeywordKind::Normal));
    assert!(!Keyword::Break.kind(KeywordKind::Primitive));
}

#[test]
fn keyword_parse() {

    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));
    assert_eq!(Keyword::parse("await"), Some(Keyword::Await));
    assert_eq!(Keyword::parse("一个chinese变量"), None);
    assert_eq!(Keyword::parse("a_中文_var"), None);
    assert_eq!(Keyword::parse("as"), Some(Keyword::As));
    assert_eq!(Keyword::parse("as"), Some(Keyword::As));
    assert_eq!(Keyword::parse("ywor"), None);
    assert_eq!(Keyword::parse("in"), Some(Keyword::In));
    assert_eq!(Keyword::parse("new"), None);
    assert_eq!(Keyword::parse("in"), Some(Keyword::In));
    assert_eq!(Keyword::parse("osition"), None);
    assert_eq!(Keyword::parse("ccessExp"), None);
    assert_eq!(Keyword::parse("hec"), None);
    assert_eq!(Keyword::parse("rra"), None);
    assert_eq!(Keyword::parse("ver"), None);
    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));
    assert_eq!(Keyword::parse("orKi"), None);
    assert_eq!(Keyword::parse("or"), Some(Keyword::Or));
    assert_eq!(Keyword::parse("an"), None);
}
