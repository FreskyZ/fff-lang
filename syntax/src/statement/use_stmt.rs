////! fff-lang
///!
///! syntax/use_stmt
///! use_stmt = 'use' name [ 'as' identifier ] ';'

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Seperator;
use lexical::Keyword;

use super::super::Name;
use super::super::SimpleName;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxFormat;
use super::super::ISyntaxParse;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct UseStatement {
    pub name: Name,
    pub as_span: Span,
    pub target: Option<SimpleName>,
    pub all_span: Span,
}
impl ISyntaxFormat for UseStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("use-stmt").space().span(self.all_span).endl()
            .set_prefix_text("alias-from").apply1(&self.name).unset_prefix_text();
        match self.target {
            None => f.finish(),
            Some(ref ident) => 
                f.endl().indent1().lit("\"as\"").space().span(self.as_span).endl()
                    .set_prefix_text("alias-to").apply1(ident).unset_prefix_text().finish(),
        }
    }
}
impl fmt::Debug for UseStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl UseStatement {
    
    pub fn new_default(all_span: Span, name: Name) -> UseStatement {
        UseStatement{ name, all_span, as_span: Span::default(), target: None }
    }
    pub fn new_target(all_span: Span, name: Name, as_span: Span, target: SimpleName) -> UseStatement {
        UseStatement{ name, all_span, as_span, target: Some(target) }
    }

    fn new_some(all_span: Span, name: Name, as_span: Span, target: Option<SimpleName>) -> UseStatement {
        UseStatement{ name, all_span, as_span, target }
    }
}
impl ISyntaxGrammar for UseStatement {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::Use) }
}
impl ISyntaxParse for UseStatement {
    type Output = UseStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<UseStatement> {

        let starting_span = sess.expect_keyword(Keyword::Use)?;
        let from_name = Name::parse(sess)?.into_name();

        let (as_span, to_ident) = if let Some(as_span) = sess.try_expect_keyword(Keyword::As) {
            (as_span, Some(SimpleName::parse(sess)?))
        } else {
            (Span::default(), None)
        };
        let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
        let all_span = starting_span.merge(&semicolon_span);

        Ok(UseStatement::new_some(all_span, from_name, as_span, to_ident))
    }
}

#[cfg(test)] #[test]
fn use_stmt_parse() {
    use super::super::WithTestInput;

    assert_eq!{
        UseStatement::with_test_str("use a;"),
        UseStatement::new_default(make_span!(0, 5),
            Name::new(make_span!(4, 4), vec![SimpleName::new(make_id!(1), make_span!(4, 4))])
        )
    }

    assert_eq!{ //                   0123456789012345678901234567890
        UseStatement::with_test_str("use std::fmt::Debug as Display;"),
        UseStatement::new_target(make_span!(0, 30), 
            Name::new(make_span!(4, 18), vec![
                SimpleName::new(make_id!(1), make_span!(4, 6)),
                SimpleName::new(make_id!(2), make_span!(9, 11)),
                SimpleName::new(make_id!(3), make_span!(14, 18))
            ]),
            make_span!(20, 21),
            SimpleName::new(make_id!(4), make_span!(23, 29))
        )
    }
}