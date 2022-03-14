////! fff-lang
///!
///! syntax/use_stmt
///! use_stmt = 'use' name [ 'as' identifier ] ';'

use super::prelude::*;
use super::{Name, SimpleName};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct UseStatement {
    pub name: Name,
    pub as_span: Span,
    pub target: Option<SimpleName>,
    pub all_span: Span,
}
impl UseStatement {
    
    pub fn new_default(all_span: Span, name: Name) -> UseStatement {
        UseStatement{ name, all_span, as_span: Span::new(0, 0), target: None }
    }
    pub fn new_target(all_span: Span, name: Name, as_span: Span, target: SimpleName) -> UseStatement {
        UseStatement{ name, all_span, as_span, target: Some(target) }
    }

    fn new_some(all_span: Span, name: Name, as_span: Span, target: Option<SimpleName>) -> UseStatement {
        UseStatement{ name, all_span, as_span, target }
    }
}
impl Node for UseStatement {
    type ParseOutput = UseStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Use)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<UseStatement> {

        let starting_span = sess.expect_keyword(Keyword::Use)?;
        let from_name = Name::parse(sess)?.into_name();

        let (as_span, to_ident) = if let Some(as_span) = sess.try_expect_keyword(Keyword::As) {
            (as_span, Some(SimpleName::parse(sess)?))
        } else {
            (Span::new(0, 0), None)
        };
        let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(UseStatement::new_some(all_span, from_name, as_span, to_ident))
    }
    
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_use_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_name(&self.name)?;
        if let Some(alias) = &self.target {
            v.visit_simple_name(alias)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn use_stmt_parse() {
    use super::make_node;

    assert_eq!{
        make_node!("use a;" as UseStatement),
        UseStatement::new_default(Span::new(0, 5),
            Name::new(Span::new(4, 4), vec![SimpleName::new(1, Span::new(4, 4))])
        )
    }

    assert_eq!{ //                   0123456789012345678901234567890
        make_node!("use std::fmt::Debug as Display;" as UseStatement),
        UseStatement::new_target(Span::new(0, 30), 
            Name::new(Span::new(4, 18), vec![
                SimpleName::new(1, Span::new(4, 6)),
                SimpleName::new(2, Span::new(9, 11)),
                SimpleName::new(3, Span::new(14, 18))
            ]),
            Span::new(20, 21),
            SimpleName::new(4, Span::new(23, 29))
        )
    }
}