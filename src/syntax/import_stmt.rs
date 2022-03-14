////! fff-lang
///!
///! syntax/import_stmt
///! import_stmt = 'import' identifier [ 'as' identifier ] ';'

use super::prelude::*;
use super::{SimpleName};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ImportStatement {
    pub name: SimpleName,
    pub as_span: Span,
    pub target: Option<SimpleName>,
    pub all_span: Span,
}
impl ImportStatement {
    
    pub fn new_default(all_span: Span, name: SimpleName) -> ImportStatement {
        ImportStatement{ name, all_span, as_span: Span::new(0, 0), target: None }
    }
    pub fn new_target(all_span: Span, name: SimpleName, as_span: Span, target: SimpleName) -> ImportStatement {
        ImportStatement{ name, all_span, as_span, target: Some(target) }
    }

    fn new_some(all_span: Span, name: SimpleName, as_span: Span, target: Option<SimpleName>) -> ImportStatement {
        ImportStatement{ name, all_span, as_span, target }
    }
}
impl Node for ImportStatement {
    type ParseOutput = ImportStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Import)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<ImportStatement> {

        let starting_span = sess.expect_keyword(Keyword::Import)?;
        let name = SimpleName::parse(sess)?;

        let (as_span, to_ident) = if let Some(as_span) = sess.try_expect_keyword(Keyword::As) {
            (as_span, Some(SimpleName::parse(sess)?))
        } else {
            (Span::new(0, 0), None)
        };
        
        let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(ImportStatement::new_some(all_span, name, as_span, to_ident))
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_import_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_name(&self.name)?;
        if let Some(alias) = &self.target {
            v.visit_simple_name(alias)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn import_stmt_parse() {
    use super::make_node;

    assert_eq!{
        make_node!("import a;" as ImportStatement),
        ImportStatement::new_default(Span::new(0, 8),
            SimpleName::new(1, Span::new(7, 7))
        )
    }

    assert_eq!{ //                   012345678901234567890
        make_node!("import windows as os;" as ImportStatement),
        ImportStatement::new_target(Span::new(0, 20),
            SimpleName::new(1, Span::new(7, 13)),
            Span::new(15, 16),
            SimpleName::new(2, Span::new(18, 19))
        )
    }
}