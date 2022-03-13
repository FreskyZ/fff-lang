////! fff-lang
///!
///! syntax/import_stmt
///! import_stmt = 'import' identifier [ 'as' identifier ] ';'

use super::prelude::*;
use super::{SimpleName};

#[cfg_attr(test, derive(PartialEq))]
pub struct ImportStatement {
    pub name: SimpleName,
    pub as_span: Span,
    pub target: Option<SimpleName>,
    pub all_span: Span,
}
impl ISyntaxFormat for ImportStatement {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("import-stmt").space().span(self.all_span).endl().apply1(&self.name);
        match self.target {
            None => f.finish(),
            Some(ref ident) => 
                f.endl().indent1().lit("\"as\"").space().span(self.as_span).endl()
                    .set_prefix_text("alias-as").apply1(ident).unset_prefix_text().finish(),
        }
    }
}
impl fmt::Debug for ImportStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
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
impl<'ecx, 'scx, F> Node<'ecx, 'scx, F> for ImportStatement where F: FileSystem {
    type ParseOutput = ImportStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Import)) 
    }

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<ImportStatement> {

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