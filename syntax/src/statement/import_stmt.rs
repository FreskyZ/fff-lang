////! fff-lang
///!
///! syntax/import_stmt
///! import_stmt = 'import' identifier [ 'as' identifier ] ';'

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Seperator;
use lexical::Keyword;

use super::super::IdentExpr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ImportStatement {
    pub name: IdentExpr,
    pub as_span: Span,
    pub target: Option<IdentExpr>,
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
    
    pub fn new_default(all_span: Span, name: IdentExpr) -> ImportStatement {
        ImportStatement{ name, all_span, as_span: Span::default(), target: None }
    }
    pub fn new_target(all_span: Span, name: IdentExpr, as_span: Span, target: IdentExpr) -> ImportStatement {
        ImportStatement{ name, all_span, as_span, target: Some(target) }
    }

    fn new_some(all_span: Span, name: IdentExpr, as_span: Span, target: Option<IdentExpr>) -> ImportStatement {
        ImportStatement{ name, all_span, as_span, target }
    }
}
impl ISyntaxItemGrammar for ImportStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(Keyword::Import) }
}
impl ISyntaxItemParse for ImportStatement {
    type Target = ImportStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ImportStatement> {

        let starting_span = sess.expect_keyword(Keyword::Import)?;
        let name = IdentExpr::parse(sess)?;

        let (as_span, to_ident) = if let &Token::Keyword(Keyword::As) = sess.tk {
            let as_span = sess.pos;
            sess.move_next();
            (as_span, Some(IdentExpr::parse(sess)?))
        } else {
            (Span::default(), None)
        };
        let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
        let all_span = starting_span.merge(&semicolon_span);

        Ok(ImportStatement::new_some(all_span, name, as_span, to_ident))
    }
}

#[cfg(test)] #[test]
fn import_stmt_parse() {
    use super::super::WithTestInput;

    assert_eq!{
        ImportStatement::with_test_str("import a;"),
        ImportStatement::new_default(make_span!(0, 8),
            IdentExpr::new(make_id!(1), make_span!(7, 7))
        )
    }

    assert_eq!{ //                   012345678901234567890
        ImportStatement::with_test_str("import windows as os;"),
        ImportStatement::new_target(make_span!(0, 20),
            IdentExpr::new(make_id!(1), make_span!(7, 13)),
            make_span!(15, 16),
            IdentExpr::new(make_id!(2), make_span!(18, 19))
        )
    }
}