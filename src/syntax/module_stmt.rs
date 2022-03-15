///! syntax::import_stmt
///! module_stmt = 'module' identifier [ 'as' identifier ] ';'

use super::prelude::*;
use super::{SimpleName};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ModuleStatement {
    pub name: SimpleName,
    pub as_span: Span,
    pub target: Option<SimpleName>,
    pub all_span: Span,
}
impl ModuleStatement {
    
    pub fn new_default(all_span: Span, name: SimpleName) -> Self {
        Self{ name, all_span, as_span: Span::new(0, 0), target: None }
    }
    pub fn new_target(all_span: Span, name: SimpleName, as_span: Span, target: SimpleName) -> Self {
        Self{ name, all_span, as_span, target: Some(target) }
    }

    fn new_some(all_span: Span, name: SimpleName, as_span: Span, target: Option<SimpleName>) -> Self {
        Self{ name, all_span, as_span, target }
    }
}
impl Node for ModuleStatement {
    type ParseOutput = ModuleStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Module)) 
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<ModuleStatement> {

        let starting_span = cx.expect_keyword(Keyword::Module)?;
        let name = cx.expect_node::<SimpleName>()?;

        let (as_span, to_ident) = if let Some(as_span) = cx.try_expect_keyword(Keyword::As) {
            (as_span, Some(cx.expect_node::<SimpleName>()?))
        } else {
            (Span::new(0, 0), None)
        };
        
        let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(ModuleStatement::new_some(all_span, name, as_span, to_ident))
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_module_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_name(&self.name)?;
        if let Some(alias) = &self.target {
            v.visit_simple_name(alias)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] 
#[test]
fn module_stmt_parse() {

    case!{ "module a;" as ModuleStatement,
        ModuleStatement::new_default(Span::new(0, 8),
            SimpleName::new(2, Span::new(7, 7))
        )
    }
    //                   012345678901234567890
    case!{ "module windows as os;" as ModuleStatement,
        ModuleStatement::new_target(Span::new(0, 20),
            SimpleName::new(2, Span::new(7, 13)),
            Span::new(15, 16),
            SimpleName::new(3, Span::new(18, 19))
        )
    }
}