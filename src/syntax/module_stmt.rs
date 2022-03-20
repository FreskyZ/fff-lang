///! syntax::module_stmt
///! module_stmt = 'module' identifier [ str_lit ] ';'

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ModuleStatement {
    pub name: IsId,
    pub name_span: Span,
    pub path: Option<(IsId, Span)>,
    pub all_span: Span,
}

impl Parser for ModuleStatement {
    type Output = ModuleStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Module)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<ModuleStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Module)?;
        let (name, name_span) = cx.expect_ident()?;

        let path = cx.try_expect_str_lit();        
        let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
        let all_span = starting_span + semicolon_span;

        Ok(ModuleStatement{ all_span, name, name_span, path })
    }
}

impl Node for ModuleStatement {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_module_stmt(self)
    }
}

#[cfg(test)] 
#[test]
fn module_stmt_parse() {

    case!{ "module a;" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 7), path: None, all_span: Span::new(0, 8) },
    }
    //                   012345678901234567890
    case!{ "module os \"windows\";" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 8), path: Some((IsId::new(3), Span::new(10, 18))), all_span: Span::new(0, 19) },
    }

    //      0         1          2
    //      012345678901234567 89012345 6
    case!{ "module otherdir r\"ab/c.f3\";" as ModuleStatement,
        ModuleStatement{ name: IsId::new(2), name_span: Span::new(7, 14), path: Some((IsId::new(3), Span::new(16, 25))), all_span: Span::new(0, 26) },
    }
}
