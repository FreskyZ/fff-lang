///! syntax::module_stmt

#[cfg(test)] 
#[test]
fn module_stmt_parse() {use super::prelude::*;

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
