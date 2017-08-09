///! fff-lang
///!
///! semantic/package, a compilation unit

use std::collections::LinkedList;

use std::rc::Rc;
use std::cell::RefCell;

use syntax;

use super::Statement;
use super::ISemanticAnalyze;
use super::DefScope;
use super::SharedDefScope;
use super::Module;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    main_module: Module,
    global_scope: SharedDefScope,
}
impl Package {
    
    pub fn new(tree: syntax::SyntaxTree) -> Package {

        // Phase 1: direct map, scope management
        let global_scope = Rc::new(RefCell::new(DefScope::new(String::new()))); // yes 4 news
        let import_maps = tree.import_maps;
        let mut modules = tree.modules.into_iter().map(|module| Module::from_syntax(module, global_scope.clone())).collect::<Vec<Module>>();

        // Phase 1 special: build up module dependence tree
        let mut main_module = modules[0].move_out();
        main_module.buildup_imports(&import_maps, &mut modules);

        Package{ global_scope, main_module }
    }
}

#[cfg(test)] #[test]
fn analyze_scope_manage() {
    
}

#[cfg(test)] #[test]
fn package_buildup_import_map() {
    use codemap::SourceCode;
    use codemap::Span;
    use super::*;

    let package = Package::new(syntax::SyntaxTree::new_modules(vec![
        syntax::Module::new(Rc::new(SourceCode::with_test_str(0, "import a; import b;")), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 1, 1), syntax::SimpleName::new(make_id!(1), make_span!(0, 2, 2)))),
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 3, 3), syntax::SimpleName::new(make_id!(2), make_span!(0, 4, 4)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::with_test_str(1, "import c;")), vec![   // a
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(1, 1, 1), syntax::SimpleName::new(make_id!(3), make_span!(1, 2, 2)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::with_test_str(2, "")), vec![]),         // b
        syntax::Module::new(Rc::new(SourceCode::with_test_str(3, "import d;")), vec![   // c
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(3, 1, 1), syntax::SimpleName::new(make_id!(4), make_span!(3, 2, 2)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::with_test_str(4, "")), vec![]),         // d
    ], vec![
        syntax::ImportMap::new(0, make_id!(1), 1),
        syntax::ImportMap::new(0, make_id!(2), 2),
        syntax::ImportMap::new(1, make_id!(3), 3),
        syntax::ImportMap::new(3, make_id!(4), 4),
    ]));

    let expect = Module{
        module_id: 0,
        items: vec![
            Item::Import(ImportStatement{
                name: SimpleName{ value: make_id!(1) },
                alias: None,
                module: Some(Module{
                    module_id: 1,
                    items: vec![
                        Item::Import(ImportStatement{
                            name: SimpleName{ value: make_id!(3) },
                            alias: None,
                            module: Some(Module{
                                module_id: 3,
                                items: vec![
                                    Item::Import(ImportStatement{
                                        name: SimpleName{ value: make_id!(4) },
                                        alias: None,
                                        module: Some(Module{
                                            module_id: 4,
                                            items: vec![],
                                        }),
                                    }),
                                ],
                            }),
                        }),
                    ],
                }),
            }),
            Item::Import(ImportStatement{
                name: SimpleName{ value: make_id!(2) },
                alias: None,
                module: Some(Module{
                    module_id: 2,
                    items: vec![],
                }),
            }),
        ],
    };

    assert_eq!(package.main_module, expect);
}

// TODO: add scope name, where global is package name, fn main is package name + "::main", fn main for stmt is package name + "::main::<for-stmt<5:5-10:5>>"
// add format method, maybe directly use syntax::ISyntaxFormat and syntax::Formatter