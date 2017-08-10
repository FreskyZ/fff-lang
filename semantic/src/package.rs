///! fff-lang
///!
///! semantic/package, a compilation unit

use codemap::SymbolCollection;
use syntax;

use super::Statement;
use super::ISemanticAnalyze;
use super::SharedDefScope;
use super::Module;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    main_module: Module,
    global_scope: SharedDefScope,
}
impl Package {

    pub fn new(tree: syntax::SyntaxTree, symbols: &mut SymbolCollection) -> Package {

        // Phase 1: direct map, scope management, dependent tree
        let global_scope = SharedDefScope::new("");

        let mut tree = tree; // do not mut in parameter because I want to leave this driver beautiful
        let mut main_module = Module::from_syntax(tree.modules[0].move_out(), global_scope.clone());
        main_module.buildup_imports(&tree.import_maps, &mut tree.modules, symbols);

        // Phase 2: collect type declares, not definitions

        Package{ global_scope, main_module }
    }
}

// TODO: beside `<anon#3>` should be `c`, `<scope #0>` should be `<scope >`
#[cfg(test)] #[test] 
fn package_buildup_import_map() {
    use std::rc::Rc;
    use codemap::SourceCode;
    use codemap::Span;
    use super::*;

    let package = Package::new(syntax::SyntaxTree::new_modules(vec![
        syntax::Module::new(Rc::new(SourceCode::pretend_with_file_name(0, vec!["main.ff"], "import a; import b;")), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 1, 1), syntax::SimpleName::new(make_id!(1), make_span!(0, 2, 2)))),
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 3, 3), syntax::SimpleName::new(make_id!(2), make_span!(0, 4, 4)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::pretend_with_file_name(1, vec!["a", "module.ff"], "import c;")), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(1, 1, 1), syntax::SimpleName::new(make_id!(3), make_span!(1, 2, 2)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::pretend_with_file_name(2, vec!["b.ff"], "")), vec![]),
        syntax::Module::new(Rc::new(SourceCode::pretend_with_file_name(3, vec!["a", "c", "module.ff"], "import d;")), vec![   // c
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(3, 1, 1), syntax::SimpleName::new(make_id!(4), make_span!(3, 2, 2)))),
        ]),
        syntax::Module::new(Rc::new(SourceCode::pretend_with_file_name(4, vec!["a", "c", "d.ff"], "")), vec![]),         // d
    ], vec![
        syntax::ImportMap::new(0, make_id!(1), 1),
        syntax::ImportMap::new(0, make_id!(2), 2),
        syntax::ImportMap::new(1, make_id!(3), 3),
        syntax::ImportMap::new(3, make_id!(4), 4),
    ]), &mut make_symbols!["a", "b", "c", "d"]);

    let expect = Module{
        module_id: 0,
        this_scope: SharedDefScope::new(""),
        items: vec![
            Item::Import(ImportStatement{
                name: SimpleName{ value: make_id!(1) },
                alias: None,
                module: Some(Module{
                    module_id: 1,
                    this_scope: SharedDefScope::new("").sub("a"),
                    items: vec![
                        Item::Import(ImportStatement{
                            name: SimpleName{ value: make_id!(3) },
                            alias: None,
                            module: Some(Module{
                                module_id: 3,
                                this_scope: SharedDefScope::new("").sub("a").sub("c"),
                                items: vec![
                                    Item::Import(ImportStatement{
                                        name: SimpleName{ value: make_id!(4) },
                                        alias: None,
                                        module: Some(Module{
                                            module_id: 4,
                                            this_scope: SharedDefScope::new("").sub("a").sub("c").sub("d"),
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
                    this_scope: SharedDefScope::new("").sub("b"),
                    items: vec![],
                }),
            }),
        ],
    };

    if package.main_module != expect {
        panic!("assertion failed, left: `{}`, right: `{}`", package.main_module.display(), expect.display());
    }
}

#[cfg(test)] #[test]
fn package_module_scope() {
    
}

// TODO: add scope name, where global is package name, fn main is package name + "::main", fn main for stmt is package name + "::main::<for-stmt<5:5-10:5>>"
// add format method, maybe directly use syntax::ISyntaxFormat and syntax::Formatter