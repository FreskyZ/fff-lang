///! fff-lang
///!
///! semantic/package, a compilation unit

use codemap::SourceMap;
use codemap::SymbolCollection;
use message::MessageCollection;
use syntax;

use super::Module;
use super::FromSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub main_module: Module,
    pub global_scope: SharedDefScope,
}
impl Package {

    /// semantic module driver
    ///
    /// source, messages, symbols routine, sources is not mutable because logically it is
    pub fn new(tree: syntax::SyntaxTree, sources: &SourceMap, symbols: &mut SymbolCollection, _messages: &mut MessageCollection) -> Package {
        let mut tree = tree; // do not mut in parameter because I want to leave this driver beautiful

        // Phase 1: direct map, scope management, dependent tree
        let main_source = sources.index(0);
        let global_sess = FromSession::new(SharedDefScope::new(""), main_source.as_ref(), symbols);
        let mut main_module = Module::from_syntax(tree.modules[0].move_out(), global_sess.clone_scope());
        main_module.buildup_imports(&tree.import_maps, &mut tree.modules, sources, symbols);

        // Phase 2: collect type declares, not definitions

        Package{ global_scope: global_sess.into_scope(), main_module }
    }
}

#[cfg(test)] #[test] 
fn package_buildup_import_map() {
    use std::rc::Rc;
    use codemap::SourceCode;
    use codemap::Span;
    use super::*;

    let sources = make_sources![
        Rc::new(SourceCode::pretend_with_file_name(0, vec!["main.ff"], "import a; import b;")),
        Rc::new(SourceCode::pretend_with_file_name(1, vec!["a", "module.ff"], "import c;")),
        Rc::new(SourceCode::pretend_with_file_name(2, vec!["b.ff"], "")),
        Rc::new(SourceCode::pretend_with_file_name(3, vec!["a", "c", "module.ff"], "import d;")),
        Rc::new(SourceCode::pretend_with_file_name(4, vec!["a", "c", "d.ff"], "")),
    ];
    let mut symbols = make_symbols!["a", "b", "c", "d"];
    let mut messages = make_messages![];

    let package = Package::new(syntax::SyntaxTree::new_modules(vec![
        syntax::Module::new(sources.index(0), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 1, 1), syntax::SimpleName::new(make_id!(1), make_span!(0, 2, 2)))),
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 3, 3), syntax::SimpleName::new(make_id!(2), make_span!(0, 4, 4)))),
        ]),
        syntax::Module::new(sources.index(1), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(1, 1, 1), syntax::SimpleName::new(make_id!(3), make_span!(1, 2, 2)))),
        ]),
        syntax::Module::new(sources.index(2), vec![]),
        syntax::Module::new(sources.index(3), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(3, 1, 1), syntax::SimpleName::new(make_id!(4), make_span!(3, 2, 2)))),
        ]),
        syntax::Module::new(sources.index(4), vec![]),
    ], vec![
        syntax::ImportMap::new(0, make_id!(1), 1),
        syntax::ImportMap::new(0, make_id!(2), 2),
        syntax::ImportMap::new(1, make_id!(3), 3),
        syntax::ImportMap::new(3, make_id!(4), 4),
    ]), &sources, &mut symbols, &mut messages);

    let expect = Module{
        module_id: 0,
        this_scope: SharedDefScope::new(""),
        items: vec![
            Item::Import(ImportStatement{
                name: SimpleName{ 
                    value: make_id!(1),
                    parent_scope: SharedDefScope::new(""),
                },
                alias: None,
                parent_scope: SharedDefScope::new(""),
                module: Some(Module{

                    module_id: 1,
                    this_scope: SharedDefScope::new("").sub("a"),
                    items: vec![
                        Item::Import(ImportStatement{
                            name: SimpleName{ 
                                value: make_id!(3),
                                parent_scope: SharedDefScope::new("").sub("a"), 
                            },
                            alias: None,
                            parent_scope: SharedDefScope::new("").sub("a"),
                            module: Some(Module{
                                module_id: 3,
                                this_scope: SharedDefScope::new("").sub("a").sub("c"),
                                items: vec![
                                    Item::Import(ImportStatement{
                                        name: SimpleName{ 
                                            value: make_id!(4),
                                            parent_scope: SharedDefScope::new("").sub("a").sub("c"),
                                        },
                                        parent_scope: SharedDefScope::new("").sub("a").sub("c"),
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
                name: SimpleName{ 
                    value: make_id!(2),
                    parent_scope: SharedDefScope::new(""), 
                },
                alias: None,
                parent_scope: SharedDefScope::new(""),
                module: Some(Module{
                    module_id: 2,
                    this_scope: SharedDefScope::new("").sub("b"),
                    items: vec![],
                }),
            }),
        ],
    };

    if package.main_module != expect {
        let symbols = make_symbols!["a", "b", "c", "d"];
        // assert_eq!(package.main_module, expect);
        let formatter = Formatter::new(None, Some(&symbols));
        panic!("assertion failed, left: `\n{}`, right: `\n{}`", package.main_module.format(formatter.clone()), expect.format(formatter));
    }
}

// TODO: Add source to from_syntax ...
// let's use FromSession