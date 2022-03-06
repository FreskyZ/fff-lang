///! fff-lang
///!
///! semantic/package, a compilation unit

use codemap::SourceMap;
use codemap::SymbolCollection;
use message::MessageCollection;
use syntax;

use super::Module;
use super::ScopeType;
use super::FromSession;
use super::CollectSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;
use super::DefinitionCollection;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub main_module: Module,
    pub global_scope: SharedDefScope,
    pub defs: DefinitionCollection,   // type and fn defs are stored in package scope because they are finally very global scope, variables do not live here
}
impl Package {

    /// semantic module driver
    ///
    /// source, messages, symbols routine, sources is not mutable because logically it is
    pub fn new(tree: syntax::SyntaxTree, sources: &SourceMap, symbols: &mut SymbolCollection, _messages: &mut MessageCollection) -> Package {
        let mut tree = tree; // do not mut in parameter because I want to leave this driver beautiful

        // Phase 1: direct map, scope management, dependent tree
        let main_source = sources.index(0);
        let global_sess = FromSession::new(SharedDefScope::new("", ScopeType::Global), main_source.as_ref(), symbols);
        let mut main_module = Module::from_syntax(tree.modules[0].move_out(), global_sess.clone_scope());
        main_module.buildup_imports(&tree.import_maps, &mut tree.modules, sources, symbols);

        // Phase 2: collect type declares, not definitions
        let mut defs = DefinitionCollection::new(); { // because def is mutable borrowed in this block... when NLL can be stable?
            let mut messages = MessageCollection::new();
            let mut collect_sess = CollectSession::new(&mut defs, &mut messages);
            main_module.collect_definitions(&mut collect_sess);
        }

        Package{ main_module, defs, global_scope: global_sess.into_scope() }
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
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 1, 1), syntax::SimpleName::new(1, make_span!(0, 2, 2)))),
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(0, 3, 3), syntax::SimpleName::new(2, make_span!(0, 4, 4)))),
        ]),
        syntax::Module::new(sources.index(1), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(1, 1, 1), syntax::SimpleName::new(3, make_span!(1, 2, 2)))),
        ]),
        syntax::Module::new(sources.index(2), vec![]),
        syntax::Module::new(sources.index(3), vec![
            syntax::Item::Import(syntax::ImportStatement::new_default(make_span!(3, 1, 1), syntax::SimpleName::new(4, make_span!(3, 2, 2)))),
        ]),
        syntax::Module::new(sources.index(4), vec![]),
    ], vec![
        syntax::ImportMap::new(0, 1, 1),
        syntax::ImportMap::new(0, 2, 2),
        syntax::ImportMap::new(1, 3, 3),
        syntax::ImportMap::new(3, 4, 4),
    ]), &sources, &mut symbols, &mut messages);

    let expect = Module{
        module_id: 0,
        this_scope: SharedDefScope::new("", ScopeType::Global),
        items: vec![
            Item::Import(ImportStatement{
                name: SimpleName{ 
                    value: 1,
                    parent_scope: SharedDefScope::new("", ScopeType::Global),
                },
                alias: None,
                parent_scope: SharedDefScope::new("", ScopeType::Global),
                module: Some(Module{
                    module_id: 1,
                    this_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global),
                    items: vec![
                        Item::Import(ImportStatement{
                            name: SimpleName{ 
                                value: 3,
                                parent_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global), 
                            },
                            alias: None,
                            parent_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global),
                            module: Some(Module{
                                module_id: 3,
                                this_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("c", ScopeType::Global),
                                items: vec![
                                    Item::Import(ImportStatement{
                                        name: SimpleName{ 
                                            value: 4,
                                            parent_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("c", ScopeType::Global),
                                        },
                                        parent_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("c", ScopeType::Global),
                                        alias: None,
                                        module: Some(Module{
                                            module_id: 4,
                                            this_scope: SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("c", ScopeType::Global).sub("d", ScopeType::Global),
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
                    value: 2,
                    parent_scope: SharedDefScope::new("", ScopeType::Global), 
                },
                alias: None,
                parent_scope: SharedDefScope::new("", ScopeType::Global),
                module: Some(Module{
                    module_id: 2,
                    this_scope: SharedDefScope::new("", ScopeType::Global).sub("b", ScopeType::Global),
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

// CONSIDER: use unique ids to replace unaccessible names (contains scope segment with span)
