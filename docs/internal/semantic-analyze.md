# fff-lang Internal Document

## Semantic Analyze

**phase 1**, convert syntax tree to semantic tree, nearly direct map tree structure, drop span info if not needed, at the same time, build up an associate scope tree, a semantic node maybe either owner of a scope node or, holding a ref to an ancestor's scope node, like this

          some-root{ scope, ... }
          / (1)      / (2) \ (2)
        /          /       \_____________
        /          /                      \
    some-node{ scope, ... }, other-node{ scope, ... }, ...
      | (1)       \ (2)
      |            \
    another-node{ scope, ... }, ...

1. child and parent or multiple level of child and parent  
2. outer and inner scope

debug display:

```
main module              // <scope>
    def a                // def a
    def b                // def b
    module c             // <scope c>
        def d            // def c::d
    modele e             // <scope e>
        def f            // def e::f
        def g            // def e::G
        model h          // <scope e::h>
            def i        // def e::h::i
            fn j                     // <scope e::h::j>, def e::h::j
                block <<3>100-200>   // <scope e::h::j::<block<<3>100-200>>>
                    var k            // def e::h::j::<block<<3>100-200>>::k
                for <<3>300-400>     // <scope e::h::j::<for<<3>300-400>>>
                    if <<3>320-360>  // <scope e::h::j::<for<<3>300-400>>::<if<<3>320-360>>>
                        var l        // def e::h::j::<for<<3>300-400>>::<if<<3>320-360>>::l   
```

**phase 2**, collect definitions, iterate through semantic tree, recording definitions, a scope owns its definitions, a definition is a symbol name and its child node id of the definition scope's owner's child node list, definition types may include

  - typedef, typedef can be used before definition
  - fndef, invisible after scope
  - type template def, for array and tuple currently, type template def can be used before definition, invisible after scope
  - fn template def, for array and tuple methods currently, fn template def can be used before definition, invisible after scope
  - variable def, for type field and function member, var def **cannot** be used before definition, type members maybe visible after scope, local vars is destructed (not deconstructed) after scope
  - template var def, for template type fields and template fn locals

**phase 3**, bind symbol uses to definitions, actually is storing item id of the node's scope node's definition list, attentions maybe

- variable can only be accessed after defined, while other definiton types not
- local variables cannot be accessed in same or inner level type definitions, but leave constexpr variables can, these feature is really needed, but not in handin2 milestone

### After 
that, try design static reflection API and check usability, the semantic tree may will be

main module
    defs            // each node and defs' items are Rc<RefCell<>>, while each use is a Weak<RefCell<>>
        fn main
        type a::b
        var a::c
        fn d::e
        var d::e::f
    def fn main      // TODO: main can be in any module, but even with namespace prefix, main cannot be referenced, cannot be called and there should be only one functionc called main
    module a
        def type b
        def var c
    module d
        def fn e
            def var f
            use var c

// TODO: record use count in definiton, warn on not used
// TODO: consider fndef as variable and how handle pure function, captured or binded function and variable name resolve issues
