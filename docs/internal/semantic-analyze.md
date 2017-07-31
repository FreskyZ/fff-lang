# fff-lang Internal Document

## Semantic Analyze

First, convert syntax tree to semantic tree, recording all symbol and sub node information, but leave span information unconverted and convert them when required, at the same time, build up a scope tree depend on the semantic tree, while a scope is either the node's owned scope or the node's some kind of parent scope.

Then, iterate through semantic tree, recording all kind of definitions, 

  - typedef, typedef can be used before definition
  - fndef, invisible after scope
  - type template def, for array and tuple currently, type template def can be used before definition, invisible after scope
  - fn template def, for array and tuple methods currently, fn template def can be used before definition, invisible after scope
  - variable def, for type field and function member, var def **cannot** be used before definition, type members maybe visible after scope, local vars is destructed (not deconstructed) after scope
  - template var def, for template type fields and template fn locals
  
// TODO: consider fndef as variable and how handle pure function, captured or binded function and variable name resolve issues
