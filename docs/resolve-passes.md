# fff-lang Designment Document

## semantic analyze design 1

actually it maybe version 3 or 4, see  
driver/codegen/mod.rs before codegen is moved out to semantic

let's see how many passes semantic analyze should use at least

First, (optionally) move syntax tree to semantic tree, current tree is  
exactly same as syntax tree, generate builtin types and fns, which means,  
typedef and even type template def is added nearly added to syntax tree,  
because stdlib in self dependent and already known, maybe add them later

Then, move all strings into symbol collection, intern them, return symbol ID  

Then, iterate through scopes, record definitions, currently 3 kind of  
definitions
  - typedef, currently only allowed in package level, future allowed  
    everywhere, typedef can be used before definition, invisible after scope
  - fndef, in package level or typedef level, fndef can be used before 
    definition, invisible after scope
  - type template def, for array and tuple currently, type template def can  
    be used before definition, invisible after scope
  - fn template def, for array and tuple methods currently, fn template def  
    can be used before definition, invisible after scope
  - variable def, for type field and function member, var def **cannot** be  
    used before definition, type members maybe visible after scope,  
    local vars is destructed (not deconstructed) after scope
  - template var def, for template type fields and template fn locals
  
in this case, fndef still not merged into var, because fn donot consider  
order, but vardef consider order, you cannot use it before definition

discuse normal function def, captured function def (lambda), binded function  
def (binded) and callable object in otherwhere

so (template) type def and (template) function def only records definition  
info, but (template) var def should record definition order to prevent use  
before definition

Then, iterate through tree leaves, analyze all identifiers (or symbols) to  
definition use, also record use to definition info, to provide not used  
warning after it.

In theory, that's finished, provide public interfaces which can only retrieve  
information after last pass, let previous info die in their enum member

