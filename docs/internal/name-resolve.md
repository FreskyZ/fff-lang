# fff-lang Internal Document

## Name resolve pass

### from `resolve-passes.md` earlier

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

### something new

Before v0.1.2 there is no special name resolve pass in ffc structure, because compiler principle tell me the passes should be lexical parse, syntax parse, semantic analyze and intermediat code generation, optimization and target code generation. Yes semantic analyze and IR generation is one pass and I do impl ffc according to that.

That causes much more complexity and difficulty in designment and implementation, I spent much time figuring out function names, variable names and type definitions, put them in complex structures and impl complex functions to generate them, and at the same time, IR code is generated. Actually var use resolve is along with expression's code generation, the function consumes syntax::expr with identifier string, and IR code with var id is generated

So, after new syntax parser generally finished, new name resolve, or semantic analyze pass should be considered

Name resolve, in one word, means replacing every identifier in raw syntax tree to symbol id. In many words, identifiers and symbol types include, 

   - `PrimaryExpr::Ident` which is local var/argument usage
   - `PrimaryExpr::Ident` and then `PostfixExpr::FunctionCall` which is function call
   - `PostfixExpr::MemberAccess` which is type field
   - `PostfixExpr::MemberFunctionCall` which is member function call
   - `TypeUse::Simple` which is type usage
   - `LabelDef` which is label definition
   - `ContinueStatement` and `BreakStatement`'s label usage

local var/argument usage and global function call is the easist, you walk through syntax tree and get global function definitions, and then replace the global function usages. You walk through statements in a function, record all local var definitions and at the same time replace identifier usage to local var id.

Then things become complicated

- how to resolve member access, then you have to know the type of a variable, or more precisely, type of an expression
- then, according to other concerns, array type or at least pointer type is necessary, any of them is type template, do I need a type template in  semantic analyze or in syntax tree? 
- if in syntax tree, then there should be something like std.ff, then primitive types should be tagged with builtin attribute, then attribute syntax and semantic?
- if type template, then its member function is also function template, then function template?
- if type template, then non-type template argument?

Another set of problems in implementation, current syntax tree is an immutable and cannot decontruction tree, then how to add resolved information to it, several solutions

- Any identifier is actually IdentOrID, which requires muttable tree
- Deconstruct and move into new structure, which requires new physical structure
- (There was some more considerations but I forgot)

The latter is selected and current syntax tree nodes' `get_*` impls will be moved into semantic tree and fields will be public

More detail concerns

(After a lot of concern which I forget, too)

Parent node use shared pointer to manage child nodes, child nodes use weak ref to immutablly borrow paren nodes to get information, if operation on child nodes needs to modify parent node, then the operation is performed by parent node and he get information from child nodes and update it self, if  the operation only changes child nodes, then let them do it themselves
