# fff-lang Designment Document

## Name, Compile Unit, Name Resolve and Name Alias Issues

### C

C has roughly 4 kinds of name, function name, global variable name, type name and field name, the former 2 is physical, they are actually pointer value to code segment and data segment, the latter 2 is virtual, a type name is actually a collection of field names, a field name is an offset value, compiler translate the virtual names into physical names finally

C's compile unit is every source code file, a source code file is a collection of names, and specially for function names, its implementation, every name can only be used after declared, but the compiler do not care about name implementation at all while linker is responsible for this. Every module is parallel and not hierarchical, so global initializer do not have order. You can declare a private name with `static` storage specifier

C's name resolve procedure is simple, you can only use a name after declared, and any not implemented name is an unresolve name in object file, after that linker will try find the name in other modules and generate errors for search failure

C do not have name alias, if not consider macors, which is very annoying, especially Windows's C API, it has to place anything in global namespace and thus polutes IDE intellisense result, and with C's unscoped enum type design, every enum member have a prefix of abbreviation of the type name, which is more unconvenient and ugly

### C++

C++'s name resolve and compile unit design is similiar to C, but with much more complex function templates, type templates and object oriented type definitions

C++ has namespace concept, all names declared in namespace add a namespace prefix by compiler, and `using namespace xxx` semanticly aliases all names visible in the namespace to without namespace prefix version

C++ has unnamed namespace is designed to replace the old `static` storage specifier, a type can declare some of its members to private, a friend function or type declare can configure detailed internal accessibility to same program other function or types

### C#

C#'s name design is similiar to C, difference is that global scope has only types, and type has members like field, method, property, events, etc. 

C#'s compile unit is assembly, every file in the project will be merged together and types can be accessed before declaration, which makes things very easy to use

C# has namesapce concept, all types declared in namespace add a namespace prefix by compiler, and `using xxx` add a name lookup source when resolving type names in source code, after successfully resolved, full type names are used in MSIL code. C#'s namespace concept is irrelavent to program physical structure, but normally it is impled as folder as namespace segments and file names as type names.

C# type members can have difference accessibility include private, assembly internal, protected, protected and assembly internal and public

### rust

rust has hierarchical module concept, which helps manage program semantic structure based on program's file system structure. compiler merges all files based on module definitions and logically compile one file.

rust has complex access control based on module structure, recently added pub level feature fulfills more requirements

### python

in python everything is object, name is object, module is object, get member of module is actually get attr of module, a module is simply a file or a `__init__.py` in the same name directory

### fff-lang

hierarchical module seems very convenient, powerful and interesting, I'd like to use it, and keyword selection, `alias` is ugly, `mod` seems like the `%` operator, so use `import` to declare module import, use `use` for name alias, import auto use the module name

add to syntax

```ebnf
statement = ...
          | 'use' name [ 'as' identifier ] ';'
          | 'import' name [ 'as' identifier ] ';'    // local import is strange, deny it
```

leave them all public currently, discuss access control after former is implemented

another issue, about syntax driver implementation, there have to be some where holding the logic that recursively check current module's import declarations, according to import name to determine possible file to be parsed, check file existence, open and read file, lexical parse it and syntax parse it, record it somewhere and continue recursion

in detail, first there is only one input file, say `main.ff`, give it to `SourceMap`, produce a `SourceCode`, call lexical module driver, get a `TokenStream`, construct a `ParseSession`, give it to `Module`'s parser, get a generated `Module`, then check its imports, say `import a;` and `import c;`, then search the same dir for `./a.ff` or `./a/module.ff` and `./c.ff` or `./c/module.ff`, recursion

so, current `codemap::SourceCode` and `syntax::Module` should add some fields and allow some kind of mutable, source code should calculate current directory when constructing, module should add a file id to link to the source code, or Rc the source code, module also should contain information as module name, which add a segment to its defined names, here is a new issue, what happens to the names when

```fff-lang
// a.ff, main module
import b;
int a;    // full name is 'a'

// b.ff
import c;
int b;    // full name is 'b::b'

// c.ff
int c;    // full name is 'b::c::c' or 'c::c' ?
```

rust's solution is forbid module declare in non (main.rs, lib.rs, mod.rs) files, this seems great and every thing seems like rust...

let's learn more about C++ ts and haskell and go and javascript's module design

in python, physical structure with

\something
  main.py
  m1.py
  m2.py
  \m3
    m4.py

and content

```py
from m1 import M1Type;
from m2 import M2Type;
from m3.m4 import M34Type;
```

it works fine

check for information about haskell
also consider learning about COM's design