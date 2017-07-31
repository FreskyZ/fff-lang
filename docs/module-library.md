# fff-lang Designment Document

## Module and Library System

As program become larger and more complex, components more than source code file is required

Very simply, a module is a collection of global items, maybe nestable, to make global items logically clearer, 
a library is a collection of global items or collection of modules, and a basic 3rd party feature publish and distribute unit

refs: 
- [Rust document about module](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [C++ module initial paper](http://open-std.org/JTC1/SC22/WG21/docs/papers/2014/n4047.pdf)
- [C++ module paper r2](http://open-std.org/JTC1/SC22/WG21/docs/papers/2014/n4214.pdf)
- [C++ module paper r3](http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2015/n4465.pdf)
- [C++ module paper r4](http://open-std.org/JTC1/SC22/WG21/docs/papers/2016/p0142r0.pdf)
- [C++ module paper newest](http://open-std.org/JTC1/SC22/WG21/docs/papers/2017/n4667.pdf)
- [Clang document about module](https://clang.llvm.org/docs/Modules.html)
- [GCC document about module](https://gcc.gnu.org/wiki/cxx-modules)
- [VC++ blog about module](https://blogs.msdn.microsoft.com/vcblog/2015/12/03/c-modules-in-vs-2015-update-1/)
- [Haskell document about module](https://www.haskell.org/tutorial/modules.html)
- [Rust document about module](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [A blog about ES6 module](http://exploringjs.com/es6/ch_modules.html)

in rust, a module is a file or part of file, module system associates with file system, a module also represents namespaces, privacy can be carefully managed based on that
in haskell, a module is a file or mutliple files, module system not associate with file system
in python, module consists of declarations and statements, a module is a file, module's statements only execute once when first imported in an exection instance, module's global variable is 'module private'
in C#, CLR module is not used and no module concept, there is only types in global scope, files are concat together and then Main is called
in C, no module concept, files are compiled and then linked together, then binary's entry point pointer points to main function start
in ES6, a module is a file, module name associate with file name
in C++, things is too complex to learn, but the dicussion can be applied here

### Purpose

supporting mutliple source code file

### Module system and file system association

a file is a module, associate file name with module name

a module do not map to multiple files, because when you see import xx::yy::zz, you'd like to try to find xx::yy module first and then find zz name next, if a module maps to multiple file, it is troublesome or complex for human or tools to do that, this is the same reason for not mapping single file to multiple modules

associate file name with module name, because it makes thing clear, rust uses this design, 

C#, although no module concept, the best practice is associate namespace hierarchy with file system hierarchy, it also makes thing clear, except that when you open file in some text editor and find the same path both appear in file path and source code hearder, it seems redundency, 

so, associate module name with file name, provide attributes to alias it, not syntax

and, a small preference, I prefer hyphen for file name while module name, which is an identifier, can only use underscore, so, for names like `xx_yy_zz`, I'd like to search file name with `xx-yy-zz.ff` first, then the underscoed version next

and, about the word 'hierarchy', rust's designment is both allowing `xxx.rs` and `xxx/mod.rs` for `mod xxx;` while searching module is in current module's current directory, C#'s design is just merging every source code together and do next things together, as namespace path is written in source code, the actual file location is not so important, I do not like the fix names `mod.rs` and `lib.rs`, they are abbreviation version is one of the reasons, so I'd like try this

```fff-lang
import xxx; // normal search 'xxx.ff' in current module current directory
import xxx::yyy;  // searching 'yyy.ff' in 'xxx' directory in current module current directory
```

that seems like at last I have to set up a best practice like "prefer use xxx/module.ff for module main file and reexport the sub modules in that directory"...
and prefer 'xxx/xxx.ff' for module main file seems redundent, and use python's `__init__.py` is not propert, because module main file is not that semantic

at last, use `module.ff` for module main file......

### Module interface

every name declare in module is public, actually struct's field names are also all public, because I'm lazy to implement them, while module granularity privacy control is very complex

there is a not small neither large issue, when you want to reexport module, when you want to reexport many, you have to write all of the names, because I do not like `*` import, which is tiring, but reexport all is not supported, also because I do not like `*` export at all, so, when you want to reexport, export the module name itself, actually is just an import, or export the names manually, which, currently, requires import at first, then you have already export the module name, but things may change in future days

no special module interface declaration syntax item or module interface file, because I recommend simple source code distribution for libraries, currently

### Module import

use `'import' name` for declare a module import, support directly import sub directory's module, do not support import super directory module, do not support multiple time imported module, only support import declare in package main file or module main file

use `'use' name [ 'as' identifier ]` for name alias, used name conflict with module local name is error, `use some_local_name as another` to alias local name, while also export the name


// TODO: continue clean up documents on module, name and name resolve
// discuss syntax::SyntaxTree and codemap module driver in internal documents
