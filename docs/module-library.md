# fff-lang Designment Document

## Module and Library System

As program become larger and more complex, components more than source code file is required

Very simply, a module is a collection of global items, maybe nestable, to make global items logically clearer, 
a library is a collection of global items or collection of modules, and a basic 3rd party feature publish and distribute unit

refs: 
- [C++ module initial paper](http://open-std.org/JTC1/SC22/WG21/docs/papers/2014/n4047.pdf)
- [C++ module paper r3](http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2015/n4465.pdf)
- [C++ module paper r4](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/n4610.pdf)
- [Clang document about module](https://clang.llvm.org/docs/Modules.html)
- [GCC document about module](https://gcc.gnu.org/wiki/cxx-modules)
- [VC++ blog about module](https://blogs.msdn.microsoft.com/vcblog/2015/12/03/c-modules-in-vs-2015-update-1/)
- [Haskell document about module](https://www.haskell.org/tutorial/modules.html)
- [Rust document about module](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)

in rust, a module is a file or part of file, module system associates with file system, a module also represents namespaces, privacy can be carefully managed based on that
in haskell, a module is a file or mutliple files, module system not associate with file system
in python, module consists of declarations and statements, a module is a file, module's statements only execute once when first imported in an exection instance, module's global variable is 'module private'
in C#, CLR module is not used and no module concept, there is only types in global scope, files are concat together and then Main is called
in C, no module concept, files are compiled and then linked together, then binary's entry point pointer points to main function start
in C++, things is too complex to learn