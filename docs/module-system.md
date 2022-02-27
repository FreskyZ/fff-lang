# fff-lang Designment Document

## Module System

As program become larger and more complex, module system is required to manage various program components.

### Prior Art

- [Rust document about module](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [VC++ module](https://docs.microsoft.com/en-us/cpp/cpp/modules-cpp?view=msvc-170)
- [Clang document about module](https://clang.llvm.org/docs/Modules.html)
- [GCC document about module](https://gcc.gnu.org/wiki/cxx-modules)
- [Haskell document about module](https://www.haskell.org/tutorial/modules.html)
- [Rust document about module](https://doc.rust-lang.org/book/second-edition/ch07-00-modules.html)
- [Python document about module](https://docs.python.org/3/tutorial/modules.html)
- [A blog about ES6 module](http://exploringjs.com/es6/ch_modules.html)

- in rust, a module is a file or part of file, module system associates with file system, a module also represents namespaces, privacy can be carefully managed based on that
  // update 2022: rust have updated module system in 2018, it is more easy to use now
- in haskell, a module is a file or mutliple files, module system not associate with file system
- in python, module consists of declarations and statements, a module is a file, 
  module's statements only execute once when first imported in an exection instance, module's global variable is 'module private'
- in C#, CLR module is not used and no module concept, there is only types in global scope, files are concat together and then Main is called
- in C, no module concept, files are compiled and then linked together, then binary's entry point pointer points to main function start
- in javascript ES6, a module is a file, module name associate with file name
- in C++, things is too complex to learn, but the dicussion can be applied here // update 2022: c++ module is added in 2020

### Detailed Design

A file is a module, a module contains some items (syntax item like function definition, 
global variable or module definition and name alias), and a program or a library is represented by the entry module.

A module is declared by `module module_name;` item,
logically, compiler will replace module declaration with module content, with every item prepended module path as namespace, recursively, e.g.

```
// src/main.f3
module module1;
module module2;

fn main() {}

// src/module1.f3
fn m1() {}

// src/module2/index.f3
module module2_2;
fn m2() {}

// src/module2/module2-2.f3
fn m2_2() {}
```

is logically

```
fn module1::m1() {}
fn module2_2::m2_2() {}
fn module2::m2() {}
fn main() {}
```

module `module_name` is resolved in order
- for other module src/other-module.f3
  - src/other-module/module-name.f3
  - src/other-module/module-name/index.f3
  - src/other-module/module_name.f3
  - src/other-module/module_name/index.f3
  - src/other_module/module-name.f3
  - src/other_module/module-name/index.f3
  - src/other_module/module_name.f3
  - src/other_module/module_name/index.f3
- for other module src/other-module/index.f3
  - src/other-module/module-name.f3
  - src/other-module/module-name/index.f3
  - src/other-module/module_name.f3
  - src/other-module/module_name/index.f3
- entry module, even not called index.f3, also resolves like index.f3

> - allow hyphen because I personally prefer hyphen anywhere possible
> - use index.f3 not mod.f3 because mod is too short and may be confused with modify/moderate etc.
> - use index.f3 not module.f3 because module is too long (6 character is really much longer than 5 characters)

### Visibility
No, currently there is none, rust 2018's visibility control is powerful and comfortable
while making less powerful visibility control mechanism not comfortable to use and same powerful mechanism hard to implement,
so there is none currently.

### Name alias
in this style module system, an `use` item is simply a name alias for
- `name` in `use namespace1::namespace2::name;`, or
- `aliased_name` in use `namespace1::namespace2::name as aliased_name;`

although all items are publicly available accross program, name alias only works in current module
