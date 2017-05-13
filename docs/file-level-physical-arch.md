# fff-lang Designment Document

## File level phsical architecture

C language do not have namespace concept, which is very annoying,  

  - Windows.h places anything in global namespace and polutes IDE  
    intellisense result, and all members in 'unscoped enum type' have a  
    prefix of abbreviation of the type name, which is very ugly
  - linux kernel declares most functions and global variables to be  
    static to avoid collision with other file

C++ has namespace concept, all names declared in namespace add a  
namespace prefix by compiler, and `using namespace xxx` logically aliases  
all names visible in the namespace to without namespace prefix version.  
After that the names with many double colons are quickly mangled to 
complex names with many non identifier char...

C# has namesapce concept, all types declared in namespace add a namespace  
prefix by compiler, and `using xxx` add a name lookup source when resolving  
type names in source code, after successfully resolved, full type names are  
used in MSIL code. C#'s namespace concept is irrelavent to program physical  
structure, but normally it is impled as folder as namespace segments and  
file names as type names. 

rust has module concept, which helps manage program logical structure based  
on program's file system structure. compiler merges all files based on 
module definitions and logically compile one file.

C language do not have access level control, visible is usable, unvisible  
but declared is also usable.

C++ has simple access level control on class members, the storage specifier  
is added to member function signature and then mangled in vc++ impl

C# has simple access level control on types and type members, private,  
internal, public and protected, internal is applied to assembly not namespace  

rust has complex access level control based on module architecture, access  
level of types, members and functions can be carefully declared to this  
module only or a special level of parent module

Now what kind of namespace or module architecutre will I choose?

rust's module arch is too complex and I read through rust's name resolve  
designment in rfcs and still can not understand the process, then C#'s  
namespace architecture is selected

then, new syntax about namespace definition

    Package = [Item]*
    Item = FnDef | TypeDef | NamespaceDef
    NamespaceDef = fNamespace Name fLeftBrace [Item]* fRightBrace
    Name = Identifier [fNamespaceSep Identifier]*

and namespace use, several selections

    using namespace xxx;            // C++
    use namespace xxx;              // no
    using xxx;                      // C#
    using yyy = very::long::ns;     // C# namespace alias
    use xxx;                        // rust, use module
    use some_ns::some_type;         // rust, use type
    use some_ns::some_fn;           // rust, use fn
    use some_ns::{ some_type, some_fn };    // rust, use multiple
    import some_ns;                 // python, java
    import some_ns::some_type;      // no
    from some_ns import some_type   // python
    from some_ns import some_type, some_fn  // python, import multiple

here, C++, C# and java are actually `use xxx::*`, which I donot like, so  
full name is required, and about name alias

`use xxx::yyy::some_fn` is actually alias it to `some_fn`, then there  
should be explicit alias, 

    use xxx::yyy::some_fn as some_other_name;   // rust, python
    use some_other_name = xxx::yyy::some_fn;    // C#

C#'s version makes the name alias looks like some assignment statement,  
and use it in VS you see intellisense warnings on this cannot find this name.  
so rust's version is selected and `as` keyword comes back. syntax:

    Item = UseDecl | FnDef | TypeDef | NamespaceDef
    Block = UseDecl | ...other statements
    UseDecl = fUse Name [fAs fIdentifier] fSemiColon

namespace feature is basic feature not core feature and will not be provided  
currently

TODO: Access Level