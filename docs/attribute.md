# fff-lang Designment Document

## Attribute

### Menu:

- C# attribute
- Python decorator
- Java annotation
- rust attribute
- gcc attribute
- msvc declspec
- C++ attribute

### C# Attribute

C# allows basic declarative information include `protected`, `public`,  
`internal`, `private`, etc. C# also allows custom declarative information  
by attribute

Entities include assembly, class, delegate(which is class), enum, event,  
field, generic parameter, interface, method, module, parameter, property,  
return value and struct can have attribute, they are added to the entity's  
metadata by compiler and can be retrieved at runtime by reflection

Attributes are declared by providing attribute name, positional parameters  
and named parameters, attribute name is attribute class's name, positional  
parameters are attribute class's constructor parameter, named parameters  
are attribute class's public settable property

Syntax:

    GlobalAttributeSection = 
        fLeftBracket GlobalTarget AttributeList fRightBracket
    GlobalTarget = fAssembly fColon | fModule fColon
    AttributeSection = 
        fLeftBracket [AttributeTarget] AttributeList fRightBracket
    AttributeTarget = 
        [fField | fEvent | fMethod | fParam | fProperty | fReturn | fType]
        fColon
    AttributeList = Attribute [fComma Attribute]* [fComma]
    Attribute = 
        fIdentifier [ fLeftParen [Positionals] [Nameds] fRightParen ]
    Positionals = 
        Expr [fComma Expr]
    Nameds = 
        fIdentifier fEqual Expr [ fComma fIdentifier fEqual Expr ]

std attributes

  - `Conditional` attribute is replacement of C's #ifdef, which only allows  
    conditional compilation variables but not preprocessor exprs
  - `CallerLineNumber` attribute is \_\_LINE__  
  - `CallerFilePath` attribute is \_\_FILE__  
  - `CallerMemberName` attribute is \_\_FUNC__
  - `DllImport`, `MarshalAs`, etc. attribute to specify C style import
  - `COMInterface`, `Guid`, etc. attributes to support COM interop
  - `DebuggerWalkThrough` etc. attributes to interop with debugger
  - `Flags` attribute to implement `System.Enum` language feature
  - `CLSCompilant`, `IndexerName` to support CLS compliance issues

C# attribute is very useful and powerful

### Python decorator

Python decorators enable developers to modify function funtionality at  
runtime, decorators are actually simplly a function receive a function as  
parameter and return another function, e.g.

```py
def my_dec(f):
    def wrapper(name):   # *args and **kwargs are more practical
        return f"<p>{f(name)}</p>";
    return wrapper;

@my_dec
def get_text(name):
    return f"lorem ipsum, {name} dolor sit amet";

# which desugars to 
def __get_text(name):
    return ...
get_text = my_dec(__get_text)

# and decorators with syntax
def surround_by(tag):
    def wrapper_f(f):
        def wrapper_param(*args, **kwargs):
            return f"<{tag}>{f(*args, **args)}</{tag}>";
        return wrapper_param;
    return wrapper_f;

@surround_by('div')
def get_text(name):
    return "42" + name;

# desugars to
def __get_text(name):
    return ...
get_text = surround_by('div')(__get_text)
```

Python decorator is very useful and powerful

### Java annotation

Java annotation is a form of metadata, which may provide information for  
compiler, deployment and runtime, e.g.

```java
@Override
void my_method() {}

@SuppressWarnings(value = "unchecked")
void your_method() {}

new @Interned SomeType();
(@NonNull String)str;
class ReadonlyList<T> implements @ReadOnly List<@ReadOnly T> {}
void some_method() throws @Critical SeriousException {}

// `at` is used in annotation because @ = AT = Annotation Type
```

custom annotations:

```java
@Documented  // to appear in javadoc
@interface ClassMiscInfo {
    String author();
    String date();
    int currentVersion() default 1;
    String lastModified() default "N/A";
    String[] reviewers();
}

@ClassMiscInfo(
    author = "Fresky",
    date = "5/13/2016",
    lastModified = "5/13/2017",
    reviewers = { "r1", "r2", "r3" }
)
public class SuperGreatType {}
```

Java annotation is useful

### Rust attribute

According to documents, rust's attribute modeled on C#.

rust attribute can be applied to all kinds of items, include typedef, fndef,  
trait def, use decl and module declares, also include crate

syntax:

    Attribute = fAttribute [fNot] fLeftBracket MetaItem fRightBracket
    MetaItem = fIdent
               | fIdent fAssign fLiteral
               | fIdent fLeftParen MetaItemInner*
    MetaItemInner = [ MetaItem | fLiteral ] [ fComma [ MetaItem | fLiteral ] ]*

which is very complex and look through source code to get them,  
valid sentences include

    #[attr]
    #![attr]
    #[attr = 5]
    #[attr(true)]
    #[attr(identifier)]
    #[attr(value = 'a')]
    #[attr(value = 'a', 42)]
    #[attr(another(42, that_value = "hello"), 42, value = true)]

that is, meta item is either pure identifier, or identifier and identifier  
and its lit value pair or looks like function with at least one parameter,  
each parameter is meta item

it is very complex compare to C#'s syntax, which actually desugars to an  
attribute class instance construction and property setting operation.

rust only supports builtin attributes, commonly used ones include

    #![crate_name = "some_great_library"]
    #![crate_type = "dylib"]
    #![recursion_limit = "256"]
    
    #[path = "foo.rs"] mod bar;
    #[test] fn test_1() {}
    #[repr(C)] struct WINDOWCLASSEX { ... }
    #[macro_use] mod macros;
    #[macro_Use] extern crate utils;
    #[macro_export] macro_rules! great_macro { }

    #[cfg(all(unix, target_pointer_width = "32"))]
    #[cfg_attr(test, derive(Eq, PartialEq))]

    #[allow(unused_variables)]
    #[deny(missing_docs)]

    #[inline]
    #[derive(Debug, Display, Eq, PartialEq, Copy, Clone, Hash)]

no custom attribute feature provided

rust's attribute is very useful

### GCC/clang attribute

GCC's attribute can be applied to functions, variables and types, common  
ones include

```C++
struct SomeType {
    int a;
    char b;
} __attribute__((packed));
static_assert(sizeof(SomeType) == 5);

void __attribute__((noreturn)) panic() { 
    TerminateProcess(GetCurrentProcess()); 
}

void __f() { /* magic */ }
void f() __attribute__((weak, alias("__f"))); // weak symbol

void freq_called_fn() __attribute__((always_inline)) {}
void want_debug() __attribute__((noinline)) {}
```

which is not very useful currently, some are obesolete, some are replaced by  
language feature, some is auto judged by compiler

### MSVC declspec

in MSVC you see everywhere

```C++
extern "C" __declspec(dllimport) uint32_t __stdcall GetCurrentProcessId();
extern "C" __declspec(dllexport) MyGreatStruct MyGreateFunction();

struct __declspec(novtable) __declspec(uuid("a long guid")) IGreatInterface {
    HRESULT __stdcall Create(*MyGreatStructure);
}

__declspec(align(1)) struct SlowStruct { char a; char b; };
```

also, `noreturn`, `noinline`, some kind of thread local is provided

although they appear frequently in MSVC code, developers donot usually use them

### C++ attributes

As C++ become moderner, it includes its own attribute feature, but not cover  
much circustances and not widely used

```C++
[[noreturn]] void abort() { ... }

switch (...)
{
    case CASE1:
        xxx;
        [[fallthrough]]
    case CASE2:
        yyy;
        break;
    default: throw "error";
}

int __stdcall WinMain([[maybe_unused]] HINSTANCE, [[maybe_unused]HINSTANCE]);
```

TODO: think about mine