# fff-lang Designment Document

## type cast syntax

Because I do not like C's type cast syntax `(typeuse)unary_expr`, I abandoned  
it and uses C#'s syntax `postfix_expr as typeuse`, but combined with other  
syntax design there is some maybe confusing things like

    1 as [i32][2]
    42 as (i32, string).item0

which seems like the folloing postfix is following the typeuse not the  
postfix expression, 2 fixes come up directly

    1.as([i32])[2]
    42.as<[i32]>()[2]

the second one is not good because template is not introduced to my language  
at all and requires much change on whole process, also these requires 4 more  
delimeters which seems redundent.

the first one is not good because it adds typeuse between parenthenes, which  
is also confusing because only expression can be in parenthenes except for  
tuple type definition, while this is neither tuple type nor expression.

also the rust's template use can be concerned:

    1.as::<[i32]>()[2]
    1.as::[i32][2]

the double colon is not introduced to my language but it will be introduced  
eventually and maybe quickly, you always need namespace and some kind of  
module design ment to seperate names.

but the first one contains 6 additional delimeters and second one still  
seems kind of strange, the core problem here because there are 2 spaces  
around keyword `as`, which seems seperated, then?

    1.as.[i32][2]
    42.as.(string, i32).item1

still seems seperated, should not require space after typeuse, because space  
and other space characters are ignored since end of lexer, bring them after  
syntax seems strange, "why should there be a space? are you bash?", then

    1.as[i32][2]
    42.as(string, i32).item1
    450.as u64.to_string()

the last case it not good, there must be a parenthenes or bracket or brace  

try c#,

    // CS0039, cannot convert
    new int[] { 1, 2, 3 } as uint[]  

    // CS0270, cannot specify array size. 
    // this is because uint[][2] is parsed as multi dimensional array 
    new int[] { 42 } as uint[][2]    

    // ; expected, member or statement or eof expected
    // compiler is fainted
    new int[] { 42 } as uint[].ToString()

    // ; expected, member or statement or eof expected
    // compiler is continuing fainted
    42 as ulong.ToString()

    // ulong is not reference type or nullable type
    // this is c#'s designment
    (42 as ulong).ToString()

try rust

    struct A();
    struct B();
    #[derive(Clone, Copy)] struct C();

    impl From<C> for A { fn from(_: C) -> A { A() } }
    impl From<C> for B { fn from(_: C) -> B { B() } }

    let c = C();    
    let a = c.into();    // cannot infer type for _
    let b: B = c.into(); // OK

rust is based on From and Into trait and type infer  

a new idea, remove the as operator and typecast expression syntax,  
just recommend using `as_*` member functions to act as typecast method,  
primitive integral type will have something like `as_i32`, `as_u64`,  
char type will have its `from_utf32` and `to_utf32`, etc.  
user custom type will impl their own `as_xxx` tell users to use this

in detail, small integral type will have cast methods into large integral  
type, but there will be no opposite direction methods because I don't like  
redundent codes

Another problem, cast between primitive integral types are `static_cast`,  
currently I will not provide `reinterpret_cast`, I will provide something  
like rust's `std::mem::transmute` something, then what will be  
`dynamic_cast`? or, casting from parent type to sub type. let's remain it  
until I have something like type inheritance and template function.

also, this readability problem just exist at array type and tuple type,  
where I would not allow c# and java's array invariant semantic, so
 
In conclusion, remove the `as typeuse` syntax, because the syntax maybe  
confusing, and the syntax is not of much use.

- `as_*` for static cast between primitive numeric types,  
  consider remove implicit conversion at all
- `std::mem::transmut` for reinterpret cast
- user provide `as_*` for user types
- consider other approaches for dynamic cast
