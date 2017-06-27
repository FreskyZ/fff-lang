# fff-lang Designment Document

## What will semantic analyze do

src:
```rust
fn main() {
    var a = 0;
    writeln("a = " + a.to_string());
    a = 42;
    writeln("now a = " + a.to_string());
}
```

Syntax Tree:
```
Package 
  fn #1 <0-117>                     // this #1 is fn id, package unique
    Name #1 <3-6>                   // this #1 is symbol id, package unique
    Params (empty)
    Body <10-117>
      stmt #1 VarDecl var <16-25>   // this #1 is stmt id, package unique
        #2 <20-20>
        Literal (i32)1 <24-24>
      stmt #2 ExprStmt simple <31-62>
        FnCall <31-61>
          Ident #3 <31-37>
          paren <38-61>
          BinaryExpr <39-60>
            Literal #4 <39-44>
            add <46-46>
            FnCall <48-60>
              MemberAccess <48-58>
                Ident #2 <48-48>
                dot <49-49>
                Ident #5 <50-58>
              paren <59-60>
              Params (empty)
      stmt #3 ExprStmt assign <68-74>
        Ident #2 <68-68>
        assign <70-70>
        Literal (i32)42 <72-73>
      stmt #4 ExprStmt simple <80-115>
        FnCall <80-114>
          Ident #3 <80-86>
          paren <87-114>
          BinaryExpr <88-113>
            Literal #4 <88-97>
            add <99-99>
            FnCall <101-113>
              MemberAccess <101-111>
                Ident #2 <101-101>
                dot <102-102>
                Ident #5 <103-111>
              paren <112-113>
              Params (empty)
```

AST should be
principles: 
    all compile errors ends here
    should have full information that a compile time reflector want
    simple format: only display IDs, full format: provide preview for id
    try your best to make it can convert back to source code, update: only provide span info when message required
```
// simple ; full
package
  type-def#1 <x>              // a scope has definitions like typedef, typedef has package unique id,
    name sym#1 <x>     ; unit //     although typedef is in scope, it is actually package global to help merge same definitions in different scope 
    var-def#1 <x>  
      name sym#2 <x>   ; 'operator=='
      type type#2 <x>  ; 'fn<bool, unit, unit>' //  fn(unit, unit) -> bool is identical to fn<bool, unit, unit>
      body (builtin)                            // fn type has body
    var-def#2 <x>
      name sym#3 <x>   ; 'operator!='
      type type#2 <x>  ; type-template#2 by type#2, type#1, type#1, 'fn<bool, unit, unit>'
      body (builtin)
  type-def#2 <x>
    name sym#4 <x>     ; bool
    var-def#1 <x> 
      name sym#5 <x>   ; 'operator negative'     // for this case use 'operator negative' and 'operator sub' to distinguish
      type type#3 <x>  ; type-template#2 by type#2, typ#2, 'fn<bool, bool>
      body (builtin)
    var-def#2 <x>
      name sym#2 <x>   ; 'operator equal'        // consider allowing both `operator==` and `operator equal`,
      type type#4 <x>  ; 'fn<bool, bool, bool>'
    var-def#3 <x>
      name sym#3 <x>   ; 'operator!='            // and 'negtive', 'add', 'sub', etc. is sepcial identifier not keyword, when used
      type type#4 <x>  ; 'fn<bool, bool, bool>'
      body (builtin)
  type-def#5 <x>                      // sample integral type
    name sym#6 <x>     ; 'i32' 
    var-def#1 <x>
      name sym#7 <x>   ; 'operator add'          //     here, it is operator overloading name, when not
      type type#6 <x>  ; 'fn<i32, i32, i32>'
      body (builtin)
    var-def#2 <x>
      name sym#8       ; 'operator neg'
      type type#6 <x>  ; 'fn<i32, i32>
    ...
  type-template-def#1 <x>                       // ref
    name sym#9 <x>     ; 'ref'
    type-param-def#1 sym#10        ; 'T'
    type-def#8 <x>
      instantiated type-template#1 by type-param#1  ; 'ref<T>'  // this means, member fn's param typeuse resolve after type decl
    var-def#1 <x>
      name sym#2 <x>   ; 'operator eqaul'   // actually when T: Eq, but not currently
      type type#7      ; type-template#2 by type#2, type#8, type#8, 'fn<bool, ref<T>, ref<T>>'
    var-def#2 <x>
      name sym#13 <x>  ; 'operator shift left'   // also 'operator shl', 'operator<<'
      type type#8      ; type-template#2 by type-param#1, type-param#1, type#5, 'fn<T, T, i32>'
  // type-template-def#2 <x>                       // fn type is too basic and not explicitly defined
    // name sym#12        ; 'fn'
    // type-param-def#1 sym#14 <x>    ; 'TReturn'
    // type-param-pack-def#1 sym#15   ; 'TParams'  
  type-template-def#3 <x>                 // Then how to manage var-def with type with T instantiated?
    name sym#16 <x>       ; 'array'
    type-param-def#1 sym#17 <x> ; 'T'
    partial-type-def#10 <x>               // partial type def act the same as type def but should be instantiated after all instantiated,
      intantiated type-template#1 by type-param#1  ; 'ref<T>'  
    var-def#1 <x>
      name sym#18 <x>      ; 'data'
      type type#10      ; 'ref<T>'
    var-def#2 <x>
      name sym#19 <x>      ; 'size'
      type type#8       ; 'u64'
    var-def#3 <x>
      name sym#20 <x>      ; 'capacity'
      type type#8       ; 'u64'
    var-def#4 <x>
      name sym#21 <x>      ; 'new'
      type type#12      ; 'fn<array<T>>
      body
        stmt-def#1 VarDecl var <x>  // emit 'var this: array<T> = ptr<T>::new(reflect::sizeof(array<T>))' for constructor
          name sym#22   ; 'this'    // which require a '#[constructor]' and static reflection, because type size is not known
          type type#10  ; 'array<T>'
          init by
            FnCall <x>     // 
              Name <x>
                
  
  ...
  fn-def#200 writeln internal <x>
    name sym#10
    return #1
  var-def#1
    name#123 'main'
    type#456 type-template#5 by type#1, type#2, type#3
  fn-def #300 main <<0>0-117>
    name #x <<0>3-6>
    params (empty)
    return #1 <x>
    var-def a i32
    body 
```

steps:

  1. Copy all content, when child(ren) is node, convert to semantic::xxxnode, 
     which actually is semantic::xxxnode{ node: syntax::xxxnode } currently, add more fields in future if need
     discard all span information, add them back in `new(syntax::xxxnode)` when error message requires them