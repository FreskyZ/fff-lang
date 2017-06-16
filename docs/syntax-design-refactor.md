# fff-lang Designment Document

## Syntax designment refactor #1

### Remove array duplicate definition syntax

Because it makes parsing kind of complex:

```ebnf
array_def = '[' [ expr_list ] ']'
array_def2 = '[' expr ';' expr ']'
tuple_def = '(' [ expr_list ] ')'
function_call = name '(' [ expr_list ] ')'
```

when trying to merge these 3 `expr_list` usage, the array_def2 makes it hard to implement, 
so this syntax is abandoned and use a new semantic mechanism to replace:

    // one of this, to be determined
    array<T>::operator*(this, m: i32) -> array<T>; // where T: Copy
    array<T>::duplicate(this, m: i32) -> array<T>; // where T: Copy

e.g.

    var empty_strings = [""] * 10;
    var zeros = [0].duplicate(10);

### Remove member function call postfix

the Postfix::MemberFunctionCall is to be removed to make syntax tree and parsing process more simple

these means that member function handling is more complex and powerful, mainly new feature is

```rust
    var a = "123";
    // type a is &fn(string);
    // which should be implemented as a binded function like
    // struct ?binded?string::len {
    //     var self: string;
    //     fn operator()(this) -> u64 {
    //         return string::len(this.self);
    //     }
    // }
    const len = a.len;    
    assert_eq(len(a), 3);
    a.push('4');
    assert_eq(len(a), 4);
```