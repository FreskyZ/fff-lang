# fff-lang Designment Document

## function overloading

Function parameter type overloading is kind of convenient for similar usage functions, that's the only advantage

It increases complexity of semantic analyze, you can only kown the overloading of a function after all the parameters' types are known, and in addition with primitive numeric type auto size, object oriented or object based issues, things become much more complex

It decrease readability, occassions like
  
```c++
auto c = great_function(5, 10, 15);        // a function takes 3 ints
auto d = great_function("hello", "world"); // what?
```

then the reader have to go to definition**s** to understand what's the meaning, while this is much more clear

```c++
auto c = construct_great_type_with_3_int(5, 10, 15);
auto d = construct_great_type_with_2_str("hello", "world");
```

It is hard for compiler to decide function type when a overloaded function is used as parameter, it is also hard for developer to specify function type when trying to use function as parameter, remember the following 2 syntaxes, first has never supported in fff-lang, second is removed from fff-lang

```fff-lang
iter.filter((fn(&str) -> bool)filt_fn);   // or in C, `iter.filter((bool(*)(&str))&filt_fn)` which is even more ugly
iter.filter(filt_fn as fn(&str) -> bool); // ugly too
```

In conclusion, function parameter type overloading is not and will not be supported in fff-lang
~~, not because rust do not support it and I'm used to it~~
