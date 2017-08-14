# fff-lang Designment Document

## Add block label

from v0.1.0 we support loop name specifier in loop statement:

```ebnf
loop-stmt = 'loop' expr block
```

where `expr` has to be a simple string literal, this feature supports some usage like 

```fff-lang
loop "1" {
    loop "loop2" {
        // ...
        continue "loop2";
        // ...
        break "1";
    }
}
```

which is very sweet at some time

But, the feature is not added to for statement and while statement, because I don't know where to add the loop name specifier

```ebnf
for-stmt = 'for' identifier 'in' expr ':' expr block
                                    ----     ~~~~
while-stmt = 'while' expr expr block
                        ---- ~~~~
```

while statement is similiar, this makes this feature inconsistent, which is not natural and not natural

these solutions makes weird and reduces readability or causes unexpect parsing result

in rust, where the loop name specifier comes from, have

```ebnf
for-stmt = [ lifetime ':' ] 'for' identifier [ ':' typeuse ] 'in' expr block-expr
while-stmt = [ lifetime ':' ] 'while' expr block-expr
loop-stmt = [ lifetime ':' ] 'loop' block-expr
```

It is based on rust's lifetime mechanism, and even if you don't specify it, rustc will automatically emit a lifetime to every block if is needed, and rust has its own special lexer to distinguish character literal and lifetime specifier

but I don't want to change my lexer to support this rather strange kind of lifetime token, and currently I do not have that complex lifetime mechanism

in C style language there are assembly style labels:

```ebnf
identifier ':' statement
```

where the identifier maybe confused with variables (not in assembly codes where every identifier is actually label to code seg or data seg)

gcc supports a very special label array language extension, I don't know what is that double get address going to do, but it generally is understandable, labels are pointers to code segment, they can be regarded as void* and be gotoed

```c++
    void* labels[2] = { &&Label1, &&Label2 }
    Label1: xxx
    Label2: yyy
    goto *labels[0];
```

Currently I won't support raw gotos include this label array, because they are raw, unsafe and difficult to do control flow analyze and optimization

so I decided to add a new label syntax, use a new special char for it, as underscore is reserved for future pattern matching, $ is reserved for future macro, # is reserved for future attribute, ~ is bit not, ` is normally for code quote in many places, then the only left character on standard keyboard is @

in detail, the label lexical token is defined as

    label = @[_@a-zA-Z0-9]*

single @ is supported, double @@ (yes from masm) is supported, I donot like more `@`s like `@@@` and `@@@@@@@`, but I won't forbid it like I didn't forbit single character identifiers, they are too strict and I won't write this style code and I'm lazy to add this kind of check into lexical parser

And changes to syntax design:

```ebnf
label-def = label ':'
loop-stmt = [ label-def ] 'loop' block
while-stmt = [ label-def ] 'while' expr block
for-stmt = [ label-def ] 'for' identifier [ ':' typeuse ] 'in' expr block
block-stmt = [ label-def ] block
break-stmt = 'break' label ';'
continue-stmt = 'continue' label ';'
```

And now break statement are also available in block statement, which acts like C style language's `do { ... } while(0)` pattern which usually lies in macro definition to support break statement in that block

```fff-lang
@@: {
    ...
    break @@;
}
```

is same as 

```c
do {
    ...
    break;
} while (0);
```

also, label arrays or some equavalent maybe introduced in future, which bring in another syntax item that have to be pre resolved like functions, which requires name resolve to be completely refactored and maybe move into syntax parse
