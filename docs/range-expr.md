# fff-lang Designment Document

## Range expression

from v0.1.0 fff-lang's for statement is designed as 

    ForStatement = fFor fIdent fIn Expression fRange Expression

which has a fixed range operator(`:` at that time) in this syntax definition,  
which do not allows iterable objects here and then iterate through array  
have to be written as

    for i in 0..array.length {
        writeln(array[i]);
    }

(0.1.2 fixed dotdot range operator bug and bring in this again)  
which is low readablility and not natural and not convenient

rust has its right exclusive range expression: 
```rust
    ..                          // RangeFull
    ..Expression                // RangeTo(Expression)
    Expression..                // RangeFrom(Expression)
    Expression .. Expression    // Range(Expression, Expression)
```

in new updates I want add this to my language, and setup a iterable and  
iterator infrastructure which allows not only range expression but also array  
and other user defined iterable objects

first versions maybe similiar to C++, which only requires a begin and end  
method in iterable type definition and an increase and a dereference method  
in iterator type definition, plus simple iteration stop control

but, there's some problems, in syntax parser designment

first, according to current expr parser, exprs are divided into 4 classes,  
binary, unary/prefix, postfix and primary, the range to expression seems to  
be a prefix expression, the range from expression seems to be a postfix  
expression, the range expression seems to be binary expression.  
then, what's the priority of the range operator? thinking about some examples  

    1 + 2 .. 3   // yes .. lower then +-*/%
    // range based on Ord, or in my language, < operator
    // and bits type do not have ord, so this case is irrelevant
    4 .. 55 << 6 
    1..2 <= 2..3 // range object do not ord, this case is irrelevant

binary operator version is rather OK, but for range from and range to

    1...start    // what? dotdotdot?
    println("{}", ..); // what? comma dotdot?
    ..!!1         // what? suprise?

they make the designment hard, and I cannot find rust's designment discuss  
because they add this syntax at before 1.0

A conservative solution maybe add std::iter::range functions

    fn range<T>(T start, T end) -> Range<T>;
    fn range_from<T>(T start) -> RangeFrom<T>;
    fn range_to<T>(T end) -> RangeTo<T>;
    fn range_full<T>() -> RangeFull<T>;

this definition have no more problem and are just lib only iterator helper  
functions

And a not so conservative solution

    PrimaryExpr = 
        Lit
        Ident
        Paren
        Array
        Tuple
        RangeExpr

    RangeExpr = 
        fLeftParen [Expr] fRange [Expr] fRightParen
        fLeftBracket Expr fRange [Expr] fRightParen
        fLeftParen [Expr] fRange Expr fRightBracket
        fLeftBracket Expr fRange Expr fRightBracket

which is very match equation like

    (1..3)
    [2..)
    (..8]
    (..)

the most concerning problem maybe, its paren and bracket is not paired, which  
maybe seem strange, and add complexity to future error recovery that assumes  
that parens, brackets and braces are paired, or text editors which always  
assumes parens, brackets and braces are paired

the problems are complicated and may cause huge problems in future, so remain  
it here and uses the conservative solution instead