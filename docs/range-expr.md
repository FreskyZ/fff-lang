# fff-lang Designment Document

## Range expression

the previous for statement syntax uses a fixed range operator:

    for_stmt = 'for' identifier 'in' expr ':' expr block

which do not allow iterable objects here include builtin array type, then iteration through an array has to be written as

```fff
for i in 0 : array.length {
    writeln(array[i]);
}
```

this limits features about iterators and later generators and coroutines

rust has its right exclusive range expression:

```rust
let a = [1, 2, 3, 4];
let b = &a[..];       // b = &[1, 2, 3, 4],  range_full = '..'
let c = &a[1..];      // b = &[2, 3, 4],     range_left_bound = expr '..'
let d = &a[..2];      // b = &[1, 2],        range_right_bound = '..' expr
let e = &a[1..2];     // b = &[2],           range = expr '..' expr
```

a few test cases show its detail design:

```rust
// 2..4, lower than additive operator
let e = 1 + 1 .. 2 * 2; 
// 8..3, lower than shift operator and if expr
let f = 1 << 3 .. if 4 < 5 { 3 } else { 2 };
// lower then struct init, not require Ord and Eq
let g = NoDerive{ a: 1 } .. NoDerive{ a: -8 };
// bool allowed, what's the usage?
let h = false..true;
// false..false, lower than equality operators
let i = 1 == 2.. 3 == 4;
// 3..15, lower than bit operators
let j = 1 | 2 .. 5 ^ 10;
// 1.., postfix version's priority is normal
let k = [1, 2][1]..;
// ..-6, prefix version's priority is normal
let l = ..!5;
// 3.., although postifx like, but priority is lower than binary
let m = 1 + 2..;
// ..15, although prefix like, but priority is lower than binary
let n = ..3 * 5;
```

these cases show that the syntax is very complex

solution 1, use several range* functions like python:

```ff
fn range<T>(T start, T end) -> Range<T>;
fn range_from<T>(T start) -> RangeFrom<T>;
fn range_to<T>(T end) -> RangeTo<T>;
fn range_full<T>() -> RangeFull<T>;
```

this is simple and easy and pythonic

solution 2, a very math solution,

```ebnf
range_expr =
    '(' [ expr ] '..' [ expr ] ')'
    | '[' expr '..' [ expr ] ')'
    | '(' [ expr ] '.. expr ']'
    | '[' expr '..' expr ']'
```

this seem strange, add complexity to future error recovery which assumes that parens, brackets and braces are paired, confuses text editor lexical parsers and language servers which also assumes parens, brackets and braces are paired

solution 3, three months after previous two solutions where syntax/expr is refactored stronger and clearer, use a `RangeFull` struct expecting `..` as first final, and expects optional binary expr after that, and a `RangeLeft` struct calls binary expr parse method at first, then expecing optional `..`, if exists, then expecting optional binary expr

add to `enum Expr`:

- `RangeFull(RangeFull{ span: Span })`
- `RangeLeft(RangeLeftBound{ left: Expr, all_span: Span })`
- `RangeRight(RangeRightBound{ right: Expr, all_span: Span })`
- `RnageBinary(RangeBinary{ left: Expr, right: Expr, range_span: Span, all_span: Span })`

that's how the parsers expect to work