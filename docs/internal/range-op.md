# fff-lang Internal Document

## Range operator

range operator is much more complex then expected

it starts from things like

```rust
for i in 0..5 {
    do_things(i);
}
let c = &bytes[some_start..some_end];
do_something(c);
```

and 

```py
for i in range(0, 5):
    do_things(5)

c = l[1:]
do_something(c)
```

rust says, it is left inclusive right exclusive range operator, 4 usages:

```rust
let a = [1, 2, 3, 4];
let b = &a[..];       // b = &[1, 2, 3, 4],  range_full = '..'
let c = &a[1..];      // b = &[2, 3, 4],     range_left_bound = expr '..'
let d = &a[..2];      // b = &[1, 2],        range_right_bound = '..' expr
let e = &a[1..2];     // b = &[2],           range_binary = expr '..' expr
```

but no document tells me the priority, while the source code is also very complex to understand,
so a feas test
```rust
let a = ..;
let b = ..1; 
let c = 1..;
let d = 1..2;

let e = 1 + 1 .. 2 * 2;  // 2..4, lower then multiplicative and additive
let f = 1 << 3 .. if 4 < 5 { 3 } else { 2 }; // 8..3, lower then shift and if expr
// lower then struct init, range on even no Ord and Eq
let g = NoDerive{ a: 1 } .. NoDerive{ a: -8 }; 
let h = false..true;  // yes bool allowed
let i = 1 == 2.. 3 == 4; // false..false, lower then ==
let j = 1 | 2 .. 5 ^ 10;  // lower the bit operator
let k = [1, 2][1]..; // same as other postfix
let l = ..!5; // same as other prefix
let m = 1 + 2..; // 3.., postfix but lower then binary
let n = ..3 * 5; // ..15, prefix but lower then binary
```

so that is a `struct RangeFullOrRight` which expect '..' as first final, and expects BinaryExpr after '..', 
if not, then is a range full, if is binary expr's first final, then is a right bounded range expr, then,
a final `struct RangeExpr` after binary's parser in expr's parser, which checks '..' after binary expr,
and if is that, expect binary expr after '..', if not , is a left bounded, if is, is a binary range, and,
add Expr's enum member:

  - `RangeFull(RangeFull{ span: Span })`
  - `RangeLeft(RangeLeftBound{ left: Expr, all_span: Span })`
  - `RangeRight(RangeRightBound{ right: Expr, all_span: Span })`
  - `RnageBinary(RangeBinary{ left: Expr, right: Expr, range_span: Span, all_span: Span })`

That should be OK, discuss more problem after tests