# fff-lang Designment Document

calculated properties may be very useful in some strange situations

```rust
fn get<T: Add<T>, N: usize>(tuple: (T, T, T)) -> T {
    return tuple[N] + tuple[N - 1];
}

get::<i32, 1>((2, 3, 4)) == 3;
// NOTE that non-single-token generic argument need a paren
// (rust use brace because rust has block expression),
// I use paren because type refs always use angle bracket for quoting, so when meet paren it is expression
get::<i32, (1 + 1)>((2, 3, 4)) == 4;

// ??? const generic parameter, variable generic parameter, subscription type
fn get<N: usize, T...>(tuple: tuple<T>) -> <tuple<T>[N] as Add<tuple<T>[N + 1]>>::Output {
    return tuple[N] + tuple[N + 1];
}

// ??? subscription type, equality where constraint, spread in object expr
fn assign<T, N: string>(s: Struct, value: T) -> Struct where Struct[N] == T {
    return Struct{ [N]: value ...s };
}
```
