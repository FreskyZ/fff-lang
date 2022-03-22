
current enum is simple namespaced constant collection

```
enum E: u8 {
    M1 = 0,
    M2 = 1,
    M3 = 2,
}
```

while tagged union is required for, for example, syntax tree expression types,
this small one should be easy to implement and kind of usable

```
enum type E {
    T1{ f1: i32, f2: &u8 },
    T2{ f1: OtherType, f3: AnotherType },
}

fn main() {
    var e = get_e();

    switch e {
        E::T1 v => { writeln(f"f1 {v.f1} f2 {v.f2}"); }
        E::T2 v => { return v.f1 + v.f3; }
    }
}
```

use `switch` not `match` because it is invenient that regex is forced to use `Captures` for capture result,
while using `switch`, same as C, C++, Java and C#, does not collide with the network device or gaming console that often

// I cannot wait to see how crazy will randinput.py generate to test enum type memory layout optimization!
