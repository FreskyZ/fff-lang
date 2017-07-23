# fff-lang Designment Document

## static reflection

recent develop process shows at least 1 situation requiring static reflection

first, you have

```fff-lang
struct GreatTraitContext {
    great_prop: usize,
    great_member: Point,
}
trait GreatTrait {
    fn great_fn(great: GreatTraitContext) -> GreatResult;
}
```

then you can

```fff-lang
struct GreatType {
    great_member: AnotherGreatType,
}
impl GreatTrait for GreatType {
    fn great_fn(great: GreatTraitContext) -> GreatResult {
        magic_fn()
    }
}
```

for convenience, add

```fff-lang
impl GreatTraitContext {
    fn apply<T: GreatTrait>(self) -> GreatResult { T::great_fn(self) }
}
```

this seems great, but usage have to be

```fff-lang
fn some_other_great_fn() {
    const context = get_context();
    const result = context.apply::<GreatType>();
}
```

then static reflection will be very convient to be

```fff-lang
// C# style
fn apply(self, ty: std::reflect::Type) -> GreatResult {
    static_assert(ty.is_trait(typeof(GreatTrait));
    ty.get_method(typeof(GreatTrait), "great_fn").apply(self)
}

// my maybe cool style
fn apply(self, T: GreatTrait) -> GreatResult {
    T::great_fn(self) 
}

const result = context.apply(typeof(GreatType));
```