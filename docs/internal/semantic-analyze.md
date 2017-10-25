# fff-lang Internal Document

## Semantic Analyze

**phase 1**, convert syntax tree to semantic tree

- directly map node tree, drop span information if not needed
- build up an accociate scope tree, every semantic node may have a its own scope or reference a parent node's owned scope, a scope itself also knows its parent
- collect definitions and stores in scope
  - a function or type definition is always stored in global scope, given def id or shared reference to be referenced
  - a variable definition is stored in its local scope, given shared reference to be referenced

result maybe like, from

```fff-lang
type string { data: &u8, cap: u64, len: u64 };
fn new_string() -> string {
    var result = mem::alloc::<string>();
    result.data = mem::alloc_array(16);
    result.cap = 16;
    result.len = 1;
    return result;
}

fn main(argc: i32) {
    
    type AnotherType{ argc: int, argv: [string] };
    fn magic_function() -> i32 { return 42; }

    const magic = magic_function();
    if magic > 0 {
        println("magic is {magic}");
    }
    return magic - 1;
}
```

to

```
module x, id = 0, file name = '<file>'
  scope 'x'
    def type 'string', child node id = 0
    def fn 'new_string', child node id = 1
    def fn 'main', parameters, return type
  typedef 'string', stmt#0 fields, location, functions
    scope 'string'
      def field 'data', member id = 0
      def field 'cap', member id = 1
      def field 'len', member id = 2
    fielddef 'data', member#0, type
    fielddef 'cap', member#1, type
    fielddef 'len', member#2, type
  fndef 'new_string', stmt#1, parameters, return type
    scope 'new_string'
      def var 'result', child node id = 0
    vardef 'result', scope ref x::new_string, init expr, location
    assign, scope ref x::new_string, left expr, right expr
    assign, scope ref x::new_string, left expr, right expr
    assign, scope ref x::new_string, left expr, right expr
    return, scope ref x::new_string, expr
  fndef 'main' stmt#2, return type
    scope 'main'
      def var 'argc', parameter id = 0
      def type 'AnotherType', statement id = 0,
      def fn 'magic_function', staetment id = 1,
      def var 'magic', statement id = 2
    paramdef 'argc', parameter#0, type
    typedef 'anothertype', statement#0, ...
    fndef 'magic_function', statement#1, ...
    vardef 'magic', statement#2, scope ref x::main, const, type
    if-stmt, statement#3
      scope '<if at 14:4-16:4>'
      fn-call-stmt 'println' stmt#1, exprs
    return, stmt#2, scope ref x::main, expr
```

**phase 2**, bind symbol uses to definitions, steps maybe

- source code `{ const a = "WORLD"; const b = ("hello" + b).to_lower(); }`
- the p1 semantic tree is like 
    ```
    block-stmt #?
      scope '<block at 1:2-3:4>'
        def var 'a', statement id = 0
        def var 'b', statement id = 1
      vardef 'a', scope ref <block at 1:2-3:4>, statement#0, init-expr = string literal "WORLD";
      vardef 'b', scope ref <block at 1:2-3:4>, statement#1, 
        init-expr = fn-call(member-access(paren(binary(string-literal, name))))
    ```
- walking through the semantic tree, an unbound name 'a' in the init expr of vardef 'b' is found, then a search request { name: 'a', type: 'var', querying_id: '1' } is send to parent scope field of vardef 'b'
- the scope is bound to the block-stmt's scope at phase 1, it first walk through its own definitions, then find the name 'a' and matches the type 'var', as for variable definition, the definition is located at statement#0, which is lower of querying statement#1, all condition matches, then returned def id path (0, 0), first 0 means no parent forward, second 0 means 0th definition in that scope
- the expression continue type check, find a binary operator is applied to 2 string type values, then a seaerch request { name: 'operator+', type: 'fn' }, the block's scope does not find the definition, a same request is send to block scope's parent scope, assume it's just global scope, and a matched definition is found, which is the 3rd definition in the definition collection, then the global scope returned def id path (0, 3), first 0 means no forward, second 3 means index, then the block's scope add a 1 to forward count and returned (1, 3), then the expression continues to get a fndef reference according to the defid path, find the function's parameter type maches and the return type is string, then the binary expression is assigned string type, things continued

### Static reflection API design

add expression 'GetTypeExpression', which maybe 

    get-type-expr = 'typeof' '(' type-use | expr ')'

like C++, the expression is only type checked but never executed

the expression returns a 'Type' type value, it may contains

```
type Type {
    fn get_fields() -> [Field];
    fn get_methods() -> [Method];
    fn get_size() -> u64;
}

type Field {
    fn get_name() -> string;
    fn get_type() -> Type;
    fn is_public() -> bool;
}

etc.
```

### Other things

- record use count in definiton, warn on not used
- consider fndef as variable and how handle pure function, captured or binded function and variable name resolve issues
