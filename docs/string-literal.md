# fff-lang Designment Document

There are actually many things about string literal

## Basic Syntax

```rust (add some color by saying this is rust)
var a = "hello world";          // always double quote
var b = "\thello world\n\0";    // basic escape
var c = "hello world\x0A";      // hex escape, fixed 2 hex digits, WIP
var e = "你好，世界！";          // direct unicode characters allowed
// unicode escape, fixed 4 hex digits (lower case 'u') or 6 hex digits (upper case 'U')
// use 6 hex digits not 8 because it is ridiculous for unicode to increase plane count from 16 to over 256
// // I know people didn't expect network device count to increase over uint32, but
// // all the characters/emojis all human beings inputs/types in the world really won't exceed 16777216
var d = "hello worl\u0063\U00FF01"; 

// multiple line allowed, note that trailing whitespaces and leading whitespaces
// all included, line end is *fixed* LF regardless of actual line end in source code file
var f = "你好
    世界"; 
var g = "hello // this is inside string literal, not a comment
world";
// add a `\r` at end of each line if you really want a CRLF line end 
var h = "hello\r
world\r
!";
// use continuous string literals for long single line content string, WIP
// allow add operator on string literal TBD
var i = 
    "hello and some long content"
    " world and some other long content"
    " more very long content";
```

## Raw String Literal

```rust
// lower case r allows everything except double quote inside
var j = r"C:\Users\User\Desktop\desktop.ini";
var k = Regex::new(r"\d+");
// it can be multiple line, same as normal string literal, line end is *fixed* LF
var k = r"
some other
multiple line
string content with many \ inside";

// upper case R for specify custom delimeter WIP
// the custom delimeter cannot contain `(` or `"` or any whitespace, max length 16
var l = R"(C:\Users\Some\Path)";
var m = R"xml(<tag attr="value" attr2="value2">text</tag>)xml";
var n = R"sql(
SELECT *
FROM TABLE
WHERE COLUMN = "ABC"
)sql";

// raw string can be mixed with normal string when concat
var x = "abc\r" r"def\ghi" R"(some"")"; 
```

## Binary String Literal WIP

```rust
// normal string literal has type /* TBD */ string?, binary string literal has type [u8; N]
// only ascii content allowed
var o = b"ascii"; // o: [u8; 5] = [0x61, 0x73, x69, 0x69, 0x69];
// simple escape and hex escape allowed, but no unicode escape
var p = b"ascii\tonly\x0A\x00"; // p: [u8; 12]
// binary literal, have type u8
var q = b'\x41'; // p: u8 = 0x41
// binary raw string
var q = br"abc\n\r\t"; // or rb"...", q: [u8; 9] = [b'a', b'b', b'c', b'\\', b'n', b'\\', b'r', b'\\', b't'];

// if concat, all components must be explicitly marked as binary string, normal string literal and raw normal string literal not allowed
var y = 
    b"very long part1"
    b"even longer part2"
    rb"raw part\n\r\t"
    b"not raw part again";
```

## User Defined Literal / Tagged Template

see [c++](https://en.cppreference.com/w/cpp/language/user_literal)
and [javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
and [rust discussion](https://internals.rust-lang.org/t/pre-rfc-custom-literals-via-traits/8050)

it is useful for imaginary/complex, datetime literal and some other with-unit values

```c++ (no, c++ does not have 'var', but it does have custom literal postfix)
var r = 1 + 2i; // Complex
var s = (2 + 3i) * (3 + 4i); // Complex as Add<Complex>

var datetime = 2021y/May/5;
var duration = 1h + 2min + 3s;

var distance1 = 1.6km;
var distance2 = 1mi;
assert(distance1 == distance2);
```

I'm not going to implement this, because

- for simple single conversion, use conversion methods
```rust
var t = Expr::from(primary_expr);
var u = Color::from_rgb(240, 240, 120);
var v: string = path.into(); // Path as Into<string>
var w: i32 = "42".parse(); // i32 as FromStr
```

- for complex, datetime and duration, I see no big advantage of writing looks-very-cool literals
```rust
var value = Complex::new(1, 2);
var time = Time::new(1, 2, 3);
var duration = Duration::new_ms(0, 0, 0, 420);
```

- for other with-unit values, what kind of program need them?
```
var si_distance = 1.6;
var us_distance = 1.0 * KM_PER_MILE;
```

no, I found one, `var stmt = sqlf"SELECT COL1, COL2 FROM {mycollection} WHERE COL3 = {complex_fn()}";`,
rust procedure macro uses token stream as input and output,
this one may need semantic tree or runtime reflection to implement "cutom format string" or "custom format string to sql"

## Format String

```rust
// basic
var z = f"hello {1 + 1} world {complex_fn()}: {vector.join(",")}";
// format specifier, detail syntax TBD, custom specifier (e.g. datetime) TBD
var a = f"{display}, {debuggable:?}, {hexable:X}, {number:.5f}, {padded:>10}";
// complex inline logic inside format string (why don't you put it outside?), IIFE syntax TBD
var b = f"{a * b << h / c + d % e - f >> g > h * i < j << k >= m && n || o & p | q ^ r != s == t} {fn { println("helloworld"); return 42; }():X}";

// no binary format string, use methods on vector<u8> to concat byte sequences
// no raw format string, concat them, all components will become parameter in string::format (name TBD, variadic parameter function TBD) call...
// ...semantically, if any format segment can be const evaluated, it may be eliminated, if all eliminated, it may be simple constant string
var c =
    r"raw part \r\n"
    f"format part {1 + 1}"
    r"raw part again"
    "not raw not format part";
```

TODO format specifier:
- https://en.cppreference.com/w/cpp/utility/format/formatter, custom by specialized std::formatter
- https://doc.rust-lang.org/std/fmt/, custom by proc macro
- https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types, custom by implement ICustomFormatter (no, .net framework definitely don't call this)
- https://docs.python.org/3/library/string.html#formatspec, custom by extend string.Formatter

## String Literal Type

this is not about string literal syntax but string literal semantic

```ts (this is literally learn from typescript)
type Limited = "value-1" | "value-2";
type Limited2 = Limited1 | "another-value";
```

although the grammar is `literal-type = LITERAL` and `sum-type = type-ref (OR type-ref)*`,
sum type is only allowed to only contains string literal or only contain number literal, other type-refs not allowed

> it is not guaranteed to be string value at runtime (maybe a number) TBD

## Raw Identifier WIP

this is not string literal but an identifier, but before rust decide to use `r#keyword` raw identifier, 
I designed the string literal style syntax `i"keyword or even whitespace"`, note that unicode is supported by normal identifier, don't need this

or use `ri"more raw identifier\a\b\c\d\e"` to interpret content as raw string

## Prior Art

> these prior arts, if ordered by time, is some kind of programming language design improvement history

### C++

https://en.cppreference.com/w/cpp/language/string_literal

- multiple line: not allowed in normal string literal
- octal escape: cannot decide following [0-7] is part of this escape
- hex escape: cannot decide following [0-9A-F] is part of this escape
- unicode escape: lower case/upper case supported, fixed 4 length/8 length
- unit type prefix: `L`/`u8`/`u`/`U` prefix for `wchar_t`/`char8_t`/`char16_t`/`char32_t`
  > unit type prefix: string literal represents some sequence, type of its item (unit) may be determined by prefix
- raw string literal: `R"delimiter(content)delimiter"`, can have unit type prefix, delimeter cannot be double quote, paren or *whitespace*, max 16 length
- mssc: continuous string literal, must have same unit type prefix, or omit to default to previous one, mix raw and not raw is ok
  > mssc: multiple line in source code single line in content
- format string: used to be `printf`, incorrectly goes to `iostream`, now there is `std::format` and use `{}` style parameter, format specifier allowed

### Rust

https://doc.rust-lang.org/reference/tokens.html

- multiple line: allowed in string literal and raw string literal, fixed LF
- octal escape: no
- hex escape: fixed 2 length
- unicode escape: brace quoted 1-6 length
- unit type prefix: `b` for binary string for u8 slice
  > other prefixes are reserved, I'd like to forbit them, too
- raw string literal: use `r###"something include # and ""###`, can have unit type prefix, # count need match
  > I don't like to count number of #s, although many #s are rarely used
- mssc: `concat!` macro, or use `\` at end of line, leading whitespaces in following line is trimmed
- format string: `format!` series macros, `{}` style, format specifier allowed

### C#

https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/

> it is amazingly updated in C#11 (??? C# is now 11?)

- multiple line: not allowed in normal string literal, allowed in raw string literal
- octal escape: no
- hex escape: cannot decide following [0-9A-F] is part of this escape
- unicode escape: lower case/upper case supported, fixed 4 length/8 length
- unit type prefix: no
- raw string literal
  - verbitam string literal: `@"C:\some\file"`, use double double quote to represent one double quote
  - raw string literal: `"""some " or "" inside"""`, use more double quote to allow more double quote inside,
    it maybe either completely single line, or
    beginning quote sequence must immediately follow by line end, ending quote sequence must be in its own line,
    and all lines of actual content is begin trimmed first line's whitespace count
    > this is nearly exactly same as my own design in another cannot open source project,
    > but I don't like this as a language feature, a standard library feature (string asscociate method) is ok
- mssc: use +
- format string: `$"{1 + 1}"` and new `$$"""allow {{1 + 1}} and not a parameter {} inside"""`, format specifier allowed
  > it's a shame that C# don't allow `$"{condition ? a : b}"` but require a paren, and will never be fixed in future

### Java

https://www.vojtechruzicka.com/raw-strings/
http://openjdk.java.net/jeps/326 // is this official document?

- multiple line: not allowed in normal string literal, allowed in raw string literal
- octal escape: no
- hex escape: cannot decide following [0-9A-F] is part of this escape
- unicode escape: lower case u and fixed 4 length, use surrogate pair for non-BMP code points
- unit type prefix: no
- raw string literal: backslash, use more backslash if need backslash inside, use String.align to remove common leading whitespaces
  > cannot use backslash source code in markdown conveniently is why I don't want backslash in my grammar
- mssc: use +
- format string: use String.format, format specifier allowed

### Python

https://docs.python.org/3/reference/lexical_analysis.html

- multiple line: not lallowed in short string literal, allowed in long string literal
- octal escape: cannot decide following [0-7] is part of this escape
- hex escape: fixed 2 length
- unicode escape: lower case/upper case supported, fixed 4 length/8 length, unicode character name (UCD) allowed
- unit type prefix: none for `str`, `b` for `bytes`, seems to be a no effect `u` for compatibility
- raw string literal: `r"C:\some\file"`
- mssc: continuous string literal, and trailing \ in whole language's lexical grammar, not only in string literal
- format string: `f"{1 + 1}"`, format specifier allowed
  > it is not shame to not allow `f"{'"'}"` and `f'{"'"}'` because it makes parser simpler

### Go

https://go.dev/ref/spec#String_literals

- multiple line: not allowed in string literal, allowed in raw string literal
  > I don't write any of this, but grammar seems saying it can
- octal escape: fixed 3 length
- hex escape: fixed 2 length
- unicode escape: lower case/upper case supported, fixed 4 length/8 length
- unit type prefix: no
- raw string literal: use backslash
- mssc: use +
- format string: use `fmt.Sprintf`, `%` style, custom format specifier in syntax
  > why this this century language is using % style format specifiers?
  > why custom format specifier in syntax?

### Javascript

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Grammar_and_types#string_literals  
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals

> search result for 'javascript string literal' is completely overriden by this ES2015 template literal

- multiple line: not allowed in string literal, allowed in template literal
- octal escape: cannot decide following [0-7] is part of this escape
- hex escape: fixed 2 length
- unicode escape: lower case supported, fixed 4 length, upper case supported, brace quoted 1-6 length
- unit type prefix: tagged template allow return type to be arbitrary
- raw string literal: String.raw tag
- mssc: use +
- format string: `{}` style, no format specifier (use `String.prototype.toExponention`, etc. methods)
