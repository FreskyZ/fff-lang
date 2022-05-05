# fff-lang Internal Document

## It's 2022 now

near 5 years since last active development phase, I'm encouranged by [crafting interpreters](http://www.craftinginterpreters.com/introduction.html),
walking through a lot of public or private, ongoing or finished code project and a lot more real life events, I'd like to push this project advance a little.

The final target is still generating native executable files and bootstrap, but this time I accept using llvm,
and the recent target is still add a new layer for semantic analysis, 
for at least variable definition use check and simple type check, and run them with the original virtual machine.

Currently, master can only syntax parse a little test files, virtual machine does not follow new syntax parse libraries,
and tags/v0.1.0, the homework handin version, can successfully run tests/syntax/prime.sm to a result (although all other test files failed to run).
The original semantic try is defacto failed, because it stucked and paused because it is too large for human mind, and now I completely forget
every related things in my mind (just like DRAM) and cannot continue from where I paused.

Then a rough __road map__ will look like

1. try fix tags/v0.1.0 (called @runnable later) to run more test files, include some "fast impl" caused by reaching deadline (at Dec 2016)
2. merge cargo workspace into one cargo project, they split because incremental building does not exist at that time 
   and this 20k lines of code "small" project need 40 seconds to build even with only a line or a character changed, but now there is.
3. update virtual machine and virtual machine codegen to new syntax parsing library with help of @runnable
   and try to execute several test files to end, after this, @runnable should be acceptable to discard

and more

- update lexing and syntax parse to my current experience and knowledge
- update disagnostics management, rename messages to diagnostics, remove perror! and perrorln! because there is now eprint! and eprintln!
- design and add semantics layer (actually when I came up with the semantic-analyze.md file rustc is still developing its mir layer)
- try add some basic optimizations like const eval, const propagation, loop invariant move and change tail recursion to loop
- generate llvm code and build to native executable file
- bootstrap self

## Update

Previous contents will keep as is, for some kind of "don't forget what you initially think". 

Major change summary

- handin: I do fix some issues and make some updates, include the loveheart.sm, but when I review the following changes (changes 
  after Dec 2016 before Jul 2017), they are already too far away from original virtual machine, so I have to write a complete new interpreter
- source: redesign source location types, now span is only u64 
- source: add virtual file system, this can and should replace a lot of "with_test_input" style methods
- lexical: redesign token type, make it more flat and easy to use
- lexical: refactor parser, merge the original multiple layer lexers into one unified parser. I was very proud of 
  that multiple layer explicit state transfer lexical parser 5 years ago, but now the unified version is clearly simpler and more powerful
- syntax: update traits, clarify Node (Visitable) and Parser, get rid of propagating <F> parameter by the way
- syntax: add visitor and pretty print visitor, use std::fmt::Formatter instead of return string
- syntax: update test infrastructure, use make_* macros instead of a lot of "with_test_input"s
- syntax: refactor parser, merge into one unified parser, which allows more flexiable parameter and return value control
- interface: add formal interface, add command line argument parser based on my experience using clap
- syntax: update and add a lot of new syntax, include major support of generics, because I think without vector<T> I cannot write any useful
  program, while this time I don't want it to be language feature but only a standard library feature
- syntax: store ast in arena instead of vecs and boxes, blame this line to view results in tests/ast/*.stdout

some detailed plans is included by the way

- [ ] syntax: define syntax in formal syntax, validate it is LL1 and use calulated first set in maybe_* functions
- [ ] syntax: add recursive guard for ast visitors, eq returns true, formatters prints `[circular]`, profiler count nothing, use shared preallocate array to reduce allocate
- [ ] syntax: update random test input generator
- [ ] syntax: add missing array dup def
- [ ] syntax: add numeric literal type as suffixed/unsuffixed and bin/oct/dec/hex/unprefixed, tuple index expr require unsuffixed and unprefixed
- [ ] syntax: format string, attention that retry lexical parsing need to withdraw diagnostics
- [ ] syntax: function expression, capture syntax TBD, IIFE is a more tradictional may where rust block expression is used
- [ ] source: use unsafe instead of panic in char iterator
