# fff-lang Internal Document

## It's 2022 now

near 5 years since last active development phase, I'm encouranged by [crafting interpreters](http://www.craftinginterpreters.com/introduction.html),
walking through a lot of public or private, ongoing or finished code project and a lot more real life events, I'd like to push this project advance a little.

The final target is still generating native executable files and bootstrap, but this time I accept using llvm,
and the recent target is still add a new layer for semantic analysis, 
for at least variable definition use check and simple type check, and run them with the original virtual machine.

Currently, master can only syntax parse a little test files, virtual machine does not follow new syntax parse libraries,
and tags/v0.1.0, the homework handin version, can successfully run tests/syntax/prime.sm to a result (although all other test files failed to run).

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
