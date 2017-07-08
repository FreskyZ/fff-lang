Release Note of ffc
============

0.1.0
-----
at 16/12/22  
Handin version as Compiler Principles course work

0.1.1
-----
at 17/1/28
Remove fsz-common dependency which depends on time and then libc and kernel32 and a bunch of complex things
Rename from smallc to ffc, rename executable name from smc to ffc

0.1.2
-----
at 17/3/?
Setup codepos, messages and util crates for basic infrastructure
Move lexical out and completely refactor, completely standarized tests
Move lexical/v0 out as codemap for source code manager and future message format
Merge lexical/v2 and v3 and rewrite numeric literal parser for floating point exponents
Move syntax out
Setup build scripts