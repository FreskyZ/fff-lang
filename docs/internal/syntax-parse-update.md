
[x] merge ISyntaxParse and ISyntaxGrammar to Node, provide default return false implementation for matches_first
[x] add make_lit for making LitExpr node, macro syntax similar to lexical t!
[x] change ISyntaxFormat to Visitor
[x] pass test
[x] add take address expression
[x] rename type use to type ref
[x] rename session to context, rename sess to cx, remove `<F>` from SourceChars, lexical::Parser and ParseContext
[x] split Node into Node and Parser, and ParserOutput can be trait Parser { type Output: Node }, remove default implementation for parse and accept
[x] change array type ref to rust style `[type; size]` syntax, add function type ref, reference type ref and change type ref to an abc
[x] add keyword kind identifierable, and intern them when meet in lexical parser, that will make string list consistent (same as appear in source code)
[x] add FormatVisitor::impl_visit(name, all_span)
[x] discard all other use of simple name, merge simple name into name, accept generic part in name segment
[-] fix random test input generator for num_lit/expr/type_ref
[-] discard test constructors and use make_xxxx macros, upgrade existing make macros to use `at $start:lit + $end:lit` instead of plain $start,$end, change make_exprs to new make_expr
[x] add object literal syntax, it can fits in postfix expr with require base expr is an name
[ ] add numeric literal type as suffixed/unsuffixed and bin/oct/dec/hex/unprefixed, member access require unsuffixed and unprefixed
[x] add simple enum type, works as typed namespaced constant
[x] move multiple file operations from syntaxtree.rs into driver, add file id to module
[ ] format string
[ ] add missing array dup def
[ ] add generic parameters to type, add impl type and impl trait for type
[ ] add generic parameters to function, add generic constraints to function, impl type and impl trait
[ ] try connect to old semantic analysis virtual machine?
