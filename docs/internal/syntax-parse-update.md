
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
[x] add bin-expr { op: >, left: bin-expr { op: < }} error in binary parser, boolean definitely returns bool which does not have operator> so that is a generic segment mistake
[-] fix random test input generator for num_lit/expr/type_ref
[-] discard test constructors and use make_xxxx macros, upgrade existing make macros to use `at $start:lit + $end:lit` instead of plain $start,$end, change make_exprs to new make_expr
[x] add object literal syntax, it can fits in postfix expr with require base expr is an name
[ ] add numeric literal type as suffixed/unsuffixed and bin/oct/dec/hex/unprefixed, member access require unsuffixed and unprefixed
[ ] add simple enum type, works as typed namespaced constant
[ ] move multiple file operations from syntaxtree.rs into driver, add file id to module
[ ] format string
[ ] try try for visitor and parse result
[ ] try connect to old semantic analysis virtual machine?
