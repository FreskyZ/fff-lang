
[x] merge ISyntaxParse and ISyntaxGrammar to Node, provide default return false implementation for matches_first
[x] add make_lit for making LitExpr node, macro syntax similar to lexical t!
[x] change ISyntaxFormat to Visitor
[x] pass test
[x] add take address expression
[x] rename type use to type ref
[x] rename session to context, rename sess to cx, remove <F> from SourceChars, lexical::Parser and ParseContext
[ ] redesign builtin array type, should include size, the dynamic array type should be in standard library based on pointer
[ ] merge simple name into name
[ ] support generic type in type ref and name, like rust, use ::< for generic parameter in expr
[ ] add object literal syntax
[ ] move multiple file operations from syntaxtree.rs into driver
[ ] add simple enum type
[ ] try connect to old semantic analysis virtual machine?
