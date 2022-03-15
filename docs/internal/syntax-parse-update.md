
[x] merge ISyntaxParse and ISyntaxGrammar to Node, provide default return false implementation for matches_first
[x] add make_lit for making LitExpr node, macro syntax similar to lexical t!
[x] change ISyntaxFormat to Visitor
[x] pass test
[x] add take address expression
[ ] merge simple name into name, rename type use to type ref
[ ] redesign builtin array type, should include size, the dynamic array type should be in standard library based on pointer
[ ] support generic type in type ref and name, like rust, use ::< for generic parameter in expr
[ ] add object literal syntax
[ ] move multiple file operations from syntaxtree.rs into driver
[ ] add simple enum type
[ ] try connect to old semantic analysis virtual machine?
