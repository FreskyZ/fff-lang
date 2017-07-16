# fff-lang Internal Document

## Source code management designment

Currently, source code is managed by codemap crate, it stores source file names and contents, 
provide iterators to iterate through srouce code chars and their positions, and provide infrastructure to store
symbols/strings

messages crate rely on codemap, a Message only stores description strings and source code locations,
then it need source code manager to display very human friendly informations

lexical and syntax crates rely on codemap and messages, they store very many kinds of structured information and 
their location information to be used in later debug and message construction

now I want to finish the format methods and I'm not clear how to design the new module drivers for program driver and 
test driver to use. first, module drivers and test drivers:
```rust
// codemap::CodeMap:
fn CodeMap::new() -> CodeMap;
fn CodeMap::with_files(files: Vec<String>) -> Result<CodeMap, CodeMapError>;
fn CodeMap::with_test_str(src: &str) -> CodeMap;
fn CodeMap::input_files(&self, files: Vec<String>) -> Result<(), CodeMapError>;
fn CodeMap::input_str(&self, src: &str);
// codemap::SymbolCollection
fn SymbolCollection::new() -> SymbolCollection;
// messages::MessageCollection
fn MessageCollection::new() -> MessageCollection;
// lexical::TokenStream
fn TokenStream::new(code_chars: CodeChars, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> TokenStream;
fn TokenStream::with_test_str(src: &str) -> TokenStream;
fn TokenStream::with_test_input(src: &str, symbols: &mut SymbolCollection) -> TokenStream;
// syntax::SyntaxTree
fn SyntaxTree::new(tokens: &TokenStream, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> SyntaxTree;
fn ISyntaxItemParse::with_test_str(src: &str) -> <Self as ISyntaxItemParse>::Target;
fn ISyntaxItemParse::with_test_input(src: &str, symbols: &mut SymbolCollection) -> <Self as ISyntaxItemParse>::Target;
// semantic::Package (in design)
fn Package::new(syntax_forest: &SyntaxForest, messages: &mut MessageCollection, symbols: &mut SymbolCollection) -> Package;
```

now syntax and message need human friendly format method, current is

    fn Message::fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
    fn ISyntaxItemFormat::format(&self, indent: u32) -> String;
    
it at least need these

    fn Message::format_with(&self, f: (&SymbolCollection, &CodeMap)) -> String;
    fn IFormatWith<(u32, &SymbolCollection, &CodeMap)>(&self, f: (u32, &SymbolCollection, &CodeMap)) -> String;

and codemap need this

    fn CodeMap::get_position_with_span(&self, span: &Span) -> (usize, usize);
    fn CodeMap::get_line_with_position(&self, fileid: usize, row: u32) -> &str;

you can see, every parse method needs messages and symbols, but leave them separated, because if you define something like 
MessageCollectionAndSymbolCollection, it will be to complicated waste keyboard hit, something like

    struct ParseSession {
        tokens: &TokenStream,
        messages_and_symbols: &mut MessageCollectionAndSymbolCollection
    }

will also seem strange and very hard to use

and every format method need symbols and codemap, still leave them seperated, because syntax format need an extra indent
but message format do not need it

another problem, how to manage multi file input's syntax rule and semantic rule, 
as in design, I support multi file input and they are parallel, type def and fn defs do not have to declare before use,
but, current lexical's interface ignored EOFs and give syntax a EOF, then syntax know nothing about EOF and EOFs and stoped at EOF,
which make it actually not usable at multi file occassions,
and, new problem arises when I allow statements at global scope, which is the order of them, actually, which is the order of the files,
do I have to follow exactly command line input order to decide file order, that means, commandline input will significantly impact semantic,
although command line options will change many semantic in other language, like optimization level, error handling method, etc. but this 
seems too strange, in fact, I will not feel happy if all the files have to be written in command line and order is strict

and issue/6, I'd like to make lexical and syntax unaware of files, let them only facing one file in their complete process, 
so codemap should at lease publify code file, or refactor its interface

  - try privatify ISyntaxItemGrammar
  - continue your semantic
  - name this change as v0.1.2 to v0.1.3
