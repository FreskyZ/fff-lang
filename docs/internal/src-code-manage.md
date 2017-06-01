# fff-lang Internal Document

// TODO: it is a quicknote currently

todos:  
for codemap, codepos, messages, lexical and syntax
every step should make them pass compile, optionally pass test

  - remove codepos::Position or consider better CharPos, should pass test
  - change codepos::StringPosition to Span which based on file id and 2 byte positions, which only takes 2 u32s
    make sure `type StringPosition = Span` is given and old `make_strpos` and `make_str_pos` still working, may not pass test
  - do not move codepos::Span to codemap::Span, continue not pass test, because messages rely on codepos
    try change Message to Message<SpanInfo>
  - provide span at previous v0lexer currently codemap_iter, pass previous broken test
  - change lexical's interface's identifier and label to span, maynot pass syntax's compile
  - provide symbol intern service in codemap, which accepts both span and owned string and raw string to be interned
    also provide stringify methods on symbol id, change syntax dependency on string to symbol id, pass all tests
  - finish message format in codemap because codemap relies on messages
  - move `ISyntaxItemFormat` and `ISemanticItemFormat` to `util::IFormatWithIndent` which contains `fmt_with_indent`
  - try privatify ISyntaxItemGrammar
  - continue your semantic

  - name this change as v0.1.2 to v0.1.3


consideration: what on earth is relationship between codemap, codepos and messages?

currently, in v0.1.2, codepos declares StringPosition and relies on nothing
messages declare StringPosition as field of Message and then relies on codepos
codemap may through several kind of exception (actually only 2 file reader exception) so rely on messages

new designment 1:
move Span to codemap and declares Message as Message<SpanInfo>, which causes all following crates to reference messages and codemap both
and, new Message<SpanInfo> actually relies on codemap::Span, and codemap::CodeMap an codemap::CodeMapIter relies on messages, 
which is very strange

new designment 2:
because, Span is actually index in source code which is managed by CodeMap, Message relies on Span to report location, 
which relies CodeMap for source code string, then, codemap should not rely on messages, because messages already rely on codemap
for source code string, that is, codemap has its own simple message format, and messages rely on codemap for more info, 
message's format is also implemented in messages itself

// TODO: make previous more clear