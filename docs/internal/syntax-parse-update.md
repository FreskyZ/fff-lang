# fff-lang Internal Document

## Syntax parser update 2

syntax parser implementations have been completely updated in 23a366a, but there still remains something like,

```rust
sess.tk, sess.next_pos;
if let &Token::Sep(sep) = sess.tk { ... sess.move_next(); ... }
```

still kind of not clear and easy, new update concerns:

Update: add sess.prev_span and cut off relavant codes

Analyze: parse sess interface update requirements
    binary_expr:
        current: `match (sess.tk, sess.pos) { (&Token::Sep(sep), sep_span) if sep.is_category($cat) => ..., ... }`
        expect:  `match sess.try_expect_sep_cat($cat) { Some((sep, span)) => ..., None => ... }`
    expr_lit:
        current: `let (expect_end_sep, span) = match (sess.tk, sess.pos) { (&LBrace, span) => (RBrace, span), (&LParen, span) => (RParen, span), ... };`
        expect:  `let (starting_sep, starting_span) = sess.expect_seps(&[LParen, LBracke, LBrace])?; let expect_end_sep = match starting_sep ...`
    expr_list:
        current: `if sess.tk == &expect_end_sep { ... }`
        expect:  `if let Some((_, span)) = sess.try_expect_sep(expect_end_sep) { ... }`
    expr_list:
        current: `if let &Comma = sess.tk { if sess.next_tk == &expect_end_sep { sess.move_next2(); return ... } }`
        expect:  `if let Some((_span1, ending_span)) = sess.try_expect_sep_2(Comma, expect_end_sep);`

gather requirements:
        `try_expect_sep_2`        + 2
        `try_expect_sep`          + more than 5
        `try_expect_sep_cat`      + 2
        `expect_seps`             + 1
        `try_expect_if_grammar`   + 5: primary, postfix, expr, statement, item
    and more precise `expect xxx, yyy, zzz` message content specifier

and how to update `sess.tk, sess.pos (sess.span), sess.next_tk, sess.next_span`
    solution 1: `sess.current_token, sess.current_span, sess.next_token, sess.nextnext_span`
    solution 2: `sess.tokens[0].token, sess.tokens[2].span`
    soltuion 3: `sess.tokens(0).token, sess.tokens(1).span`
    solution 4: `sess[0].token, sess[1].span`
    solution 5: `sess.offset(0).token, sess.offset(2).span`
    solution 6: `sess.next_nth_token(1), sess.next_nth_span(0)`
    solution 7: `sess.token, sess.span, sess.next_token, sess.next_span, sess.nextnext_token, sess.nextnext_span`

decide them after the previous features updated

collect new expectations:

```rust
// old
fn expect_sep(&mut self, sep: Seperator) -> Result<Span, ()>;
fn expect_keyword(&mut self, kw: Keyword) -> Result<Span, ()>;
fn expect_ident(&mut self) -> Result<(SymbolID, Span), ()>;
fn expect_ident_or(&mut self, kws: impl Iterator<Item = Keyword>) -> Result<(SymbolID, Span), ()>;  // used by fn, for and var decl to allow underscore and this
fn expect_ident_or_if(&mut self, f: fn(&Keyword) -> bool) -> Result<(SymbolID, Span), ()>;          // used by type use to allow primitive types

// new
fn expect_seps(&mut self, seps: impl Iterator<Item = Seperator>) -> Result<Span, ()>;               // used by expr_list to allow mutliple open seps
fn try_expect_sep(&mut self, sep: Seperator) -> Option<Span>;                                       // used by many expecting comma, colon, paren, etc.
fn try_expect_sep_cat(&mut self, cat: SeperatorCategory) -> Option<(Seperator, Span)>;              // used by binary and unary to filter seps
fn try_expect_sep_2(&mut self, sep1: Seperator, sep2: Seperator) -> Option<(Span, Span)>;           // used by many try expecting comma and close seps
fn try_expect_item_if_grammar<T: ISyntaxGrammar + ISyntaxParse>(&mut self) -> Option<T>;            // used by dispatcher like primary, postfix, expr and statements
```

ideally they will remove all `sess.tk` like and `sess.move_next()` like calls

considering that later more precise expecting message information added, try aggregate them

```rust
fn expect_token(enum ExpectedToken{ Sep(Seperator), Keyword(Keyword), Seprators(Vec<Seperator>) }) -> Result<Span, ()>;
fn expect_ident(enum ExpectedIdent{ Ident, OrKeywords(Vec<Keyword>), OrIf(fn(&Keyword) -> bool) }) -> Result<(SymbolID, Span), ()>;
fn expect_item<T: ...>();
fn try_expect_sep( ... );
fn try_expect_sep_cat( ... );
fn try_expect_sep_2(...);
fn try_expect_item<T: ...>();
```

this seems not good

starting implement new expectors
new issues:

  - `LabelDef::parse` require check current token is Label
  - `Module::parse` require check current token is EOF to break label
  - `LitExpr::parse` require check current token is Literal