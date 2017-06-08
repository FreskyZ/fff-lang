# fff-lang Internal Document

this is just a buffer for collect messages together to determine how they will be formatted according to the source code given by codemap

```rust
Message::new_by_str(error_strings::UnexpectedEOF, vec![
    (self.current_span, error_strings::CharLiteralHere),
    (pos.as_span(), error_strings::EOFHere)
])
// Debug format:
// error: unexpected EOF
//    | At <0>100-102: char literal here
//    | At <0>103-103: EOF here
// Want it to be
// error: unexpected EOF
//    --> <file-name-0>:15:9-15:10
//  15 |     var ch: char = 'a
//     |                    --^
//     |                    | EOF here
//     |                    char literal here
```

```rust
Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
    (self.current_span, error_strings::CharLiteralHere),
], vec![
    error_strings::UnicodeCharEscapeHelpSyntax
]),
Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
    (all_span, error_strings::CharLiteralHere),
], vec![
    error_strings::CharLiteralSyntaxHelp1
])
// Want it to be
// error: empty char literal
//    --> <file-name-0>: 103:20-103:21
// 103 |              if ''.is_numeric() {
//     |                 -- char literal here
Message::new_by_str(error_strings::UnexpectedEOF, vec![
    (all_span, error_strings::CharLiteralHere),
    (eof_pos.as_span(), error_strings::EOFHere)
])
Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, ch), vec![
    (self.current_span.get_start_pos().as_span(), error_strings::CharLiteralStartHere.to_owned()),
    (slash_pos.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
])
Message::new_by_str(error_strings::UnexpectedEOF, vec![
    (self.current_span, error_strings::CharLiteralHere),
    (pos.as_span(), error_strings::EOFHere)
])
Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
    (escape_start_span.merge(&current_span), error_strings::UnicodeCharEscapeHere.to_owned()),
], vec![
    format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, self.buf.clone()),
    error_strings::UnicodeCharEscapeHelpValue.to_owned(),
])
Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
    (escape_start_span, error_strings::UnicodeCharEscapeStartHere),
    (current_span, error_strings::UnicodeCharEscapeInvalidChar)
], vec![
    error_strings::UnicodeCharEscapeHelpSyntax,
])
Message::new_by_str(error_strings::UnexpectedEOF, vec![ 
    (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
    (pos.as_span(), error_strings::EOFHere),
])
Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, ch), vec![
    (self.start_pos.as_span(), error_strings::StringLiteralStartHere.to_owned()),
    (slash_pos.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
])
Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
    (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
    (self.escape_start_pos.as_span(), error_strings::UnicodeCharEscapeStartHere),
    (pos.as_span(), error_strings::StringLiteralEndHere),
], vec![
    error_strings::UnicodeCharEscapeHelpSyntax,
])
Message::new_by_str(error_strings::UnexpectedEOF, vec![
    (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
    (pos.as_span(), error_strings::EOFHere),
    (escaped_quote_pos_hint.as_span(), error_strings::LastEscapedQuoteHere),
])
Message::new_by_str(error_strings::UnexpectedEOF, vec![
    (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
    (pos.as_span(), error_strings::EOFHere),
])
Message::new_by_str(error_strings::InvalidNumericLiteral, vec![(strpos, "")])
Message::new(
    format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
    vec![(strpos, String::new())]
)
Message::with_help(
    format!("{}, {}", error_strings::InvalidNumericLiteral, $extra_msg),
    vec![(strpos, String::new())],
    $help,
)
Message::new(
    format!("{}: {:?}", error_strings::UseReservedKeyword, KeywordKind::As), 
    vec![($ident_pos, String::new())]
)
Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
    (strpos, ""), 
], vec![
    &format!("Did you mean `{}`({}) by `{}`({})?", ascii_ch, ascii_name, unicode_ch, unicode_name),
])
Message::new_by_str("Empty subscription", vec![(bracket_strpos, "subscription here")])
Message::new_by_str("Single comma in function call", vec![(paren_strpos, "function call here")])
Message::new_by_str("Single comma in function definition argument list", vec![
    (fn_name_strpos, "function definition here"),
    (params_paren_strpos, "param list here")
])
Message::new_by_str("Single item tuple type use", vec![(paren_pair_strpos, "type use here")])
Message::with_help_by_str("Require type annotation", 
    vec![(name_strpos, "Variable declaration here")],
    vec!["cannot infer type without initialization expression"]
)
Message::with_help("Unexpect symbol".to_owned(), 
            vec![(self.tokens.pos(self.current_index.get()), format!("Meet {:?}", self.tokens.nth(self.current_index.get())))],
            vec![format!("Expect {}", expect_desc)]
        )
```