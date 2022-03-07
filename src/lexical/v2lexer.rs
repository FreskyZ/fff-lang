///! fff-lang
///! 
///! lexical/v2, input v1, output string or numeric literal, identifier or other char

use crate::source::{Span, SourceChars, FileSystem, EOF};
use crate::diagnostics::{Message, MessageCollection, strings};
use super::v1lexer::{V1Token, V1Lexer};
use super::{ILexer, BufLexer, Keyword, KeywordKind, Separator, Numeric, Token, StringLiteralType, ParseSession};
use super::literal::numeric::parse_numeric_literal;

impl Default for Token { fn default() -> Token { Token::EOF } }

trait IdentifierChar {

    fn is_identifier_start(&self) -> bool;
    fn is_identifier(&self) -> bool;

    fn is_label_start(&self) -> bool;
    fn is_label(&self) -> bool;

    fn is_numeric_literal_start(&self) -> bool;
    fn is_numeric_literal(&self) -> bool;

    fn is_separator(&self) -> bool;
    
    fn pass_non_ascii_char(&self, strpos: Span, messages: &mut MessageCollection) -> char; 
}
impl IdentifierChar for char {

    // Include chinese alphabetical char
    fn is_identifier_start(&self) -> bool {
        *self == '_' || self.is_alphabetic()
    }
    // Include digit
    fn is_identifier(&self) -> bool {
        *self == '_' || self.is_alphabetic() || self.is_digit(10)  
    }

    fn is_label_start(&self) -> bool {
        *self == '@'
    }
    fn is_label(&self) -> bool {
        *self == '_' || *self == '@' || self.is_alphabetic() || self.is_digit(10)
    }

    // Only digit, '.' start is not supported
    // Update: remove '-' here, 
    //     that is, take several weeks to impl '-' and 'E' feature in num lit, but finally remove the feature in v2
    //     leave the feature in num lit parser for future use of FromStr
    fn is_numeric_literal_start(&self) -> bool {
        self.is_digit(10)
    }
    // Only digit or ASCII letters or underscore
    fn is_numeric_literal(&self) -> bool {
        *self == '_' || self.is_digit(36) || *self == '.'
    }

    fn is_separator(&self) -> bool {
        !self.is_identifier()
    }

    fn pass_non_ascii_char(&self, strpos: Span, messages: &mut MessageCollection) -> char {
        use super::unicode::check_unicode_char;

        match check_unicode_char(*self) {
            Some((unicode_ch, unicode_name, ascii_ch, ascii_name)) => {
                messages.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
                    (strpos, ""), 
                ], vec![
                    &format!("Did you mean `{}`({}) by `{}`({})?", ascii_ch, ascii_name, unicode_ch, unicode_name),
                ]));
                ascii_ch
            }
            None => *self,
        }
    }
}

pub struct V2Lexer<'chs, F> {
    pub(super) v1: BufLexer<V1Lexer<'chs, F>, V1Token, F>,
}
impl<'chs, F> ILexer<'chs, F, Token> for V2Lexer<'chs, F> where F: FileSystem {

    fn new(source: SourceChars<'chs, F>) -> V2Lexer<'chs, F> {
        V2Lexer { 
            v1: BufLexer::new(source),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, sess: &mut ParseSession) -> (Token, Span) {

        #[cfg(feature = "trace_v2_parse")] macro_rules! trace { ($($arg:tt)*) => ({ print!("[V2Next: {}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_v2_parse"))] macro_rules! trace { ($($arg:tt)*) => () }

        macro_rules! ident_to_v2 { ($ident_value: expr, $ident_pos: expr) => ({
            match Keyword::parse(&$ident_value) { 
                Some(Keyword::True) => Token::Bool(true),
                Some(Keyword::False) => Token::Bool(false),
                Some(other_keyword) => {
                    if other_keyword.kind(KeywordKind::Reserved) {
                        sess.messages.push(Message::new(
                            format!("{}: {:?}", strings::UseReservedKeyword, other_keyword), 
                            vec![($ident_pos, String::new())]
                        ));
                    }
                    Token::Keyword(other_keyword)
                }
                None => Token::Ident(self.v1.lexer.v0.lexer.0.intern(&$ident_value)),
            }
        }) }
        macro_rules! num_lit_to_v2 { ($num_lit_value: expr, $num_lit_strpos: expr) => ({
            let (num_lit_val, pos) = parse_numeric_literal($num_lit_value, $num_lit_strpos, sess.messages);
            (Token::Num(num_lit_val.unwrap_or(Numeric::I32(0))), pos)
        }) }

        #[cfg_attr(test, derive(Debug))]
        struct V15Token(char, Span, char, Span, char, Span);

        #[cfg_attr(test, derive(Debug))]
        enum State {
            Nothing,
            InIdent(String, Span),
            InLabel(String, Span),
            InNumLit(String, Span),
        }

        let mut state = State::Nothing;
        loop {
            self.v1.move_next(sess);
            let v15 = match self.v1.current_with_preview2() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Str(*value, StringLiteralType::Normal), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Str(*value, StringLiteralType::Raw), pos);
                }
                (&V1Token::CharLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (Token::Char(*value), pos);
                }
                (&V1Token::EOF, eof_pos, _2, _3, _4, _5) => {
                    // because if last token is ident, it will not finish because eof return early here
                    // Update, 17/3/25
                    //     it used be like this: `if eofed { return v2::eof } else { eofed = true, v15 = ' ' }` 
                    //     After add mutliple file support to codemap, here is a bug
                    //     because these codes assume that after EOF and call next again you will still receive a EOF, 
                    //     at that time you can return the true EOF, but now after EOF is other char or normal tokens
                    //     after last EOF is EOFs and after EOFs is EOFs, so EOF is not returned because of this
                    //     So, because not support preview2, preview nextch and return properly in InIdent, not here
                    return (Token::EOF, eof_pos);
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, &V1Token::Other(nextnext_ch), nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, sess.messages); // not need check next_ch and nextnext_ch because they will be checked in next loops
                    V15Token(ch, strpos, next_ch, next_strpos, nextnext_ch, nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, _4, nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, sess.messages);
                    V15Token(ch, strpos, next_ch, next_strpos, ' ', nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, &V1Token::EOF, eof_strpos, _4, nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, sess.messages);
                    V15Token(ch, strpos, EOF, eof_strpos, ' ', nextnext_strpos)
                } 
                (&V1Token::Other(ch), strpos, _2, next_strpos, _4, nextnext_strpos) => { 
                    let ch = ch.pass_non_ascii_char(strpos, sess.messages);
                    V15Token(ch, strpos, ' ', next_strpos, ' ', nextnext_strpos)
                }
            };

            match (state, v15) {
                (State::Nothing, V15Token(ch, strpos, EOF, _3, _4, _5)) => {
                    if ch.is_identifier_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return (ident_to_v2!(value, strpos), strpos);
                    } else if ch.is_numeric_literal_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return num_lit_to_v2!(value, strpos);
                    } else if ch.is_label_start() {
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern("")), strpos); // simple '@' is allowed, use @? to represent empty
                    } else {
                        match Separator::parse1(ch) {
                            Some(separator) => return (Token::Sep(separator), strpos),
                            None => state = State::Nothing,
                        }
                    }
                }
                (State::Nothing, V15Token(ch, strpos, next_ch, next_strpos, nextnext_ch, nextnext_strpos)) => {
                    if ch.is_identifier_start() {
                        let mut value = String::new();
                        value.push(ch);
                        if !next_ch.is_identifier() {             // TODO future: understand why it is here to make the last case pass
                            return (ident_to_v2!(value, strpos), strpos);
                        }
                        state = State::InIdent(value, strpos);
                    } else if ch.is_numeric_literal_start() {
                        let mut value = String::new();
                        value.push(ch);
                        state = State::InNumLit(value, strpos);
                    } else if ch.is_label_start() {
                        if !next_ch.is_label() {                // 17/5/8: TODO: same question as before, why this is needed
                            return (Token::Label(self.v1.lexer.v0.lexer.0.intern("")), strpos);
                        }
                        state = State::InLabel(String::new(), strpos);
                    } else {
                        match Separator::parse3(ch, next_ch, nextnext_ch) { // the try_from3 will check 3, if not, check 2, if not, check 1
                            Some((separator, 1)) => {
                                trace!("ch is {:?} at {:?}, result is {:?}", ch, strpos, separator);
                                return (Token::Sep(separator), strpos);
                            }
                            Some((separator, 2)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, result is {:?}", ch, strpos, next_ch, separator);
                                self.v1.prepare_skip1();
                                return (Token::Sep(separator), strpos + next_strpos);
                            }
                            Some((separator, 3)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, nextnext_ch is {:?}, result is {:?}", ch, strpos, next_ch, nextnext_ch, separator);
                                self.v1.prepare_skip1();
                                self.v1.prepare_skip1();
                                return (Token::Sep(separator), strpos + nextnext_strpos);
                            }
                            _ => state = State::Nothing,
                        }
                    }
                } 
                (State::InIdent(mut value, mut ident_strpos), V15Token(ch, strpos, next_ch, _4, _5, _6)) => {
                    if !ch.is_identifier() {
                        return (ident_to_v2!(value, ident_strpos), ident_strpos);
                    } else if !next_ch.is_identifier() {
                        value.push(ch); 
                        ident_strpos = ident_strpos + strpos;
                        return (ident_to_v2!(value, ident_strpos), ident_strpos);
                    } else {
                        value.push(ch);
                        ident_strpos = ident_strpos + strpos;
                        state = State::InIdent(value, ident_strpos);
                    }
                }
                (State::InLabel(mut value, mut label_strpos), V15Token(ch, strpos, next_ch, _4, _5, _6)) => {
                    if !ch.is_label() {
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern(&value)), label_strpos);
                    } else if !next_ch.is_label() {
                        value.push(ch);
                        label_strpos = label_strpos + strpos;
                        return (Token::Label(self.v1.lexer.v0.lexer.0.intern(&value)), label_strpos);
                    } else {
                        value.push(ch);
                        label_strpos = label_strpos + strpos;
                        state = State::InLabel(value, label_strpos);
                    }
                }
                (State::InNumLit(mut value, mut num_lit_strpos), V15Token(ch, strpos, next_ch, _4, _5, _6)) => {

                    if (ch == '.' && next_ch == '.')                        // for 1..2
                        || (ch == '.' && next_ch.is_identifier_start())     // for 1.to_string()
                        || !ch.is_numeric_literal() {                       // normal end
                        self.v1.prepare_dummy1();
                        return num_lit_to_v2!(value, num_lit_strpos);
                    } else if !next_ch.is_numeric_literal() {
                        value.push(ch);
                        num_lit_strpos = num_lit_strpos + strpos;
                        return num_lit_to_v2!(value, num_lit_strpos);
                    } else {
                        value.push(ch);
                        num_lit_strpos = num_lit_strpos + strpos;
                        state = State::InNumLit(value, num_lit_strpos);
                    }
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn v2_char_ext() {
    
    assert_eq!('a'.is_identifier_start(), true);
    assert_eq!('啊'.is_identifier_start(), true);
    assert_eq!(','.is_identifier_start(), false);
    assert_eq!('，'.is_identifier_start(), false);
    assert_eq!('_'.is_identifier_start(), true);
    assert_eq!('.'.is_identifier_start(), false);
    assert_eq!('1'.is_identifier_start(), false);

    assert_eq!('a'.is_identifier(), true);
    assert_eq!('啊'.is_identifier(), true);
    assert_eq!(','.is_identifier(), false);
    assert_eq!('，'.is_identifier(), false);
    assert_eq!('_'.is_identifier(), true);
    assert_eq!('.'.is_identifier(), false);
    assert_eq!('1'.is_identifier(), true);

    assert_eq!('a'.is_numeric_literal_start(), false);
    assert_eq!('啊'.is_numeric_literal_start(), false);
    assert_eq!(','.is_numeric_literal_start(), false);
    assert_eq!('，'.is_numeric_literal_start(), false);
    assert_eq!('_'.is_numeric_literal_start(), false);
    assert_eq!('1'.is_numeric_literal_start(), true);
    assert_eq!('.'.is_numeric_literal_start(), false);

    assert_eq!('a'.is_numeric_literal(), true);
    assert_eq!('啊'.is_numeric_literal(), false);
    assert_eq!(','.is_numeric_literal(), false);
    assert_eq!('，'.is_numeric_literal(), false);
    assert_eq!('_'.is_numeric_literal(), true);
    assert_eq!('1'.is_numeric_literal(), true);
    assert_eq!('.'.is_numeric_literal(), true);
    
    assert_eq!('a'.is_separator(), false);
    assert_eq!('啊'.is_separator(), false);
    assert_eq!(','.is_separator(), true);
    assert_eq!('，'.is_separator(), true);
    assert_eq!('_'.is_separator(), false);
    assert_eq!('.'.is_separator(), true);
    assert_eq!('1'.is_separator(), false);
}

#[cfg(test)] #[test]
fn v2_non_ascii_ch() {
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('.'.pass_non_ascii_char(Span::new(4, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('\\'.pass_non_ascii_char(Span::new(4, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!(';'.pass_non_ascii_char(Span::new(4, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('。'.pass_non_ascii_char(Span::new(4, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('⧹'.pass_non_ascii_char(Span::new(4, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '\\', "Backslash", '⧹', "Big Reverse Solidus"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('；'.pass_non_ascii_char(Span::new(4, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ';', "Semicolon", '；', "Fullwidth Semicolon"),
        ]));
        assert_eq!(messages, expect_messages);
    }
}

#[cfg(test)]
#[test]
fn v2_base() {
    use crate::source::{SourceContext, VirtualFileSystem, IsId, make_source};
        
    fn test_case_full(mut scx: SourceContext<VirtualFileSystem>, spans: &[Span], symbols: &[&'static str], expect_tokens: Vec<(Token, Span)>, expect_messages: MessageCollection) {
        let mut actual_messages = MessageCollection::new();
        let mut sess = ParseSession::new(&mut actual_messages);
        let mut chars = scx.entry("1");
        for span in spans {
            chars.intern_span(*span);
        }
        for symbol in symbols {
            chars.intern(*symbol);
        }

        let mut v2lexer = V2Lexer::new(chars);
        for expect_token in expect_tokens {
            assert_eq!(v2lexer.next(&mut sess), expect_token);
        }
        let next = v2lexer.next(&mut sess);
        if next.0 != Token::EOF { panic!("next is not EOF but {:?}", next); }

        assert_eq!(sess.messages, &expect_messages);
    }

    macro_rules! test_case {
        ($src:literal, [$($span:expr),*] expect $expect_tokens: expr) =>
            (test_case_full(make_source!($src), &[$($span),*], &[], $expect_tokens, MessageCollection::new()));
        ($src:literal, [$($span:expr),*] expect $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full(make_source!($src), &[$($span),*], &[], $expect_tokens, $expect_messages));
        ($src:literal, [$($span:expr),*], [$($string:literal),*] expect $expect_tokens: expr) =>
            (test_case_full(make_source!($src), &[$($span),*], &[$($string),*], $expect_tokens, MessageCollection::new()));
        ($src:literal, [$($span:expr),*], [$($string:literal),*] expect $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full(make_source!($src), &[$($span),*], &[$($string),*], $expect_tokens, $expect_messages));
    }

    macro_rules! lit {
        ($v:literal: bool, $start:expr, $end:expr) => ((Token::Bool($v), Span::new($start, $end)));
        ($v:literal: char, $start:expr, $end:expr) => ((Token::Char($v), Span::new($start, $end)));
        ($v:literal: str, $start:expr, $end:expr) => ((Token::Str(IsId::new($v), StringLiteralType::Normal), Span::new($start, $end)));
        ($v:literal: rstr, $start:expr, $end:expr) => ((Token::Str(IsId::new($v), StringLiteralType::Raw), Span::new($start, $end)));
        ($v:literal: u8, $start:expr, $end:expr) => ((Token::Num(Numeric::U8($v)), Span::new($start, $end)));
        ($v:literal: i32, $start:expr, $end:expr) => ((Token::Num(Numeric::I32($v)), Span::new($start, $end)));
        ($v:literal: u32, $start:expr, $end:expr) => ((Token::Num(Numeric::U32($v)), Span::new($start, $end)));
        ($v:literal: u64, $start:expr, $end:expr) => ((Token::Num(Numeric::U64($v)), Span::new($start, $end)));
        ($v:literal: r32, $start:expr, $end:expr) => ((Token::Num(Numeric::R32($v)), Span::new($start, $end)));
        ($v:literal: r64, $start:expr, $end:expr) => ((Token::Num(Numeric::R64($v)), Span::new($start, $end)));
    }
    macro_rules! label {
        ($val: expr, $start_id: expr, $end_id: expr) => ((Token::Label($val), Span::new($start_id, $end_id)))
    }
    macro_rules! kw {
        ($val: expr, $start_id: expr, $end_id: expr) => ((Token::Keyword($val), Span::new($start_id, $end_id)))
    }
    macro_rules! ident {
        ($name: expr, $start_id: expr, $end_id: expr) => ((Token::Ident($name), Span::new($start_id, $end_id)))
    }
    macro_rules! sep {
        ($sep: expr, $start_id: expr, $end_id: expr) => ((Token::Sep($sep), Span::new($start_id, $end_id)))
    }


    // keyword, identifier, bool lit, num lit, separator
    // byte      0         1          2         3   
    // byte      01234567890123 456789012345678901234 5678
    test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", [Span::new(4, 4), Span::new(18, 18), Span::new(35, 38)] expect vec![      
        kw!(Keyword::Var, 0, 2),
        ident!(IsId::new(2), 4, 4),
        sep!(Separator::Eq, 6, 6),
        lit!(true: bool, 8, 11),
        sep!(Separator::SemiColon, 12, 12),
        kw!(Keyword::Var, 14, 16),
        ident!(IsId::new(3), 18, 18),
        sep!(Separator::Eq, 20, 20),
        lit!(789123.4560000001: r64, 22, 32),
        sep!(Separator::SemiColon, 33, 33),
        ident!(IsId::new(4), 35, 38),
    ]}

    //           0       1      2       3
    //           0 3 67890123 690123 4 9012
    test_case!{ "一个chinese变量, a_中文_var", [Span::new(0, 16), Span::new(21, 32)] expect vec![  // chinese ident
        ident!(IsId::new(2), 0, 16),
        sep!(Separator::Comma, 19, 19),
        ident!(IsId::new(3), 21, 32),
    ]}

    // different postfix\types of num lit, different types of sep
    //           0         1         2         3         4         5         6         7
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345
    test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123r32 += 123.0 / 123u8 && 1024u8]", [] expect vec![  
        sep!(Separator::LeftBracket, 0, 0),
        lit!(1: i32, 1, 1),
        sep!(Separator::Comma, 2, 2),
        lit!(123: i32, 4, 6),
        kw!(Keyword::Underscore, 8, 8),
        lit!(1: u64, 10, 13),
        sep!(Separator::LeftParen, 14, 14),
        lit!(123.456: r64, 16, 22),
        sep!(Separator::Comma, 23, 23),
        sep!(Separator::RightParen, 24, 24),
        sep!(Separator::Sub, 26, 26),
        lit!(123456: i32, 27, 33),
        sep!(Separator::LeftBrace, 34, 34),
        lit!(123: u32, 35, 40),
        sep!(Separator::RightBrace, 41, 41),
        lit!(123.0: r32, 42, 47),
        sep!(Separator::AddEq, 49, 50),
        lit!(123.0: r64, 52, 56),
        sep!(Separator::Div, 58, 58),
        lit!(123: u8, 60, 64),
        sep!(Separator::AndAnd, 66, 67),
        lit!(0: i32, 69, 74),
        sep!(Separator::RightBracket, 75, 75),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::IntegralOverflow),
            vec![(Span::new(69, 74), String::new())],
            vec![strings::IntegralOverflowHelpMaxValue[1].to_owned()]
        ),
    ]}

    // differnt prefix\base of num lit
    //           0         1         2         3         4         5         6         7         8
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", [] expect vec![
        sep!(Separator::LeftBracket, 0, 0),
        lit!(123: i32, 1, 3),
        sep!(Separator::Mul, 5, 5),
        lit!(0x123: i32, 7, 11),
        sep!(Separator::Sub, 13, 13),
        lit!(0xAFF: i32, 15, 19),
        sep!(Separator::And, 21, 21),
        lit!(0o777: i32, 23, 27),
        sep!(Separator::OrOr, 29, 30),
        lit!(0: i32, 32, 36),
        sep!(Separator::NotEq, 38, 39),
        lit!(0b101010: i32, 41, 48),
        sep!(Separator::EqEq, 50, 51),
        lit!(0: i32, 53, 60),
        sep!(Separator::Arrow, 62, 63),
        lit!(123: i32, 65, 69),
        sep!(Separator::DotDot, 70, 71),
        lit!(0: i32, 73, 77),
        sep!(Separator::RightBracket, 78, 78),
        sep!(Separator::DotDot, 80, 81),
        sep!(Separator::SubEq, 83, 84),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(32, 36), String::new())],
            vec![strings::IntLiteralAllowedChars[1].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(53, 60), String::new())],
            vec![strings::IntLiteralAllowedChars[0].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(73, 77), String::new())],
            vec![strings::IntLiteralAllowedChars[2].to_owned()]
        ),
    ]}

    //           0       1        2
    //           012345 8901234578 123
    test_case!{ "[1, 2，3.5, 4。5】<<=", [] expect vec![  // not ascii char hint and recover
        sep!(Separator::LeftBracket, 0, 0),
        lit!(1: i32, 1, 1),
        sep!(Separator::Comma, 2, 2),
        lit!(2: i32, 4, 4),
        sep!(Separator::Comma, 5, 5),
        lit!(3.5: r64, 8, 10),
        sep!(Separator::Comma, 11, 11),
        lit!(4.5: r64, 13, 17),
        sep!(Separator::RightBracket, 18, 18),
        sep!(Separator::LtLtEq, 21, 23),
    ], make_messages![
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(5, 5), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ',', "Comma", '，', "Fullwidth Comma"),
        ]),
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(14, 14), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
        ]),
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(18, 18), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ']', "Right Square Bracket", '】', "Right Black Lenticular Bracket"),
        ]),
    ]}

    //           012345678
    test_case!{ "1..2.0r32", [] expect vec![  // range operator special case
        lit!(1: i32, 0, 0),
        sep!(Separator::DotDot, 1, 2),
        lit!(2.0: r32, 3, 8),
    ]}

    //           01234
    test_case!{ "2...3", [] expect vec![  // range operator special case 2
        lit!(2: i32, 0, 0),
        sep!(Separator::DotDot, 1, 2),
        sep!(Separator::Dot, 3, 3),
        lit!(3: i32, 4, 4),
    ]}

    //           0         1         2         
    //           012345678901234567890123456789
    test_case!{ "1.is_odd(), 123r64.to_string()", [Span::new(2, 7), Span::new(19, 27)] expect vec![  //  another special case
        lit!(1: i32, 0, 0),
        sep!(Separator::Dot, 1, 1),
        ident!(IsId::new(2), 2, 7),
        sep!(Separator::LeftParen, 8, 8),
        sep!(Separator::RightParen, 9, 9),
        sep!(Separator::Comma, 10, 10),
        lit!(123.0: r64, 12, 17),
        sep!(Separator::Dot, 18, 18),
        ident!(IsId::new(3), 19, 27),
        sep!(Separator::LeftParen, 28, 28),
        sep!(Separator::RightParen, 29, 29),
    ]}

    //           0           1          2
    //           01 234567 890 1234567890123456
    test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", [], ["hello"] expect vec![    // dispatch v1
        lit!(2: rstr, 0, 7),
        lit!('\u{1234}': char, 9, 16),
        lit!(12: i32, 18, 19),
        lit!(34: i32, 24, 25),
    ]}

    // bug from syntax::expr::postfix_expr
    //           0         1         2         3         4         5
    //           012345678901234567890123456789012345678901234567890123
    test_case!{ "1.a[[3](4, [5, 6], )](7, 8)() and[i32].bcd[10, 11, 12]", [Span::new(2, 2), Span::new(39, 41)] expect vec![
        lit!(1: i32, 0, 0),
        sep!(Separator::Dot, 1, 1),
        ident!(IsId::new(2), 2, 2),
        sep!(Separator::LeftBracket, 3, 3),
        sep!(Separator::LeftBracket, 4, 4),
        lit!(3: i32, 5, 5),
        sep!(Separator::RightBracket, 6, 6),
        sep!(Separator::LeftParen, 7, 7),
        lit!(4: i32, 8, 8),
        sep!(Separator::Comma, 9, 9),
        sep!(Separator::LeftBracket, 11, 11),
        lit!(5: i32, 12, 12),
        sep!(Separator::Comma, 13, 13),
        lit!(6: i32, 15, 15),
        sep!(Separator::RightBracket, 16, 16),
        sep!(Separator::Comma, 17, 17),
        sep!(Separator::RightParen, 19, 19),
        sep!(Separator::RightBracket, 20, 20),
        sep!(Separator::LeftParen, 21, 21),
        lit!(7: i32, 22, 22),
        sep!(Separator::Comma, 23, 23),
        lit!(8: i32, 25, 25),
        sep!(Separator::RightParen, 26, 26),
        sep!(Separator::LeftParen, 27, 27),
        sep!(Separator::RightParen, 28, 28),
        kw!(Keyword::And, 30, 32),
        sep!(Separator::LeftBracket, 33, 33),
        kw!(Keyword::I32, 34, 36),
        sep!(Separator::RightBracket, 37, 37),
        sep!(Separator::Dot, 38, 38),
        ident!(IsId::new(3), 39, 41),
        sep!(Separator::LeftBracket, 42, 42),
        lit!(10: i32, 43, 44),
        sep!(Separator::Comma, 45, 45),
        lit!(11: i32, 47, 48),
        sep!(Separator::Comma, 49, 49),
        lit!(12: i32, 51, 52),
        sep!(Separator::RightBracket, 53, 53),
    ], make_messages![
        Message::new(format!("{}: {:?}", strings::UseReservedKeyword, Keyword::And), vec![(Span::new(30, 32), String::new())]),
    ]}

    //           0         1     
    //           0123456789012345678
    test_case!{ "abc @abc @ @@ 1 @a", [Span::new(0, 2), Span::new(12, 12), Span::new(17, 17)], [""] expect vec![
        ident!(IsId::new(2), 0, 2),  // yeah
        label!(IsId::new(2), 4, 7),  // yeah
        label!(IsId::new(1), 9, 9),
        label!(IsId::new(3), 11, 12),
        lit!(1: i32, 14, 14),
        label!(IsId::new(4), 16, 17),
    ]}

    test_case!{ "@", [] expect vec![
        label!(IsId::new(1), 0, 0),
    ]}

    test_case!{ "a:", [Span::new(0, 0)] expect vec![
        ident!(IsId::new(2), 0, 0),
        sep!(Separator::Colon, 1, 1),
    ]}
    test_case!{ "@: {}", [] expect vec![
        label!(IsId::new(1), 0, 0),
        sep!(Separator::Colon, 1, 1),
        sep!(Separator::LeftBrace, 3, 3),
        sep!(Separator::RightBrace, 4, 4),
    ]}
}
