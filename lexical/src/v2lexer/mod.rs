///! fff-lang
///! 
///! lexical/v2, input v1, output string or numeric literal, identifier or other char

mod num_lit_parser;
mod unicode_char;
mod error_strings;

use codemap::Span;
use codemap::CodeChars;
use codemap::EOFCHAR;
use message::Message;
use message::MessageCollection;

use super::v1lexer::V1Token;
use super::v1lexer::V1Lexer;

use super::buf_lexer::ILexer;
use super::buf_lexer::BufLexer;

use super::LitValue;
use super::KeywordKind;
use super::SeperatorKind;
use self::num_lit_parser::parse_numeric_literal;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum V2Token {
    Literal(LitValue),
    Identifier(String), // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Label(String),      // Anything of @[_a-zA-Z0-9@]*
    Keyword(KeywordKind),
    Seperator(SeperatorKind),
    EOF,
    EOFs,
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V2Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V2Token::Literal(ref value) => write!(f, "{:?}", value),
            V2Token::Identifier(ref value) => write!(f, "Identifier {:?}", value),
            V2Token::Label(ref value) => write!(f, "Lable {:?}", value),
            V2Token::Keyword(ref kind) => write!(f, "Keyword {:?}", kind),
            V2Token::Seperator(ref kind) => write!(f, "Seperator {:?}", kind),
            V2Token::EOF => write!(f, "EOF"),
            V2Token::EOFs => write!(f, "EOFs"),
        }
    }
}

trait IdentifierChar {

    fn is_identifier_start(&self) -> bool;
    fn is_identifier(&self) -> bool;

    fn is_label_start(&self) -> bool;
    fn is_label(&self) -> bool;

    fn is_numeric_literal_start(&self) -> bool;
    fn is_numeric_literal(&self) -> bool;

    fn is_seperator(&self) -> bool;
    
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

    fn is_seperator(&self) -> bool {
        !self.is_identifier()
    }

    fn pass_non_ascii_char(&self, strpos: Span, messages: &mut MessageCollection) -> char {
        use self::unicode_char::check_unicode_char;

        match check_unicode_char(*self) {
            Some((unicode_ch, unicode_name, ascii_ch, ascii_name)) => {
                messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
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

pub struct V2Lexer<'chs> {
    v1: BufLexer<V1Lexer<'chs>, V1Token>,
}
impl<'chs> ILexer<'chs, V2Token> for V2Lexer<'chs> {

    fn new(content_chars: CodeChars<'chs>, messages: &mut MessageCollection) -> V2Lexer<'chs> {
        V2Lexer { 
            v1: BufLexer::new(content_chars, messages),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageCollection) -> (V2Token, Span) {

        #[cfg(feature = "trace_v2_parse")] macro_rules! trace { ($($arg:tt)*) => ({ print!("[V2Next:{}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_v2_parse"))] macro_rules! trace { ($($arg:tt)*) => () }

        macro_rules! ident_to_v2 { ($ident_value: expr, $ident_pos: expr) => ({
            match KeywordKind::try_from(&$ident_value) { 
                Some(KeywordKind::True) => V2Token::Literal(LitValue::from(true)),
                Some(KeywordKind::False) => V2Token::Literal(LitValue::from(false)),
                Some(other_keyword) => {
                    if other_keyword.is_reserved() {
                        messages.push(Message::new(
                            format!("{}: {:?}", error_strings::UseReservedKeyword, KeywordKind::As), 
                            vec![($ident_pos, String::new())]
                        ));
                    }
                    V2Token::Keyword(other_keyword)
                }
                None => V2Token::Identifier($ident_value),
            }
        }) }
        macro_rules! num_lit_to_v2 { ($num_lit_value: expr, $num_lit_strpos: expr) => ({
            let (num_lit_val, pos) = parse_numeric_literal($num_lit_value, $num_lit_strpos, messages);
            (V2Token::Literal(LitValue::Num(num_lit_val)), pos)
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
            self.v1.move_next(messages);
            let v15 = match self.v1.current_with_preview2() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::CharLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Char(value.clone())), pos);
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
                    return (V2Token::EOF, eof_pos);
                }
                (&V1Token::EOFs, eofs_pos, _2, _3, _4, _5) => {
                    return (V2Token::EOFs, eofs_pos);
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, &V1Token::Other(nextnext_ch), nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, messages); // not need check next_ch and nextnext_ch because they will be checked in next loops
                    V15Token(ch, strpos, next_ch, next_strpos, nextnext_ch, nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, _4, nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    V15Token(ch, strpos, next_ch, next_strpos, ' ', nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, &V1Token::EOF, eof_strpos, _4, nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    V15Token(ch, strpos, EOFCHAR, eof_strpos, ' ', nextnext_strpos)
                } 
                (&V1Token::Other(ch), strpos, _2, next_strpos, _4, nextnext_strpos) => { 
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    V15Token(ch, strpos, ' ', next_strpos, ' ', nextnext_strpos)
                }
            };

            match (state, v15) {
                (State::Nothing, V15Token(ch, strpos, EOFCHAR, _3, _4, _5)) => {
                    if ch.is_identifier_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return (ident_to_v2!(value, strpos), strpos);
                    } else if ch.is_numeric_literal_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return num_lit_to_v2!(value, strpos);
                    } else if ch.is_label_start() {
                        return (V2Token::Label("".to_owned()), strpos); // simple '@' is allowed
                    } else {
                        match SeperatorKind::try_from1(ch) {
                            Some((seperator, _)) => return (V2Token::Seperator(seperator), strpos),
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
                            return (V2Token::Label(String::new()), strpos);
                        }
                        state = State::InLabel(String::new(), strpos);
                    } else {
                        match SeperatorKind::try_from3(ch, next_ch, nextnext_ch) { // the try_from3 will check 3, if not, check 2, if not, check 1
                            Some((seperator, 1)) => {
                                trace!("ch is {:?} at {:?}, result is {:?}", ch, strpos, seperator);
                                return (V2Token::Seperator(seperator), strpos);
                            }
                            Some((seperator, 2)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, result is {:?}", ch, strpos, next_ch, seperator);
                                self.v1.prepare_skip1();
                                return (V2Token::Seperator(seperator), strpos.merge(&next_strpos));
                            }
                            Some((seperator, 3)) => {
                                trace!("ch is {:?} at {:?}, next_ch is {:?}, nextnext_ch is {:?}, result is {:?}", ch, strpos, next_ch, nextnext_ch, seperator);
                                self.v1.prepare_skip1();
                                self.v1.prepare_skip1();
                                return (V2Token::Seperator(seperator), strpos.merge(&nextnext_strpos));
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
                        ident_strpos = ident_strpos.merge(&strpos);
                        return (ident_to_v2!(value, ident_strpos), ident_strpos);
                    } else {
                        value.push(ch);
                        ident_strpos = ident_strpos.merge(&strpos);
                        state = State::InIdent(value, ident_strpos);
                    }
                }
                (State::InLabel(mut value, mut label_strpos), V15Token(ch, strpos, next_ch, _4, _5, _6)) => {
                    if !ch.is_label() {
                        return (V2Token::Label(value), label_strpos);
                    } else if !next_ch.is_label() {
                        value.push(ch);
                        label_strpos = label_strpos.merge(&strpos);
                        return (V2Token::Label(value), label_strpos);
                    } else {
                        value.push(ch);
                        label_strpos = label_strpos.merge(&strpos);
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
                        num_lit_strpos = num_lit_strpos.merge(&strpos);
                        return num_lit_to_v2!(value, num_lit_strpos);
                    } else {
                        value.push(ch);
                        num_lit_strpos = num_lit_strpos.merge(&strpos);
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
    
    assert_eq!('a'.is_seperator(), false);
    assert_eq!('啊'.is_seperator(), false);
    assert_eq!(','.is_seperator(), true);
    assert_eq!('，'.is_seperator(), true);
    assert_eq!('_'.is_seperator(), false);
    assert_eq!('.'.is_seperator(), true);
    assert_eq!('1'.is_seperator(), false);
}

#[cfg(test)] #[test]
fn v2_non_ascii_ch() {
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('.'.pass_non_ascii_char(make_span!(4, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('\\'.pass_non_ascii_char(make_span!(4, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!(';'.pass_non_ascii_char(make_span!(4, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('。'.pass_non_ascii_char(make_span!(4, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('⧹'.pass_non_ascii_char(make_span!(4, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '\\', "Backslash", '⧹', "Big Reverse Solidus"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('；'.pass_non_ascii_char(make_span!(4, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(4, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ';', "Semicolon", '；', "Fullwidth Semicolon"),
        ]));
        assert_eq!(messages, expect_messages);
    }
}

#[cfg(test)]
#[test]
fn v2_base() {
    use codemap::CodeMap;

    // Only to make decltype(V2Lexer as BufLexer::next(...)) to display better
    #[derive(Eq, PartialEq)]
    struct V2AndStrPos(V2Token, Span);
    use std::fmt;
    impl From<(V2Token, Span)> for V2AndStrPos {
        fn from(v2_and_strpos: (V2Token, Span)) -> V2AndStrPos {
            V2AndStrPos(v2_and_strpos.0, v2_and_strpos.1)
        }
    }
    impl fmt::Debug for V2AndStrPos {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "\n({:?}, {:?})", self.0, self.1)
        }
    }

    macro_rules! test_case {
        ($program: expr, $eof_pos: expr, [$($expect: expr, )*], $expect_msgs: expr) => ({
            let messages = &mut MessageCollection::new();
            let codemap = CodeMap::with_test_str($program);
            let mut v2lexer = V2Lexer::new(codemap.iter(), messages);
            let mut v2s = Vec::new();
            loop {
                match v2lexer.next(messages) {
                    (V2Token::EOFs, _) => break,
                    v2 => v2s.push(V2AndStrPos::from(v2)),
                }
            }

            let mut expect_v2s = vec![$(V2AndStrPos::from($expect), )*];
            expect_v2s.push(V2AndStrPos::from((V2Token::EOF, $eof_pos)));
            assert_eq!(v2s, expect_v2s, "Case {:?}\n", $program);

            assert_eq!(messages, &$expect_msgs, "Case {:?}\n", $program);
        });
        ($program: expr, $eof_pos: expr, [$($expect: expr, )*]) => (test_case!($program, $eof_pos, [$($expect, )*], make_messages![]))
    }

    macro_rules! lit {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Literal(LitValue::from($val)), make_span!($start_id, $end_id)))
    }
    macro_rules! lit_num_none {
        ($start_id: expr, $end_id: expr) => ((V2Token::Literal(LitValue::Num(None)), make_span!($start_id, $end_id)))
    }
    macro_rules! label {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Label($val.to_owned()), make_span!($start_id, $end_id)))
    }
    macro_rules! kw {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Keyword($val), make_span!($start_id, $end_id)))
    }
    macro_rules! ident {
        ($name: expr, $start_id: expr, $end_id: expr) => ((V2Token::Identifier($name.to_owned()), make_span!($start_id, $end_id)))
    }
    macro_rules! sep {
        ($sep: expr, $start_id: expr, $end_id: expr) => ((V2Token::Seperator($sep), make_span!($start_id, $end_id)))
    }

    // row       1              2                     3
    // col       0        1     0        1         2  0
    // col       1234567890123 412345678901234567890 11234
    // byte      0         1          2         3   
    // byte      01234567890123 456789012345678901234 5678
    test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", make_span!(39, 39), [  // keyword, identifier, bool lit, num lit, seperator
        kw!(KeywordKind::Var, 0, 2),
        ident!("a", 4, 4),
        sep!(SeperatorKind::Assign, 6, 6),
        lit!(true, 8, 11),
        sep!(SeperatorKind::SemiColon, 12, 12),
        kw!(KeywordKind::Var, 14, 16),
        ident!("b", 18, 18),
        sep!(SeperatorKind::Assign, 20, 20),
        lit!(789123.4560000001, 22, 32),
        sep!(SeperatorKind::SemiColon, 33, 33),
        ident!("defg", 35, 38),
    ]}

    //           0          1            2
    //           1 2 34567890 123456 7 8901
    //           0       1      2       3
    //           0 3 67890123 690123 4 9012
    test_case!{ "一个chinese变量, a_中文_var", make_span!(33, 33), [  // chinese ident
        ident!("一个chinese变量", 0, 16),
        sep!(SeperatorKind::Comma, 19, 19),
        ident!("a_中文_var", 21, 32),
    ]}

    //           0         1         2         3         4         5         6         7
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345
    test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123f32 += 123.0 / 123u8 && 1024u8]", make_span!(76, 76), [  // different postfix\types of num lit, different types of sep
        sep!(SeperatorKind::LeftBracket, 0, 0),
        lit!(1, 1, 1),
        sep!(SeperatorKind::Comma, 2, 2),
        lit!(123, 4, 6),
        kw!(KeywordKind::Underscore, 8, 8),
        lit!(1u64, 10, 13),
        sep!(SeperatorKind::LeftParenthenes, 14, 14),
        lit!(123.456, 16, 22),
        sep!(SeperatorKind::Comma, 23, 23),
        sep!(SeperatorKind::RightParenthenes, 24, 24),
        sep!(SeperatorKind::Sub, 26, 26),
        lit!(123456, 27, 33),
        sep!(SeperatorKind::LeftBrace, 34, 34),
        lit!(123u32, 35, 40),
        sep!(SeperatorKind::RightBrace, 41, 41),
        lit!(123f32, 42, 47),
        sep!(SeperatorKind::AddAssign, 49, 50),
        lit!(123.0, 52, 56),
        sep!(SeperatorKind::Div, 58, 58),
        lit!(123u8, 60, 64),
        sep!(SeperatorKind::LogicalAnd, 66, 67),
        lit_num_none!(69, 74),
        sep!(SeperatorKind::RightBracket, 75, 75),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::IntegralOverflow),
            vec![(make_span!(69, 74), String::new())],
            vec![error_strings::IntegralOverflowHelpMaxValue[1].to_owned()]
        ),
    ]}

    //           0         1         2         3         4         5         6         7         8
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", make_span!(85, 85), [    // differnt prefix\base of num lit
        sep!(SeperatorKind::LeftBracket, 0, 0),
        lit!(123, 1, 3),
        sep!(SeperatorKind::Mul, 5, 5),
        lit!(0x123, 7, 11),
        sep!(SeperatorKind::Sub, 13, 13),
        lit!(0xAFF, 15, 19),
        sep!(SeperatorKind::BitAnd, 21, 21),
        lit!(0o777, 23, 27),
        sep!(SeperatorKind::LogicalOr, 29, 30),
        lit_num_none!(32, 36),
        sep!(SeperatorKind::NotEqual, 38, 39),
        lit!(0b101010, 41, 48),
        sep!(SeperatorKind::Equal, 50, 51),
        lit_num_none!(53, 60),
        sep!(SeperatorKind::NarrowRightArrow, 62, 63),
        lit!(123, 65, 69),
        sep!(SeperatorKind::Range, 70, 71),
        lit_num_none!(73, 77),
        sep!(SeperatorKind::RightBracket, 78, 78),
        sep!(SeperatorKind::Range, 80, 81),
        sep!(SeperatorKind::SubAssign, 83, 84),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
            vec![(make_span!(32, 36), String::new())],
            vec![error_strings::IntLiteralAllowedChars[1].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
            vec![(make_span!(53, 60), String::new())],
            vec![error_strings::IntLiteralAllowedChars[0].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
            vec![(make_span!(73, 77), String::new())],
            vec![error_strings::IntLiteralAllowedChars[2].to_owned()]
        ),
    ]}

    //           0       1        2
    //           012345 8901234578 123
    test_case!{ "[1, 2，3.5, 4。5】<<=", make_span!(24, 24), [  // not ascii char hint and recover
        sep!(SeperatorKind::LeftBracket, 0, 0),
        lit!(1, 1, 1),
        sep!(SeperatorKind::Comma, 2, 2),
        lit!(2, 4, 4),
        sep!(SeperatorKind::Comma, 5, 5),
        lit!(3.5, 8, 10),
        sep!(SeperatorKind::Comma, 11, 11),
        lit!(4.5, 13, 17),
        sep!(SeperatorKind::RightBracket, 18, 18),
        sep!(SeperatorKind::ShiftLeftAssign, 21, 23),
    ], make_messages![
        Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(5, 5), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ',', "Comma", '，', "Fullwidth Comma"),
        ]),
        Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(14, 14), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
        ]),
        Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_span!(18, 18), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ']', "Right Square Bracket", '】', "Right Black Lenticular Bracket"),
        ]),
    ]}

    //           012345678
    test_case!{ "1..2.0f32", make_span!(9, 9), [  // range operator special case
        lit!(1, 0, 0),
        sep!(SeperatorKind::Range, 1, 2),
        lit!(2f32, 3, 8),
    ]}

    //           01234
    test_case!{ "2...3", make_span!(5, 5), [  // range operator special case 2
        lit!(2, 0, 0),
        sep!(SeperatorKind::Range, 1, 2),
        sep!(SeperatorKind::Dot, 3, 3),
        lit!(3, 4, 4),
    ]}

    //           0         1         2         
    //           012345678901234567890123456789
    test_case!{ "1.is_odd(), 123f64.to_string()", make_span!(30, 30), [  //  another special case
        lit!(1, 0, 0),
        sep!(SeperatorKind::Dot, 1, 1),
        ident!("is_odd", 2, 7),
        sep!(SeperatorKind::LeftParenthenes, 8, 8),
        sep!(SeperatorKind::RightParenthenes, 9, 9),
        sep!(SeperatorKind::Comma, 10, 10),
        lit!(123f64, 12, 17),
        sep!(SeperatorKind::Dot, 18, 18),
        ident!("to_string", 19, 27),
        sep!(SeperatorKind::LeftParenthenes, 28, 28),
        sep!(SeperatorKind::RightParenthenes, 29, 29),
    ]}

    //           0           1          2
    //           01 234567 890 1234567890123456
    test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", make_span!(27, 27), [    // dispatch v1
        lit!("hello", 0, 7),
        lit!('\u{1234}', 9, 16),
        lit!(12, 18, 19),
        lit!(34, 24, 25),
    ]}

    //           0         1         2         3         4         5
    //           012345678901234567890123456789012345678901234567890123
    test_case!{ "1.a[[3](4, [5, 6], )](7, 8)() as [i32].bcd[10, 11, 12]", make_span!(54, 54), [ // bug from syntax::expr::postfix_expr
        lit!(1, 0, 0),
        sep!(SeperatorKind::Dot, 1, 1),
        ident!("a", 2, 2),
        sep!(SeperatorKind::LeftBracket, 3, 3),
        sep!(SeperatorKind::LeftBracket, 4, 4),
        lit!(3, 5, 5),
        sep!(SeperatorKind::RightBracket, 6, 6),
        sep!(SeperatorKind::LeftParenthenes, 7, 7),
        lit!(4, 8, 8),
        sep!(SeperatorKind::Comma, 9, 9),
        sep!(SeperatorKind::LeftBracket, 11, 11),
        lit!(5, 12, 12),
        sep!(SeperatorKind::Comma, 13, 13),
        lit!(6, 15, 15),
        sep!(SeperatorKind::RightBracket, 16, 16),
        sep!(SeperatorKind::Comma, 17, 17),
        sep!(SeperatorKind::RightParenthenes, 19, 19),
        sep!(SeperatorKind::RightBracket, 20, 20),
        sep!(SeperatorKind::LeftParenthenes, 21, 21),
        lit!(7, 22, 22),
        sep!(SeperatorKind::Comma, 23, 23),
        lit!(8, 25, 25),
        sep!(SeperatorKind::RightParenthenes, 26, 26),
        sep!(SeperatorKind::LeftParenthenes, 27, 27),
        sep!(SeperatorKind::RightParenthenes, 28, 28),
        kw!(KeywordKind::As, 30, 31),
        sep!(SeperatorKind::LeftBracket, 33, 33),
        kw!(KeywordKind::PrimTypeI32, 34, 36),
        sep!(SeperatorKind::RightBracket, 37, 37),
        sep!(SeperatorKind::Dot, 38, 38),
        ident!("bcd", 39, 41),
        sep!(SeperatorKind::LeftBracket, 42, 42),
        lit!(10, 43, 44),
        sep!(SeperatorKind::Comma, 45, 45),
        lit!(11, 47, 48),
        sep!(SeperatorKind::Comma, 49, 49),
        lit!(12, 51, 52),
        sep!(SeperatorKind::RightBracket, 53, 53),
    ], make_messages![
        Message::new(format!("{}: {:?}", error_strings::UseReservedKeyword, KeywordKind::As), vec![(make_span!(30, 31), String::new())]), // TODO: this feature added, add to error_strings
    ]}

    //           0         1     
    //           0123456789012345678
    test_case!{ "abc @abc @ @@ 1 @a", make_span!(18, 18), [
        ident!("abc", 0, 2),
        label!("abc", 4, 7),
        label!("", 9, 9),
        label!("@", 11, 12),
        lit!(1, 14, 14),
        label!("a", 16, 17),
    ]}

    test_case!{ "@", make_span!(1, 1), [label!("", 0, 0),] }

    test_case!{ "a:", make_span!(2, 2), [
        ident!("a", 0, 0),
        sep!(SeperatorKind::Colon, 1, 1),
    ]}
    test_case!{ "@: {}", make_span!(5, 5), [
        label!("", 0, 0),
        sep!(SeperatorKind::Colon, 1, 1),
        sep!(SeperatorKind::LeftBrace, 3, 3),
        sep!(SeperatorKind::RightBrace, 4, 4),
    ]}
}
