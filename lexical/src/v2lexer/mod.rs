///! fff-lang
///! 
///! lexical/v2, input v1, output string or numeric literal, identifier or other char

mod num_lit_parser;
mod unicode_char;
mod error_strings;

use codemap::Span;
use codemap::SymbolID;
use codemap::SourceCodeIter;
use codemap::EOF_CHAR;
use message::Message;
use message::MessageCollection;

use super::v1lexer::V1Token;
use super::v1lexer::V1Lexer;

use super::ILexer;
use super::BufLexer;

use super::LitValue;
use super::Keyword;
use super::Seperator;
use super::ParseSession;
use self::num_lit_parser::parse_numeric_literal;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum V2Token {
    EOF,
    Literal(LitValue),
    Identifier(SymbolID), // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Label(SymbolID),      // Anything of @[_a-zA-Z0-9@]*
    Keyword(Keyword),
    Seperator(Seperator),
}
impl Default for V2Token { fn default() -> V2Token { V2Token::EOF } }

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

    fn new(source: SourceCodeIter<'chs>) -> V2Lexer<'chs> {
        V2Lexer { 
            v1: BufLexer::new(source),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, sess: &mut ParseSession) -> (V2Token, Span) {

        #[cfg(feature = "trace_v2_parse")] macro_rules! trace { ($($arg:tt)*) => ({ print!("[V2Next: {}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_v2_parse"))] macro_rules! trace { ($($arg:tt)*) => () }

        macro_rules! ident_to_v2 { ($ident_value: expr, $ident_pos: expr) => ({
            match Keyword::parse(&$ident_value) { 
                Some(Keyword::True) => V2Token::Literal(LitValue::from(true)),
                Some(Keyword::False) => V2Token::Literal(LitValue::from(false)),
                Some(other_keyword) => {
                    if other_keyword.is_reserved() {
                        sess.messages.push(Message::new(
                            format!("{}: {:?}", error_strings::UseReservedKeyword, other_keyword), 
                            vec![($ident_pos, String::new())]
                        ));
                    }
                    V2Token::Keyword(other_keyword)
                }
                None => V2Token::Identifier(sess.symbols.intern($ident_value)),
            }
        }) }
        macro_rules! num_lit_to_v2 { ($num_lit_value: expr, $num_lit_strpos: expr) => ({
            let (num_lit_val, pos) = parse_numeric_literal($num_lit_value, $num_lit_strpos, sess.messages);
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
            self.v1.move_next(sess);
            let v15 = match self.v1.current_with_preview2() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(*value)), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(*value)), pos);
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
                    V15Token(ch, strpos, EOF_CHAR, eof_strpos, ' ', nextnext_strpos)
                } 
                (&V1Token::Other(ch), strpos, _2, next_strpos, _4, nextnext_strpos) => { 
                    let ch = ch.pass_non_ascii_char(strpos, sess.messages);
                    V15Token(ch, strpos, ' ', next_strpos, ' ', nextnext_strpos)
                }
            };

            match (state, v15) {
                (State::Nothing, V15Token(ch, strpos, EOF_CHAR, _3, _4, _5)) => {
                    if ch.is_identifier_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return (ident_to_v2!(value, strpos), strpos);
                    } else if ch.is_numeric_literal_start() {
                        let mut value = String::new();
                        value.push(ch);
                        return num_lit_to_v2!(value, strpos);
                    } else if ch.is_label_start() {
                        return (V2Token::Label(sess.symbols.intern_str("")), strpos); // simple '@' is allowed, use @? to represent empty
                    } else {
                        match Seperator::parse1(ch) {
                            Some(seperator) => return (V2Token::Seperator(seperator), strpos),
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
                            return (V2Token::Label(sess.symbols.intern_str("")), strpos);
                        }
                        state = State::InLabel(String::new(), strpos);
                    } else {
                        match Seperator::parse3(ch, next_ch, nextnext_ch) { // the try_from3 will check 3, if not, check 2, if not, check 1
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
                        return (V2Token::Label(sess.symbols.intern(value)), label_strpos);
                    } else if !next_ch.is_label() {
                        value.push(ch);
                        label_strpos = label_strpos.merge(&strpos);
                        return (V2Token::Label(sess.symbols.intern(value)), label_strpos);
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
    use std::fmt;
    use codemap::SourceCode;
    use codemap::SymbolCollection;

    // Only to make decltype(V2Lexer as BufLexer::next(...)) to display better
    #[derive(Eq, PartialEq)]
    struct V2Span(V2Token, Span);
    impl From<(V2Token, Span)> for V2Span {
        fn from(v2_and_strpos: (V2Token, Span)) -> V2Span { V2Span(v2_and_strpos.0, v2_and_strpos.1) }
    }
    impl fmt::Debug for V2Span {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n({:?}, {:?})", self.0, self.1) }
    }
    
    fn test_case_full(src: &str, symbols: SymbolCollection, expect_tokens: Vec<(V2Token, Span)>, expect_messages: MessageCollection, line: u32) {
        println!("Case at {}", line);
        let mut actual_messages = MessageCollection::new();
        let mut symbols = symbols;
        let mut sess = ParseSession::new(&mut actual_messages, &mut symbols);
        let source = SourceCode::with_test_str(0, src);
        let mut v2lexer = V2Lexer::new(source.iter());
        for expect_token in expect_tokens {
            assert_eq!(v2lexer.next(&mut sess), expect_token);
        }
        let next = v2lexer.next(&mut sess);
        if next.0 != V2Token::EOF { panic!("next is not EOF but {:?}", next); }

        assert_eq!(sess.messages, &expect_messages);
    }

    macro_rules! test_case {
        ($src: expr, $symbols: expr, $expect_tokens: expr) => 
            (test_case_full($src, $symbols, $expect_tokens, MessageCollection::new(), line!()));
        ($src: expr, $symbols: expr, $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full($src, SymbolCollection::new(), $expect_tokens, $expect_messages, line!()));
    }

    macro_rules! lit {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Literal(LitValue::from($val)), make_span!($start_id, $end_id)))
    }
    macro_rules! lit_num_none {
        ($start_id: expr, $end_id: expr) => ((V2Token::Literal(LitValue::Num(None)), make_span!($start_id, $end_id)))
    }
    macro_rules! label {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Label($val), make_span!($start_id, $end_id)))
    }
    macro_rules! kw {
        ($val: expr, $start_id: expr, $end_id: expr) => ((V2Token::Keyword($val), make_span!($start_id, $end_id)))
    }
    macro_rules! ident {
        ($name: expr, $start_id: expr, $end_id: expr) => ((V2Token::Identifier($name), make_span!($start_id, $end_id)))
    }
    macro_rules! sep {
        ($sep: expr, $start_id: expr, $end_id: expr) => ((V2Token::Seperator($sep), make_span!($start_id, $end_id)))
    }


    // keyword, identifier, bool lit, num lit, seperator
    // byte      0         1          2         3   
    // byte      01234567890123 456789012345678901234 5678
    test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", make_symbols!["a", "b", "defg"], vec![      
        kw!(Keyword::Var, 0, 2),
        ident!(make_id!(1), 4, 4),
        sep!(Seperator::Assign, 6, 6),
        lit!(true, 8, 11),
        sep!(Seperator::SemiColon, 12, 12),
        kw!(Keyword::Var, 14, 16),
        ident!(make_id!(2), 18, 18),
        sep!(Seperator::Assign, 20, 20),
        lit!(789123.4560000001, 22, 32),
        sep!(Seperator::SemiColon, 33, 33),
        ident!(make_id!(3), 35, 38),
    ]}

    //           0       1      2       3
    //           0 3 67890123 690123 4 9012
    test_case!{ "一个chinese变量, a_中文_var", make_symbols!["一个chinese变量", "a_中文_var"], vec![  // chinese ident
        ident!(make_id!(1), 0, 16),
        sep!(Seperator::Comma, 19, 19),
        ident!(make_id!(2), 21, 32),
    ]}

    // different postfix\types of num lit, different types of sep
    //           0         1         2         3         4         5         6         7
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345
    test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123f32 += 123.0 / 123u8 && 1024u8]", make_symbols![], vec![  
        sep!(Seperator::LeftBracket, 0, 0),
        lit!(1, 1, 1),
        sep!(Seperator::Comma, 2, 2),
        lit!(123, 4, 6),
        kw!(Keyword::Underscore, 8, 8),
        lit!(1u64, 10, 13),
        sep!(Seperator::LeftParenthenes, 14, 14),
        lit!(123.456, 16, 22),
        sep!(Seperator::Comma, 23, 23),
        sep!(Seperator::RightParenthenes, 24, 24),
        sep!(Seperator::Sub, 26, 26),
        lit!(123456, 27, 33),
        sep!(Seperator::LeftBrace, 34, 34),
        lit!(123u32, 35, 40),
        sep!(Seperator::RightBrace, 41, 41),
        lit!(123f32, 42, 47),
        sep!(Seperator::AddAssign, 49, 50),
        lit!(123.0, 52, 56),
        sep!(Seperator::Div, 58, 58),
        lit!(123u8, 60, 64),
        sep!(Seperator::LogicalAnd, 66, 67),
        lit_num_none!(69, 74),
        sep!(Seperator::RightBracket, 75, 75),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::IntegralOverflow),
            vec![(make_span!(69, 74), String::new())],
            vec![error_strings::IntegralOverflowHelpMaxValue[1].to_owned()]
        ),
    ]}

    // differnt prefix\base of num lit
    //           0         1         2         3         4         5         6         7         8
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", make_symbols![], vec![
        sep!(Seperator::LeftBracket, 0, 0),
        lit!(123, 1, 3),
        sep!(Seperator::Mul, 5, 5),
        lit!(0x123, 7, 11),
        sep!(Seperator::Sub, 13, 13),
        lit!(0xAFF, 15, 19),
        sep!(Seperator::BitAnd, 21, 21),
        lit!(0o777, 23, 27),
        sep!(Seperator::LogicalOr, 29, 30),
        lit_num_none!(32, 36),
        sep!(Seperator::NotEqual, 38, 39),
        lit!(0b101010, 41, 48),
        sep!(Seperator::Equal, 50, 51),
        lit_num_none!(53, 60),
        sep!(Seperator::NarrowRightArrow, 62, 63),
        lit!(123, 65, 69),
        sep!(Seperator::Range, 70, 71),
        lit_num_none!(73, 77),
        sep!(Seperator::RightBracket, 78, 78),
        sep!(Seperator::Range, 80, 81),
        sep!(Seperator::SubAssign, 83, 84),
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
    test_case!{ "[1, 2，3.5, 4。5】<<=", make_symbols![], vec![  // not ascii char hint and recover
        sep!(Seperator::LeftBracket, 0, 0),
        lit!(1, 1, 1),
        sep!(Seperator::Comma, 2, 2),
        lit!(2, 4, 4),
        sep!(Seperator::Comma, 5, 5),
        lit!(3.5, 8, 10),
        sep!(Seperator::Comma, 11, 11),
        lit!(4.5, 13, 17),
        sep!(Seperator::RightBracket, 18, 18),
        sep!(Seperator::ShiftLeftAssign, 21, 23),
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
    test_case!{ "1..2.0f32", make_symbols![], vec![  // range operator special case
        lit!(1, 0, 0),
        sep!(Seperator::Range, 1, 2),
        lit!(2f32, 3, 8),
    ]}

    //           01234
    test_case!{ "2...3", make_symbols![], vec![  // range operator special case 2
        lit!(2, 0, 0),
        sep!(Seperator::Range, 1, 2),
        sep!(Seperator::Dot, 3, 3),
        lit!(3, 4, 4),
    ]}

    //           0         1         2         
    //           012345678901234567890123456789
    test_case!{ "1.is_odd(), 123f64.to_string()", make_symbols!["is_odd", "to_string"], vec![  //  another special case
        lit!(1, 0, 0),
        sep!(Seperator::Dot, 1, 1),
        ident!(make_id!(1), 2, 7),
        sep!(Seperator::LeftParenthenes, 8, 8),
        sep!(Seperator::RightParenthenes, 9, 9),
        sep!(Seperator::Comma, 10, 10),
        lit!(123f64, 12, 17),
        sep!(Seperator::Dot, 18, 18),
        ident!(make_id!(2), 19, 27),
        sep!(Seperator::LeftParenthenes, 28, 28),
        sep!(Seperator::RightParenthenes, 29, 29),
    ]}

    //           0           1          2
    //           01 234567 890 1234567890123456
    test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", make_symbols!["hello"], vec![    // dispatch v1
        (V2Token::Literal(LitValue::Str(Some(make_id!(1)))), make_span!(0, 7)), // because `lit!(make_id!(1), 0, 7)` is ambiguous
        lit!('\u{1234}', 9, 16),
        lit!(12, 18, 19),
        lit!(34, 24, 25),
    ]}

    // bug from syntax::expr::postfix_expr
    //           0         1         2         3         4         5
    //           012345678901234567890123456789012345678901234567890123
    test_case!{ "1.a[[3](4, [5, 6], )](7, 8)() as [i32].bcd[10, 11, 12]", make_symbols!["a", "bcd"], vec![
        lit!(1, 0, 0),
        sep!(Seperator::Dot, 1, 1),
        ident!(make_id!(1), 2, 2),
        sep!(Seperator::LeftBracket, 3, 3),
        sep!(Seperator::LeftBracket, 4, 4),
        lit!(3, 5, 5),
        sep!(Seperator::RightBracket, 6, 6),
        sep!(Seperator::LeftParenthenes, 7, 7),
        lit!(4, 8, 8),
        sep!(Seperator::Comma, 9, 9),
        sep!(Seperator::LeftBracket, 11, 11),
        lit!(5, 12, 12),
        sep!(Seperator::Comma, 13, 13),
        lit!(6, 15, 15),
        sep!(Seperator::RightBracket, 16, 16),
        sep!(Seperator::Comma, 17, 17),
        sep!(Seperator::RightParenthenes, 19, 19),
        sep!(Seperator::RightBracket, 20, 20),
        sep!(Seperator::LeftParenthenes, 21, 21),
        lit!(7, 22, 22),
        sep!(Seperator::Comma, 23, 23),
        lit!(8, 25, 25),
        sep!(Seperator::RightParenthenes, 26, 26),
        sep!(Seperator::LeftParenthenes, 27, 27),
        sep!(Seperator::RightParenthenes, 28, 28),
        kw!(Keyword::As, 30, 31),
        sep!(Seperator::LeftBracket, 33, 33),
        kw!(Keyword::I32, 34, 36),
        sep!(Seperator::RightBracket, 37, 37),
        sep!(Seperator::Dot, 38, 38),
        ident!(make_id!(2), 39, 41),
        sep!(Seperator::LeftBracket, 42, 42),
        lit!(10, 43, 44),
        sep!(Seperator::Comma, 45, 45),
        lit!(11, 47, 48),
        sep!(Seperator::Comma, 49, 49),
        lit!(12, 51, 52),
        sep!(Seperator::RightBracket, 53, 53),
    ], make_messages![
        Message::new(format!("{}: {:?}", error_strings::UseReservedKeyword, Keyword::As), vec![(make_span!(30, 31), String::new())]), // TODO: this feature added, add to error_strings
    ]}

    //           0         1     
    //           0123456789012345678
    test_case!{ "abc @abc @ @@ 1 @a", make_symbols!["abc", "", "@", "a"], vec![
        ident!(make_id!(1), 0, 2),  // yeah
        label!(make_id!(1), 4, 7),  // yeah
        label!(make_id!(2), 9, 9),
        label!(make_id!(3), 11, 12),
        lit!(1, 14, 14),
        label!(make_id!(4), 16, 17),
    ]}

    test_case!{ "@", make_symbols![""], vec![label!(make_id!(1), 0, 0)] }

    test_case!{ "a:", make_symbols!["a"], vec![
        ident!(make_id!(1), 0, 0),
        sep!(Seperator::Colon, 1, 1),
    ]}
    test_case!{ "@: {}", make_symbols![""], vec![
        label!(make_id!(1), 0, 0),
        sep!(Seperator::Colon, 1, 1),
        sep!(Seperator::LeftBrace, 3, 3),
        sep!(Seperator::RightBrace, 4, 4),
    ]}
}
