
// Numeric Literal

use common::StringPosition;
use lexical::message::Message;
use lexical::message::MessageEmitter;

#[cfg(test)]
#[derive(PartialEq, Clone)]
pub enum NumericLiteralValue {
    Integral(u64),
    Floating(f64),
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum NumericLiteralValue {
    Integral(u64),
    Floating(f64),
}
#[cfg(test)]
impl Eq for NumericLiteralValue {

}

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct NumericLiteral {
    pub raw: String,
    pub pos: StringPosition,
    pub has_failed: bool,
    pub value: NumericLiteralValue,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct NumericLiteral {
    pub raw: String,
    pub pos: StringPosition,
    pub has_failed: bool,
    pub value: NumericLiteralValue,
}

impl NumericLiteral {

    pub fn from(raw: &str, pos: StringPosition) -> NumericLiteral {
        NumericLiteral { 
            raw: raw.to_owned(),
            pos: pos, 
            has_failed: false,
            value: NumericLiteralValue::Integral(0),
         }
    }
}

use std::fmt;
impl fmt::Debug for NumericLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NumericLiteral {:?} at {:?}{}", 
            self.raw, self.pos,
            if self.has_failed {
                ", has failed".to_owned()
            } else {
                match self.value {
                    NumericLiteralValue::Integral(ref value) => format!(", with value {:?}", value),
                    NumericLiteralValue::Floating(ref value) =>     format!(", with value {:?}", value),  
                }
            })
    }
}

// ignore _ and ignore i32 postfix
fn numeric_literal_to_value(raw: &str, pos: StringPosition, messages: &mut MessageEmitter) -> (i32, bool) { // value, has_failed
    
    let no_postfix = if raw.len() > 3 && &raw[(raw.len() - 3)..] == "i32" {
        &raw[..(raw.len() - 3)]
    } else {
        raw
    };

    let mut digits = Vec::new();
    for ch in no_postfix.chars() {
        if ch == '_' {
            continue;
        } else if ch.is_digit(10) {
            digits.push(ch);
        } else {
            messages.push(Message::UnexpectedIdentifierCharInNumericLiteral { 
                literal_start: pos.start_pos,
                unexpected_char: ch
            });
            return (0, true);
        }
    }

    // digits will not be empty because first character must be [0-9]
    if digits.len() > 10 {
        messages.push(Message::NumericLiteralTooLong { literal_start: pos.start_pos });
        return (0, true);
    }
    
    const TENS: [i32; 10] = [
        1, 
        10, 
        100, 
        1000, 
        10000,
        100000, 
        1000000,
        10000000, 
        100000000, 
        1000000000
    ];

    let mut value = 0_i32;
    let length = digits.len();
    for i in 0..length {
        value = match (digits[i].to_digit(10).unwrap() as i32).checked_mul(TENS[length - i - 1]) {
            None => {
                messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                return (0, true);
            }
            Some(middle) => {
                match value.checked_add(middle) {
                    Some(value) => value, 
                    None => {
                        messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                        return (0, true);
                    }
                }
            }
        };
    }

    (value, false)
}

#[cfg(test)]
pub fn pub_numeric_literal(raw: &str, messages: &mut MessageEmitter) -> (i32, bool) {
    numeric_literal_to_value(raw, StringPosition::new(), messages)
}

#[cfg(test)]
mod tests {

    #[test]
    fn v3_numeric_literal() {
        use super::pub_numeric_literal;
        use lexical::message::MessageEmitter;

        let messages = &mut MessageEmitter::new();
        perrorln!("{:?}", pub_numeric_literal("123", messages));
        perrorln!("{:?}", pub_numeric_literal("123_i32", messages));
        perrorln!("{:?}", pub_numeric_literal("1_2_3i32", messages));
        perrorln!("{:?}", pub_numeric_literal("123_456_789_012", messages));
        perrorln!("{:?}", pub_numeric_literal("999_999_999_9_i32", messages));

        perrorln!("Messages: {:?}", messages);
    }
}