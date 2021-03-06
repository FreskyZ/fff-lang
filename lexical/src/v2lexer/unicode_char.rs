// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Characters and their corresponding confusables were collected from
// http://www.unicode.org/Public/security/revision-06/confusables.txt

const UNICODE_ARRAY: &'static [(char, &'static str, char)] = &[
    (' ', "No-Break Space", ' '),
    (' ', "Ogham Space Mark", ' '),
    (' ', "En Quad", ' '),
    (' ', "Em Quad", ' '),
    (' ', "En Space", ' '),
    (' ', "Em Space", ' '),
    (' ', "Three-Per-Em Space", ' '),
    (' ', "Four-Per-Em Space", ' '),
    (' ', "Six-Per-Em Space", ' '),
    (' ', "Figure Space", ' '),
    (' ', "Punctuation Space", ' '),
    (' ', "Thin Space", ' '),
    (' ', "Hair Space", ' '),
    (' ', "Narrow No-Break Space", ' '),
    (' ', "Medium Mathematical Space", ' '),
    ('　', "Ideographic Space", ' '),
    ('ߺ', "Nko Lajanyalan", '_'),
    ('﹍', "Dashed Low Line", '_'),
    ('﹎', "Centreline Low Line", '_'),
    ('﹏', "Wavy Low Line", '_'),
    ('‐', "Hyphen", '-'),
    ('‑', "Non-Breaking Hyphen", '-'),
    ('‒', "Figure Dash", '-'),
    ('–', "En Dash", '-'),
    ('—', "Em Dash", '-'),
    ('﹘', "Small Em Dash", '-'),
    ('⁃', "Hyphen Bullet", '-'),
    ('˗', "Modifier Letter Minus Sign", '-'),
    ('−', "Minus Sign", '-'),
    ('ー', "Katakana-Hiragana Prolonged Sound Mark", '-'),
    ('٫', "Arabic Decimal Separator", ','),
    ('‚', "Single Low-9 Quotation Mark", ','),
    ('ꓹ', "Lisu Letter Tone Na Po", ','),
    ('，', "Fullwidth Comma", ','),
    (';', "Greek Question Mark", ';'),
    ('；', "Fullwidth Semicolon", ';'),
    ('ः', "Devanagari Sign Visarga", ':'),
    ('ઃ', "Gujarati Sign Visarga", ':'),
    ('：', "Fullwidth Colon", ':'),
    ('։', "Armenian Full Stop", ':'),
    ('܃', "Syriac Supralinear Colon", ':'),
    ('܄', "Syriac Sublinear Colon", ':'),
    ('︰', "Presentation Form For Vertical Two Dot Leader", ':'),
    ('᠃', "Mongolian Full Stop", ':'),
    ('᠉', "Mongolian Manchu Full Stop", ':'),
    ('⁚', "Two Dot Punctuation", ':'),
    ('׃', "Hebrew Punctuation Sof Pasuq", ':'),
    ('˸', "Modifier Letter Raised Colon", ':'),
    ('꞉', "Modifier Letter Colon", ':'),
    ('∶', "Ratio", ':'),
    ('ː', "Modifier Letter Triangular Colon", ':'),
    ('ꓽ', "Lisu Letter Tone Mya Jeu", ':'),
    ('！', "Fullwidth Exclamation Mark", '!'),
    ('ǃ', "Latin Letter Retroflex Click", '!'),
    ('ʔ', "Latin Letter Glottal Stop", '?'),
    ('ॽ', "Devanagari Letter Glottal Stop", '?'),
    ('Ꭾ', "Cherokee Letter He", '?'),
    ('？', "Fullwidth Question Mark", '?'),
    ('𝅭', "Musical Symbol Combining Augmentation Dot", '.'),
    ('․', "One Dot Leader", '.'),
    ('۔', "Arabic Full Stop", '.'),
    ('܁', "Syriac Supralinear Full Stop", '.'),
    ('܂', "Syriac Sublinear Full Stop", '.'),
    ('꘎', "Vai Full Stop", '.'),
    ('𐩐', "Kharoshthi Punctuation Dot", '.'),
    ('·', "Middle Dot", '.'),
    ('٠', "Arabic-Indic Digit Zero", '.'),
    ('۰', "Extended Arabic-Indic Digit Zero", '.'),
    ('ꓸ', "Lisu Letter Tone Mya Ti", '.'),
    ('。', "Ideographic Full Stop", '.'),
    ('・', "Katakana Middle Dot", '.'),
    ('՝', "Armenian Comma", '\''),
    ('＇', "Fullwidth Apostrophe", '\''),
    ('‘', "Left Single Quotation Mark", '\''),
    ('’', "Right Single Quotation Mark", '\''),
    ('‛', "Single High-Reversed-9 Quotation Mark", '\''),
    ('′', "Prime", '\''),
    ('‵', "Reversed Prime", '\''),
    ('՚', "Armenian Apostrophe", '\''),
    ('׳', "Hebrew Punctuation Geresh", '\''),
    ('`', "Greek Varia", '\''),
    ('｀', "Fullwidth Grave Accent", '\''),
    ('΄', "Greek Tonos", '\''),
    ('´', "Greek Oxia", '\''),
    ('᾽', "Greek Koronis", '\''),
    ('᾿', "Greek Psili", '\''),
    ('῾', "Greek Dasia", '\''),
    ('ʹ', "Modifier Letter Prime", '\''),
    ('ʹ', "Greek Numeral Sign", '\''),
    ('ˊ', "Modifier Letter Acute Accent", '\''),
    ('ˋ', "Modifier Letter Grave Accent", '\''),
    ('˴', "Modifier Letter Middle Grave Accent", '\''),
    ('ʻ', "Modifier Letter Turned Comma", '\''),
    ('ʽ', "Modifier Letter Reversed Comma", '\''),
    ('ʼ', "Modifier Letter Apostrophe", '\''),
    ('ʾ', "Modifier Letter Right Half Ring", '\''),
    ('ꞌ', "Latin Small Letter Saltillo", '\''),
    ('י', "Hebrew Letter Yod", '\''),
    ('ߴ', "Nko High Tone Apostrophe", '\''),
    ('ߵ', "Nko Low Tone Apostrophe", '\''),
    ('＂', "Fullwidth Quotation Mark", '"'),
    ('“', "Left Double Quotation Mark", '"'),
    ('”', "Right Double Quotation Mark", '"'),
    ('‟', "Double High-Reversed-9 Quotation Mark", '"'),
    ('″', "Double Prime", '"'),
    ('‶', "Reversed Double Prime", '"'),
    ('〃', "Ditto Mark", '"'),
    ('״', "Hebrew Punctuation Gershayim", '"'),
    ('˝', "Double Acute Accent", '"'),
    ('ʺ', "Modifier Letter Double Prime", '"'),
    ('˶', "Modifier Letter Middle Double Acute Accent", '"'),
    ('˵', "Modifier Letter Middle Double Grave Accent", '"'),
    ('ˮ', "Modifier Letter Double Apostrophe", '"'),
    ('ײ', "Hebrew Ligature Yiddish Double Yod", '"'),
    ('❞', "Heavy Double Comma Quotation Mark Ornament", '"'),
    ('❝', "Heavy Double Turned Comma Quotation Mark Ornament", '"'),
    ('❨', "Medium Left Parenthesis Ornament", '('),
    ('﴾', "Ornate Left Parenthesis", '('),
    ('（', "Fullwidth Left Parenthesis", '('),
    ('❩', "Medium Right Parenthesis Ornament", ')'),
    ('﴿', "Ornate Right Parenthesis", ')'),
    ('）', "Fullwidth Right Parenthesis", ')'),
    ('［', "Fullwidth Left Square Bracket", '['),
    ('❲', "Light Left Tortoise Shell Bracket Ornament", '['),
    ('「', "Left Corner Bracket", '['),
    ('『', "Left White Corner Bracket", '['),
    ('【', "Left Black Lenticular Bracket", '['),
    ('〔', "Left Tortoise Shell Bracket", '['),
    ('〖', "Left White Lenticular Bracket", '['),
    ('〘', "Left White Tortoise Shell Bracket", '['),
    ('〚', "Left White Square Bracket", '['),
    ('］', "Fullwidth Right Square Bracket", ']'),
    ('❳', "Light Right Tortoise Shell Bracket Ornament", ']'),
    ('」', "Right Corner Bracket", ']'),
    ('』', "Right White Corner Bracket", ']'),
    ('】', "Right Black Lenticular Bracket", ']'),
    ('〕', "Right Tortoise Shell Bracket", ']'),
    ('〗', "Right White Lenticular Bracket", ']'),
    ('〙', "Right White Tortoise Shell Bracket", ']'),
    ('〛', "Right White Square Bracket", ']'),
    ('❴', "Medium Left Curly Bracket Ornament", '{'),
    ('❵', "Medium Right Curly Bracket Ornament", '}'),
    ('⁎', "Low Asterisk", '*'),
    ('٭', "Arabic Five Pointed Star", '*'),
    ('∗', "Asterisk Operator", '*'),
    ('᜵', "Philippine Single Punctuation", '/'),
    ('⁁', "Caret Insertion Point", '/'),
    ('∕', "Division Slash", '/'),
    ('⁄', "Fraction Slash", '/'),
    ('╱', "Box Drawings Light Diagonal Upper Right To Lower Left", '/'),
    ('⟋', "Mathematical Rising Diagonal", '/'),
    ('⧸', "Big Solidus", '/'),
    ('㇓', "Cjk Stroke Sp", '/'),
    ('〳', "Vertical Kana Repeat Mark Upper Half", '/'),
    ('丿', "Cjk Unified Ideograph-4E3F", '/'),
    ('⼃', "Kangxi Radical Slash", '/'),
    ('＼', "Fullwidth Reverse Solidus", '\\'),
    ('﹨', "Small Reverse Solidus", '\\'),
    ('∖', "Set Minus", '\\'),
    ('⟍', "Mathematical Falling Diagonal", '\\'),
    ('⧵', "Reverse Solidus Operator", '\\'),
    ('⧹', "Big Reverse Solidus", '\\'),
    ('、', "Ideographic Comma", '\\'),
    ('ヽ', "Katakana Iteration Mark", '\\'),
    ('㇔', "Cjk Stroke D", '\\'),
    ('丶', "Cjk Unified Ideograph-4E36", '\\'),
    ('⼂', "Kangxi Radical Dot", '\\'),
    ('ꝸ', "Latin Small Letter Um", '&'),
    ('﬩', "Hebrew Letter Alternative Plus Sign", '+'),
    ('‹', "Single Left-Pointing Angle Quotation Mark", '<'),
    ('❮', "Heavy Left-Pointing Angle Quotation Mark Ornament", '<'),
    ('˂', "Modifier Letter Left Arrowhead", '<'),
    ('〈', "Left Angle Bracket", '<'),
    ('《', "Left Double Angle Bracket", '<'),
    ('꓿', "Lisu Punctuation Full Stop", '='),
    ('›', "Single Right-Pointing Angle Quotation Mark", '>'),
    ('❯', "Heavy Right-Pointing Angle Quotation Mark Ornament", '>'),
    ('˃', "Modifier Letter Right Arrowhead", '>'),
    ('〉', "Right Angle Bracket", '>'),
    ('》', "Right Double Angle Bracket", '>'),
    ('Ⲻ', "Coptic Capital Letter Dialect-P Ni", '-'),
    ('Ɂ', "Latin Capital Letter Glottal Stop", '?'),
    ('Ⳇ', "Coptic Capital Letter Old Coptic Esh", '/'), ];

const ASCII_ARRAY: &'static [(char, &'static str)] = &[
    (' ', "Space"),
    ('_', "Underscore"),
    ('-', "Minus/Hyphen"),
    (',', "Comma"),
    (';', "Semicolon"),
    (':', "Colon"),
    ('!', "Exclamation Mark"),
    ('?', "Question Mark"),
    ('.', "Period"),
    ('\'', "Single Quote"),
    ('"', "Quotation Mark"),
    ('(', "Left Parenthesis"),
    (')', "Right Parenthesis"),
    ('[', "Left Square Bracket"),
    (']', "Right Square Bracket"),
    ('{', "Left Curly Brace"),
    ('}', "Right Curly Brace"),
    ('*', "Asterisk"),
    ('/', "Slash"),
    ('\\', "Backslash"),
    ('&', "Ampersand"),
    ('+', "Plus Sign"),
    ('<', "Less-Than Sign"),
    ('=', "Equals Sign"),
    ('>', "Greater-Than Sign"), ];

pub fn check_unicode_char(maybe_unicode: char) -> Option<(char, &'static str, char, &'static str)> {
    
    let (unicode_ch, unicode_name, ascii_char) = match UNICODE_ARRAY.iter().find(|&&(c, _, _)| c == maybe_unicode) {
        Some(&(ch, name, asc)) => (ch, name, asc),
        None => return None,
    };

    let (ascii_char, ascii_name) = match ASCII_ARRAY.iter().find(|&&(c, _)| c == ascii_char) {
        Some(&(asc_ch, asc_name)) => (asc_ch, asc_name),
        None => return None,
    };

    Some((unicode_ch, unicode_name, ascii_char, ascii_name))
}

#[cfg(test)]
#[test]
fn uni_sep() {

    assert_eq!(check_unicode_char('。'), Some(('。', "Ideographic Full Stop", '.', "Period")));
    assert_eq!(check_unicode_char('.'), None);
    assert_eq!(check_unicode_char('⧹'), Some(('⧹', "Big Reverse Solidus", '\\', "Backslash")));
    assert_eq!(check_unicode_char('\\'), None);
    assert_eq!(check_unicode_char('；'), Some(('；', "Fullwidth Semicolon", ';', "Semicolon")));
    assert_eq!(check_unicode_char(';'), None);
}

// this means unicode char hint should be exactly after v1 before identifier parser
#[cfg(test)]
#[test]
#[should_panic]
fn uni_sep_is_not_alphabetic() {

    let mut fails = Vec::new();
    for uni_sep in UNICODE_ARRAY {
        if uni_sep.0.is_alphabetic() != uni_sep.2.is_alphabetic() {
            fails.push(uni_sep);
        }
    }

    if fails.len() != 0 {
        panic!("{:?}", fails);
    }
}