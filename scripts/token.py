#!python3

# separator and keyword definition generateor

from functools import reduce
import glob
import random
import sys

THIS_CODE_BASE = 'src/**/*.rs'
TEST_OUTPUT_FILE = 'src/lexical/token/tests.rs'
SEPARATOR_DEF_FILE = 'scripts/token-separator.txt'
SEPARATOR_OUTPUT_FILE = 'src/lexical/token/separator.rs'
KEYWORD_DEF_FILE = 'scripts/token-keyword.txt'
KEYWORD_OUTPUT_FILE = 'src/lexical/token/keyword.rs'

# format array of string
def format_wrap(indent, array):
    b = '    ' * indent
    current_length = indent * 4
    for item in array:
        b += f'{item}, '
        current_length += len(item) + 2
        if current_length > 80:
            b = b[:-1]
            b += '\n'
            b += '    ' * indent
            current_length = indent * 4
    b = b.rstrip()
    return b

def generate_header():
    b = ''
    b += '///! --------------------------------------------------------------------------------\n'
    b += '///! This code is auto generated by a tool\n'
    b += '///! Changes may cause incorrect behavior and will be lost if the code is regenerated\n'
    b += '///! --------------------------------------------------------------------------------\n'
    b += '\n'
    return b

# read this project (these rust source files) source code and merge them into one string
def get_this_code(codebase):
    result = ''
    for filename in glob.iglob(codebase, recursive=True):
        with open(filename) as file:
            result += file.read()
    return result

# try put already hashed values into buckets, measure memory usage
class HashChecker(object):
    def __init__(self, items):
        self.items = items
        self.bucket_size, self.bucket_count = next(iter(sorted(map(self.get_bucket_size_and_number, range(len(items), len(items) + 256)), key = lambda x: x[0] * x[1])))
        self.score = self.bucket_count * self.bucket_size / len(self.items)
    def get_bucket_size_and_number(self, moder):
        hash_values = [hashed_value % moder for hashed_value in self.items]
        max_collision_count = max(len(list(filter(lambda x: x == y, hash_values))) for y in set(hash_values))
        return moder, max_collision_count
    def __str__(self):
        return f'bucket {self.bucket_count} x {self.bucket_size} for {len(self.items)} items, score {self.score:.04}'

class Separator(object):
    def __init__(self, value, name, index, cats):
        self.value, self.name, self.index, self.cats = value, name, index, cats
        self.cats_value = 0
    def __str__(self):
        return f'separator({self.value}, {self.name}, {self.index}, ' + ' | '.join(self.cats) + f': {self.cats_value})'
    def __repr__(self):
        return str(self)
    def __lt__(self, rhs):
        return self.value < rhs.value
    def update_index(self, new_index):
        self.index = new_index
        return self
    def update_cats(self, cats):
        for (cat, value) in cats:
            if cat in self.cats:
                self.cats_value += value
        return self

class Separators(object):
    def __init__(self, filename):
        self.comments = []
        self.len1s, self.len2s, self.len3s = [], [], []
        for line in map(str.strip, open(filename).readlines()):
            if line.startswith('//'):
                self.comments.append(line)
            else:
                value, rest = map(lambda x: x.strip(" '"), line.split(' => '))
                name, categories, _ = rest.split(',')  # ignore last comma
                [self.len1s, self.len2s, self.len3s][len(value) - 1].append(Separator(    # auto panic len not in [1, 2, 3]
                    value, name, 0, list(map(str.strip, categories.split('|')))
                ))
        self.len1s.sort()
        self.len2s.sort()
        self.len3s.sort()
        self.all_items = self.len1s + self.len2s + self.len3s
        self.cats = [(cat, 2 ** index) for (index, cat) in
            enumerate(sorted(list(set(cat for sep in self.all_items for cat in sep.cats))))]
        assert len(self.cats) < 16
        self.len1s = [sep.update_index(index + 1).update_cats(self.cats) for (index, sep) in enumerate(self.len1s)]
        self.len2s = [sep.update_index(index + len(self.len1s) + 1).update_cats(self.cats) for (index, sep) in enumerate(self.len2s)]
        self.len3s = [sep.update_index(index + len(self.len1s) + len(self.len2s) + 1).update_cats(self.cats) for (index, sep) in enumerate(self.len3s)]

    def __str__(self):
        return f'separators ({len(self.len1s)}+{len(self.len2s)}+{len(self.len3s)}):'  \
            + '\n  categories:\n    ' + '\n    '.join(map(str, self.cats)) \
            + '\n  len1s:\n    ' + '\n    '.join(map(str, self.len1s)) \
            + '\n  len2s:\n    ' + '\n    '.join(map(str, self.len2s)) \
            + '\n  len3s:\n    ' + '\n    '.join(map(str, self.len3s))

    def format(self):
        # format and write back
        b = '\n'.join(self.comments)
        max_name_length = 0
        for sep in self.all_items:
            max_name_length = max(max_name_length, len(sep.name))
        for sep in self.all_items:
            b += "\n{space1}'{value}' => {space2}{name}, {cats},".format(
                space1 = ' ' * (4 - len(sep.value)), value = sep.value,
                space2 = ' ' * (max_name_length - len(sep.name)), name = sep.name,
                cats = ' | '.join(sep.cats)
            )
        return b

    def check(self):
        # len1 separator, which is ascii's printable non-letter part, 
        # which is kind of arranged by bit value (e.g. you can use 1 bit to check upper/lower letter)
        # which is not very suitable to directly put it to mod
        # hashf = lambda x: ord(x)
        # they range from 0x21-0x2F, 0x3A-0x40, 0x5B-0x60, 0x7B-0x7E, squash them together by moving in outer ones
        hashf1 = lambda x: ord(x) + 32 if ord(x) < 48 else ord(x) - 43 if ord(x) > 120 else ord(x)
        result = HashChecker([hashf1(sep.value) for sep in self.len1s])
        print(f'separator len1: ' + str(result))
        print(result.items)
        print([hashf1(sep.value) % result.bucket_size for sep in self.len1s])

        # many len2 ends with eq, use hashf1 for them => 1 * 22 + 1 * 13
        # this saves 3 bytes compare to 2017 version hashf = ord(x[0]) + ord(x[1]) * MAX_CODEPOINT: 2 * 19
        hashf2 = hashf1
        result = HashChecker([hashf2(sep.value[0]) for sep in self.len2s if sep.value[1] == '='])
        print(f'separator len2 (some=): ' + str(result))
        print(result.items)
        print([hashf2(sep.value[0]) % result.bucket_size for sep in self.len2s if sep.value[1] == '='])
        hashf2 = lambda x: hashf1(x[0]) + hashf1(x[1])
        result = HashChecker([hashf2(sep.value) for sep in self.len2s if sep.value[1] != '='])
        print(f'separator len2 (other): ' + str(result))
        print(result.items)
        print([hashf2(sep.value) % result.bucket_size for sep in self.len2s if sep.value[1] != '='])

    def generate_code(self):
        b = ''

        b += '#[repr(u16)]\n'
        b += '#[allow(dead_code)]\n'
        b += '#[derive(Eq, PartialEq, Copy, Clone, Debug)]\n'
        b += 'pub enum SeparatorKind {\n'
        b += '\n'.join(f'    {cat_name} = 0x{cat_value:x},' for (cat_name, cat_value) in self.cats)
        b += '\n}\n\n'

        b += '#[repr(u8)]\n'
        b += '#[derive(Eq, PartialEq, Copy, Clone, Debug)]\n'
        b += 'pub enum Separator {\n'
        for item in self.all_items:
            b += f'    /// `{item.value}`\n    {item.name} = {item.index},\n'
        b += '}\n'
        b += '\n'
        b += 'const VALUES: &[&str] = &[\n'
        b += format_wrap(1, ['""'] + [f'"{x.value}"' for x in self.all_items])
        b += '\n'
        b += '];\n'
        b += 'const KINDS: &[u16] = &[\n'
        b += format_wrap(1, ['0'] + [str(x.cats_value) for x in self.all_items])
        b += '\n'
        b += '];\n'
        b += '\n'
        b += 'impl Separator {\n'
        b += '\n'
        b += '    pub fn display(self) -> &\'static str {\n'
        b += '        VALUES[self as u8 as usize]\n'
        b += '    }\n'
        b += '\n'
        b += '    pub fn kind(self, kind: SeparatorKind) -> bool {\n'
        b += '        let kind = kind as u16;\n'
        b += '        KINDS[self as u8 as usize] & kind == kind\n'
        b += '    }\n'
        b += '}\n'

        # see self.check for hash strategy
        hashf1 = lambda x: ord(x) + 32 if ord(x) < 48 else ord(x) - 43 if ord(x) > 120 else ord(x)
        hashf2 = lambda x: hashf1(x[0]) + hashf1(x[1])
        len1_config = HashChecker([hashf1(sep.value) for sep in self.len1s])
        len2_config1 = HashChecker([hashf1(sep.value[0]) for sep in self.len2s if sep.value[1] == '='])
        len2_config2 = HashChecker([hashf2(sep.value) for sep in self.len2s if sep.value[1] != '='])
        print('separaotr len1: ' + str(len1_config))
        print('separator len2 (some=): ' + str(len2_config1))
        print('separator len2 (other): ' + str(len2_config2))
        if len1_config.bucket_count != 1 or len2_config1.bucket_count != 1 or len2_config2.bucket_count != 1:
            raise ValueError("current generate process relies on bucket count is all 1 for separator")
        
        # no key: you originally need to store hash inside key value pair
        # to prevent collision (different key in pair means collide), but that's not needed for bucket count 1
        # use 0 to indicate no match
        bucket1 = [0 for _ in range(0, len1_config.bucket_size)]
        bucket2 = [0 for _ in range(0, len2_config1.bucket_size)]
        bucket3 = [0 for _ in range(0, len2_config2.bucket_size)]
        for sep in self.len1s:
            hash_value = hashf1(sep.value) % len1_config.bucket_size
            bucket1[hash_value] = sep.name
        for sep in [sep for sep in self.len2s if sep.value[1] == '=']:
            hash_value = hashf1(sep.value[0]) % len2_config1.bucket_size
            bucket2[hash_value] = sep.name
        for sep in [sep for sep in self.len2s if sep.value[1] != '=']:
            hash_value = hashf2(sep.value) % len2_config2.bucket_size
            bucket3[hash_value] = sep.name

        b += '\n'
        b += 'use Separator::*;\n'
        b += 'const BUCKET1: &[Option<Separator>] = &[\n'
        b += format_wrap(1, [f'Some({entry})' if entry != 0 else 'None' for entry in bucket1])
        b += '\n'
        b += '];\n'
        b += 'const BUCKET2: &[Option<Separator>] = &[\n'
        b += format_wrap(1, [f'Some({entry})' if entry != 0 else 'None' for entry in bucket2])
        b += '\n'
        b += '];\n'
        b += 'const BUCKET3: &[Option<Separator>] = &[\n'
        b += format_wrap(1, [f'Some({entry})' if entry != 0 else 'None' for entry in bucket3])
        b += '\n'
        b += '];\n'
        b += '\n'
        b += '#[inline]\n'
        b += 'fn hash(c: char) -> u32 {\n'
        b += '    let ord = c as u32;\n'
        b += '    if ord < 48 { ord + 32 } else if ord > 120 { ord - 43 } else { ord }\n'
        b += '}\n'
        b += '\n'
        b += 'impl Separator {\n'
        b += '    pub fn parse(buf: [char; 3]) -> Option<(Separator, usize)> {\n'
        b += '        match buf {\n'
        for sep in self.len3s:
            b += f'            [\'{sep.value[0]}\', \'{sep.value[1]}\', \'{sep.value[2]}\'] => Some(({sep.name}, 3)),\n'
        b += '            [c1, \'=\', _] => {\n'
        b += f'                BUCKET2[(hash(c1) % {len2_config1.bucket_size}) as usize]\n'
        b += '                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 2)) } else { None })\n'
        b += f'                    .or_else(|| BUCKET1[(hash(c1) % {len1_config.bucket_size}) as usize]\n'
        b += '                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 1)) } else { None }))\n'
        b += '            },\n'
        b += '            [c1, c2, _] => {\n'
        b += f'                BUCKET3[((hash(c1) + hash(c2)) % {len2_config2.bucket_size}) as usize]\n'
        b += '                    .and_then(|s| if s.display().as_bytes()[0..2] == [c1 as u8, c2 as u8] { Some((s, 2)) } else { None })\n'
        b += f'                    .or_else(|| BUCKET1[(hash(c1) % {len1_config.bucket_size}) as usize]\n'
        b += '                    .and_then(|s| if s.display().as_bytes()[0] == c1 as u8 { Some((s, 1)) } else { None }))\n'
        b += '            },\n'
        b += '        }\n'
        b += '    }\n'
        b += '}\n'

        return b

    def generate_tests(self, seed):
        b = ''

        # several random tests to confirm no simple error like off by 1
        # no cover all because impl is auto generated and will not have error for some but not for other 
        b += '#[test]\n'
        b += 'fn separator_basic() {\n\n'
        for _ in range(5):
            sep = random.choice(self.all_items)
            b += f'    assert_eq!(Separator::{sep.name}.display(), "{sep.value}");\n'
        for _ in range(5):
            sep = random.choice(self.all_items)
            true_cat = random.choice(sep.cats)
            maybe_false_cat = random.choice(self.cats)[0]
            b += f'    assert!(Separator::{sep.name}.kind(SeparatorKind::{true_cat}));\n'
            if maybe_false_cat in sep.cats:
                b += f'    assert!(Separator::{sep.name}.kind(SeparatorKind::{maybe_false_cat}));\n'
            else:
                b += f'    assert!(!Separator::{sep.name}.kind(SeparatorKind::{maybe_false_cat}));\n'
        b += '}\n'

        b += '\n'
        b += '#[test]\n'
        b += 'fn separator_parse() {\n\n'

        b += "    assert_eq!(Separator::parse(['<', '<', '=']), Some((Separator::LtLtEq, 3)));\n"
        b += "    assert_eq!(Separator::parse(['>', '>', '=']), Some((Separator::GtGtEq, 3)));\n"
        b += "    assert_eq!(Separator::parse(['+', ' ', '1']), Some((Separator::Add, 1)));\n"
        b += "    assert_eq!(Separator::parse(['!', '[', '(']), Some((Separator::Not, 1)));\n"
        # !!!! 17/7/8, this case is very very interesting
        # I created this case simply randomly manully, but it help me find the bug of current hash function and hash map design
        # hash = ch1 as u32 + ch2 as u32 * 256, and hashmap's bucket size is 38, and, most interestingly
        # ('{' as u32  + ' ' as u32 * 256) % 38 == ('!' as u32 + '=' as u32 * 256) % 38 == 31
        # which help me understand why the common practice is implement hashmap first and implement hashset as hashmap<T, ()>,
        # because in this case if you do not have a key stored in hashmap to confirm equality then a bug happened
        # update 2022/3/6: that's the old hash function, if you are concerned
        b += "    assert_eq!(Separator::parse(['{', ' ', 'a']), Some((Separator::LBrace, 1)));\n"
        b += "    assert_eq!(Separator::parse(['&', '&', ' ']), Some((Separator::AndAnd, 2)));\n"
        # 17/7/11 this case is manually created to prove that the original hash function has bug
        # original hash function is `lambda value: ord(value[0]) + ord(value[1]) * 256`
        # or `|ch1, ch2| ch1 as u32 + ch2 as u32 * 256` where high probability of hash coliision exists
        # update 2022/3/6: that's the old hash function, if you are concerned
        b += "    assert_eq!(Separator::parse(['Х', '9', ' ']), None);\n"

        def quote(c):
            return f"\{c}" if c == "'" or c == '\\' else c

        # this is added because results says the following method hit rate is kind of low
        for _ in range(0, 20):
            char1 = random.choice(self.len1s).value
            char2 = '=' if random.randint(0, 100) < 40 else random.choice(self.len1s).value
            char3 = random.choice(self.len1s).value
            expect = next((sep for sep in self.len3s if sep.value == f'{char1}{char2}{char3}'), \
                next((sep for sep in self.len2s if sep.value == f'{char1}{char2}'), \
                next((sep for sep in self.len1s if sep.value == char1), None)))
            if expect is None:
                b += f'    assert_eq!(Separator::parse([\'{quote(char1)}\', \'{quote(char2)}\', \'{quote(char3)}\']), None);\n'
            else:
                b += f'    assert_eq!(Separator::parse([\'{quote(char1)}\', \'{quote(char2)}\', \'{quote(char3)}\']), Some((Separator::{expect.name}, {len(expect.value)})));\n'

        # result says this hit rate is kind of low
        for _ in range(0, 100):
            index = random.randint(0, len(seed) - 3)
            chars = seed[index:index+3]
            if chars[0].isspace() or chars[1].isspace() or chars[2].isspace():
                continue
            if chars[0].isalpha() and chars[1].isalpha() and chars[2].isalpha():
                continue
            # no, there is not any <<= or >>= in current source code, but put it here
            expect = next((sep for sep in self.len3s if sep.value == chars), \
                next((sep for sep in self.len2s if sep.value == chars[:2]), \
                next((sep for sep in self.len1s if sep.value == chars[0]), None)))
            if expect is None:
                b += f'    assert_eq!(Separator::parse([\'{quote(chars[0])}\', \'{quote(chars[1])}\', \'{quote(chars[2])}\']), None);\n'
            else:
                b += f'    assert_eq!(Separator::parse([\'{quote(chars[0])}\', \'{quote(chars[1])}\', \'{quote(chars[2])}\']), Some((Separator::{expect.name}, {len(expect.value)})));\n'

        b += '}\n'

        return b

class Keyword(object):
    def __init__(self, value, name, index, cat):
        self.value, self.name, self.index, self.cat = value, name, index, cat
    def __str__(self):
        return f'keyword({self.value}, {self.name}, {self.index}, {self.cat}: {self.cat_value})'
    def __lt__(self, rhs):
        # lower: group self and Self together
        return self.value.lower() < rhs.value.lower()
    def update_index(self, new_index):
        self.index = new_index
        return self
    def update_cat_value(self, cats):
        self.cat_value = cats[self.cat]
        return self

class Keywords(object):
    def __init__(self, filename):
        self.comments = []
        self.items = []
        for line in map(str.strip, open(filename).readlines()):
            if line.startswith('//'):
                self.comments.append(line)
            else:
                value, rest = map(lambda x: x.strip(" '"), line.split('=>'))
                name, category, _ = map(str.strip, rest.split(','))
                self.items.append(Keyword(value, name, 0, category))
        assert len(self.items) < 255  # 255 is used in bucket to represent empty value
        # ATTENTION: grammar.py validation function also relies on these values
        self.cats = { 'Reserved': 0, 'Primitive': 1, 'MaybeIdentifier': 2, 'Normal': 3 }
        self.items.sort()
        self.items = [keyword.update_index(index + 1).update_cat_value(self.cats) for (index, keyword) in enumerate(self.items)]

    def __str__(self):
        return f'keywords ({len(self.items)}):' \
            + '\n  items:\n    ' + '\n    '.join(map(str, self.items))

    # format and write back
    def format(self):
        b = '\n'.join(self.comments)
        max_value_length, max_name_length = 0, 0
        for kw in self.items:
            max_value_length = max(max_value_length, len(kw.value))
            max_name_length = max(max_name_length, len(kw.name))
        for kw in self.items:
            b += "\n {space1}'{value}' => {space2}{name}, {cat},".format(
                space1 = ' ' * (max_value_length - len(kw.value)), value = kw.value,
                space2 = ' ' * (max_name_length - len(kw.name)), name = kw.name,
                cat = kw.cat
            )
        return b

    def check(self):
        # v1:
        # 16557366432705, 43 => 137, 2
        # 16557366432696, 43 => 137, 2
        # 16557366432689, 43 => 137, 2
        # hashf = lambda value: reduce(lambda x, y: x * (ord(y) - 43) % 16557366432705, value, 1)
        # v2:
        # self.items[1:], 95, 16557366432705 => 89 x 3
        # hashf = lambda value: reduce(lambda x, y: x * (ord(y) - 95) % 16557366432705, value, 1)
        # v3:
        # map lower letter to start from 1, map number to start from 1, % 173, 181, 173, 353, 389 => 84 x 3 = 252 
        #   and search through 101 - 10001 does not have better result
        # hashf = lambda value: reduce(lambda acc, x: acc * (ord(x) - 96 if ord(x) > 96 else ord(x) - 48) % 173, value, 1)
        # v4:
        # split into <=5 and > 5 => 3 x 64 + 2 x 33 = 258
        # split into (1, 2, 3), (4, 5, 6), (7, 8, 9) => 3 x 31 + 3 x 44 + 2 x 12 => 249
        # split into (1, 2, 3), (4, 5), (6, 7, 8, 9) => 3 x 31 + 2 x 45 + 2 x 33 => 249
        # split into (1, 2), (3, 4, 5), (6, 7, 8, 9) => 1 * 17 + 3 * 53 + 2 * 33 => 242
        # results = []
        # for group in [(1, 2), (3, 4, 5), (6, 7, 8, 9)]:
        #     hashf = lambda value: reduce(lambda acc, x: acc * (ord(x) - 96 if ord(x) > 96 else ord(x) - 48) % 173, value, 1)
        #     result = HashChecker([hashf(kw.value) for kw in self.items if len(kw.value) in group])
        #     results.append(result)
        #     print(f'keyword ({group}): ' + str(result))
        # print(f'keyword overall score: {sum(r.bucket_count * r.bucket_size for r in results)}')
        # v5:
        # split into length buckets: => 191
        results = []
        for length in range(1, 10):
            hashf = lambda value: reduce(lambda acc, x: acc * (ord(x) - 96 if ord(x) > 96 else ord(x) - 48) % 173, value, 1)
            result = HashChecker([hashf(kw.value) for kw in self.items if len(kw.value) == length])
            results.append(result)
            print(f'keyword ({length}): ' + str(result))
        print(f'keyword overall score: {sum(r.bucket_count * r.bucket_size for r in results)}')

    def generate_code(self):
        b = ''

        b += '#[repr(u8)]\n'
        b += '#[allow(dead_code)]\n' # normal is actually never used
        b += '#[derive(Eq, PartialEq, Clone, Copy, Debug)]\n'
        b += 'pub enum KeywordKind {\n'
        for cat_name, cat_value in self.cats.items():
            b += f'    {cat_name} = {cat_value},\n'
        b += '}\n'
        
        b += '\n'
        b += '#[repr(u8)]\n'
        b += '#[derive(Eq, PartialEq, Clone, Copy, Debug)]\n'
        b += 'pub enum Keyword {\n'
        for kw in self.items:
            b += f'    {kw.name} = {kw.index},\n'
        b += '}\n'

        b += '\n'
        b += 'const VALUES: &[&str] = &[\n'    # thanks for 1.18's const static default 'static # this historical data confirms that it's written at 1.18 time
        b += format_wrap(1, ['""'] + [f'"{kw.value}"' for kw in self.items])
        b += '\n'
        b += '];\n'
        b += 'const KINDS: &[u8] = &[\n'
        b += format_wrap(1, ['0'] + [str(kw.cat_value) for kw in self.items])
        b += '\n'
        b += '];\n'
        b += '\n'
        b += 'impl Keyword {\n'
        b += '\n'
        b += '    pub fn display(self) -> &\'static str {\n'
        b += '        VALUES[self as u8 as usize]\n'
        b += '    }\n'
        b += '\n'
        b += '    pub fn kind(self, kind: KeywordKind) -> bool {\n'
        b += '        KINDS[self as u8 as usize] == kind as u8\n'
        b += '    }\n'
        b += '}\n'

        bucket_groups = []
        ordered_items = \
            [kw for kw in self.items if kw.cat_value == self.cats['Normal']] \
            + [kw for kw in self.items if kw.cat_value == self.cats['MaybeIdentifier']] \
            + [kw for kw in self.items if kw.cat_value == self.cats['Primitive']] \
            + [kw for kw in self.items if kw.cat_value == self.cats['Reserved']]  # normal in priority, then maybeident, then primitive, last reserved

        hashf = lambda value: reduce(lambda acc, x: acc * (ord(x) - 96 if ord(x) > 96 else ord(x) - 48) % 173, value, 1)
        configs = [(length, HashChecker([hashf(kw.value) for kw in self.items if len(kw.value) == length])) for length in range(2, max(len(kw.value) for kw in self.items) + 1)]
        for (length, config) in configs:
            print(f"keyword (length {length}): {config}")
            bucket_group = [[0 for _ in range(0, config.bucket_size)] for _ in range(0, config.bucket_count)]
            for kw in [kw for kw in ordered_items if len(kw.value) == length]:
                hash_value = hashf(kw.value) % config.bucket_size
                for char in kw.value:
                    if ord(char) <= 48:
                        raise ValueError("keyword character should not less than or equal to '0'")
                for bucket in bucket_group:
                    if bucket[hash_value] == 0:
                        bucket[hash_value] = kw.name
                        break
                else:
                    print(bucket_group)
                    print(hash_value)
                    raise ValueError(f"unexpected more collision for {kw.name}")
            bucket_groups.append(bucket_group)

        b += '\n'
        b += 'use Keyword::*;\n'
        b += 'const BUCKETS: &[&[&[Option<Keyword>]]] = &[&[], &[],\n'
        for (bucket_group_index, bucket_group) in enumerate(bucket_groups):
            b += f'    /* {bucket_group_index + 2} */ &[\n'
            for bucket in bucket_group:
                display_items = [f"Some({i})" if i != 0 else "None" for i in bucket]
                if len(format_wrap(3, display_items)) < 90:
                    b += f'        &[{", ".join(display_items)}],\n'
                else:
                    b += '        &[\n'
                    b += format_wrap(3, display_items)
                    b += '\n'
                    b += '        ],\n'
            b+= '    ],\n'
        b += '];\n'
        b += '\n'
        b += 'impl Keyword {\n'
        b += '\n'
        b += '    pub fn parse(v: &str) -> Option<Keyword> {\n'
        b += '        debug_assert!(!v.is_empty(), "empty ident");\n'
        for kw in [kw for kw in self.items if len(kw.value) == 1]:
            b += f'        if v == "{kw.value}" {{ return Some({kw.name}); }}\n'
        b += f'        if v.len() < 2 || v.len() > {max(len(kw.value) for kw in self.items)} {{ return None; }}\n'
        b += '        let hash = v.as_bytes().iter().fold(1usize, |acc, &b| acc * if b > 96 { b as usize - 96 } else if b > 48 { b as usize - 48 } else { b as usize } % 173);\n'
        b += '        let buckets = &BUCKETS[v.len()];\n'
        b += '        let hash = hash as usize % buckets[0].len();\n'
        b += '        buckets.iter().filter_map(|bucket| bucket[hash].and_then(|i| if i.display() == v { Some(i) } else { None })).next()\n'
        b += '    }\n'
        b += '}\n'

        return b

    def generate_tests(self, seed):
        b = ''

        # several random tests to confirm no simple error like off by 1
        # no cover all because impl is auto generated and will not have error for some but not for other
        b += '\n'
        b += '#[test]\n'
        b += 'fn keyword_basic() {\n\n'
        for _ in range(5):
            kw = random.choice(self.items)
            b += f'    assert_eq!(Keyword::{kw.name}.display(), "{kw.value}");\n'
        for _ in range(5):
            kw = random.choice(self.items)
            maybe_false_cat = random.choice(list(self.cats))
            b += f'    assert!(Keyword::{kw.name}.kind(KeywordKind::{kw.cat}));\n'
            if maybe_false_cat == kw.cat:
                b += f'    assert!(Keyword::{kw.name}.kind(KeywordKind::{maybe_false_cat}));\n'
            else:
                b += f'    assert!(!Keyword::{kw.name}.kind(KeywordKind::{maybe_false_cat}));\n'
        b += '}\n'

        b += '\n'
        b += '#[test]\n'
        b += 'fn keyword_parse() {\n\n'
        b += '    assert_eq!(Keyword::parse("fn"), Some(Keyword::Fn));\n'
        b += '    assert_eq!(Keyword::parse("await"), Some(Keyword::Await));\n'
        b += '    assert_eq!(Keyword::parse("一个chinese变量"), None);\n'
        b += '    assert_eq!(Keyword::parse("a_中文_var"), None);\n'
        # this test case from v2_base help me find that if _invalid_key in BUCKET1 should still check BUCKET2
        b += '    assert_eq!(Keyword::parse("as"), Some(Keyword::As));\n'

        # no, this hit rate is lower
        for _ in range(0, 1000):
            length = random.randint(2, 9)
            index = random.randint(0, len(seed) - length)
            test_input = seed[index:index + length]
            if any(not i.isalpha() for i in test_input):
                continue
            expect = next((kw for kw in self.items if kw.value == test_input), None)
            if expect is None:
                if random.randint(0, 100) > 90: # 90% of not hit is discarded
                    b += f'    assert_eq!(Keyword::parse("{test_input}"), None);\n'
            else:
                b += f'    assert_eq!(Keyword::parse("{test_input}"), Some(Keyword::{expect.name}));\n'

        b += '}\n'

        return b

if __name__ == '__main__':
    separators = Separators(SEPARATOR_DEF_FILE)
    keywords = Keywords(KEYWORD_DEF_FILE)

    separator_def = separators.format()
    keyword_def = keywords.format()
    with open(SEPARATOR_DEF_FILE, 'w') as file:
        file.write(separator_def)
    with open(KEYWORD_DEF_FILE, 'w') as file:
        file.write(keyword_def)

    if len(sys.argv) == 1:
        print(separators)
        print(keywords)
        print("use $(token.py gen) to generate")

    elif sys.argv[1] == 'check':
        separators.check()
        keywords.check()

    else:
        this_code = get_this_code(THIS_CODE_BASE)
        separator_code = generate_header() + separators.generate_code()
        keyword_code = generate_header() + keywords.generate_code()
        test_code = generate_header() + 'use super::*;\n\n' + separators.generate_tests(this_code) + keywords.generate_tests(this_code)

        print(f'write {SEPARATOR_OUTPUT_FILE} {len(separator_code.splitlines())} lines')
        with open(SEPARATOR_OUTPUT_FILE, 'w') as file:
            file.write(separator_code)
        print(f'write {KEYWORD_OUTPUT_FILE} {len(keyword_code.splitlines())} lines')
        with open(KEYWORD_OUTPUT_FILE, 'w') as file:
            file.write(keyword_code)
        print(f'write {TEST_OUTPUT_FILE} {len(test_code.splitlines())} lines')
        with open(TEST_OUTPUT_FILE, 'w') as file:
            file.write(test_code)
