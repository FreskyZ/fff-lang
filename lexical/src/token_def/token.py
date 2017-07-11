# fff-lang
# seperator and keyword definition generateor

# based on 'token.grammar' file, generate the enum definition source code
# provide fast O(1) impl for their attribute getter, like `ATTR[(u8)keyword].Category` etc.
# provide fast O(1) parse method, with a special static readonly on stack hashmap generator

# this file replaces old `define_keyword` and `define_seperator` macros
# because vscode's racer or rls both do not recognize types defined in macros, currently at lease

from collections import namedtuple
from functools import reduce
from itertools import chain
import random
import codecs

SEPERATOR_DEF_FILE = 'seperator.grammar'
KEYWORD_DEF_FILE = 'keyword.grammar'
SEPERATOR_TARGET_FILE = 'seperator.rs'
KEYWORD_TARGET_FILE = 'keyword.rs'

UNICODE_MAX_CODEPOINT = 0x10FFFF

def check_hasher_perform(values, hasher):
    half_hash_values = list(map(hasher, values))
    def get_bucket_size_and_number(moder):
        hash_values = [half_hash_value % moder for half_hash_value in half_hash_values]
        max_collision_count = max(len(list(filter(lambda x: x == hash_value, hash_values))) for hash_value in set(hash_values))
        return moder, max_collision_count, moder * max_collision_count
    return next(iter(sorted(map(get_bucket_size_and_number, range(len(values), 512)), key = lambda x: x[2])), None)

class Seperator:
    def __init__(self, value, name, index, cats):
        self.value, self.name, self.index, self.cats = value, name, index, cats
        self.cats_value = 0
    def __str__(self):
        return f'seperator({self.value}, {self.name}, {self.index}, ' + ' | '.join(self.cats) + f': {self.cats_value})'
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
class Seperators:
    def __init__(self, filename):
        self.filename = filename
        self.comments = []
        self.len1s, self.len2s, self.len3s = [], [], []
        for line in map(str.strip, open(filename).readlines()):
            if line.startswith('//'):
                self.comments.append(line)
            else:
                value, rest = map(lambda x: x.strip(" '"), line.split(' => '))
                name, categories, _ = rest.split(',')  # ignore last comma
                [self.len1s, self.len2s, self.len3s][len(value) - 1].append(Seperator(    # auto panic len not in [1, 2, 3]
                    value, name, 0, list(map(str.strip, categories.split('|')))
                ))
        _ = list(map(list.sort, [self.len1s, self.len2s, self.len3s]))
        self.cats = [(cat, 2 ** index) for (index, cat) in 
            enumerate(sorted(list(set(cat for sep in chain(self.len1s, self.len2s, self.len3s) for cat in sep.cats))))]
        assert len(self.cats) < 16
        self.len1s = [sep.update_index(index).update_cats(self.cats) for (index, sep) in enumerate(self.len1s)]
        self.len2s = [sep.update_index(index + len(self.len1s)).update_cats(self.cats) for (index, sep) in enumerate(self.len2s)]
        self.len3s = [sep.update_index(index + len(self.len1s) + len(self.len2s)).update_cats(self.cats) for (index, sep) in enumerate(self.len3s)]

    def __str__(self):
        return 'seperator in ' + self.filename                         \
            + ':\n  comments:\n  ' + '\n  '.join(self.comments)        \
            + ':\n  categories:\n    ' + '\n    '.join(map(str, self.cats)) \
            + '\n  len1s:\n    ' + '\n    '.join(map(str, self.len1s)) \
            + '\n  len2s:\n    ' + '\n    '.join(map(str, self.len2s)) \
            + '\n  len3s:\n    ' + '\n    '.join(map(str, self.len3s))
    
    def format(self):
        # format and write back
        retval = '\n'.join(self.comments)
        max_name_length = 0
        for sep in chain(self.len1s, self.len2s, self.len3s):
            max_name_length = max(max_name_length, len(sep.name))
        for sep in chain(self.len1s, self.len2s, self.len3s):
            retval += "\n{space1}'{value}' => {space2}{name}, {cats},".format(
                space1 = ' ' * (4 - len(sep.value)), value = sep.value,
                space2 = ' ' * (max_name_length - len(sep.name)), name = sep.name,
                cats = ' | '.join(sep.cats)
            )
        with open(self.filename, 'w') as file:
            file.write(retval)

    def generate_hash_specs(self):
        bucket_size, bucket_number, memory_use = check_hasher_perform(
            list(map(lambda x: x.value, self.len1s)), 
            lambda x: ord(x)
        )
        print(f'len1: bucket size: {bucket_size}, number: {bucket_number}, memory use: {memory_use}') # 37, 1
        bucket_size, bucket_number, memory_use = check_hasher_perform(
            list(map(lambda x: x.value, self.len2s)), 
            lambda x: ord(x[0]) + ord(x[1]) * UNICODE_MAX_CODEPOINT
        )
        print(f'len2: bucket size: {bucket_size}, number: {bucket_number}, memory use: {memory_use}') # 22, 2
    
    def generate_tests(self):
        test_src = ''

        # test_src += '\n// to make sure the ABI not changed'
        # test_src += '#[cfg(test)] #[test]\n'
        # test_src += 'fn seperator_into_u8() {\n'
        # test_src += '    unsafe {\n'
        # for sep in chain(self.len1s, self.len2s, self.len3s):
        #     test_src += '        assert_eq!{ ::std::mem::transmute_copy::<Seperator, u8>(&Seperator::' + sep.name + ') as usize, ' + str(sep.index) + ' }\n'
        # test_src += '    }\n'
        # test_src += '}\n'

        items = list(chain(self.len1s, self.len2s, self.len3s))

        test_src += '#[cfg(test)] #[test]\n'
        test_src += 'fn seperator_debug() {\n\n'
        for _ in range(10):
            sep = random.choice(items)
            test_src += '    assert_eq!{ format!("{:?}", Seperator::' + sep.name + '), "' + sep.value + '" }\n'
        test_src += '}\n'

        test_src += '#[cfg(test)] #[test]\n'
        test_src += 'fn seperator_is_cat() {\n\n'
        for _ in range(10):
            sep = random.choice(items)
            true_cat = random.choice(sep.cats)
            maybe_false_cat = random.choice(self.cats)[0]
            test_src += '    assert_eq!{ Seperator::' + sep.name + '.is_category(SeperatorCategory::' + true_cat + '), true }\n'
            test_src += '    assert_eq!{ Seperator::' + sep.name + '.is_category(SeperatorCategory::' + maybe_false_cat + '), ' + \
                ('true' if maybe_false_cat in sep.cats else 'false') + ' }\n'
        test_src += '}\n'

        # TODO: generate proper parse test cases
        test_src += '#[cfg(test)] #[test]\n'
        test_src += 'fn seperator_parse() {\n\n'
        # for _ in range(10):
        #     sep = random.choice(items)
        #     test_src += "    assert_eq!{"

        test_src += "    assert_eq!{ Seperator::parse3('<', '<', '='), Some((Seperator::ShiftLeftAssign, 3)) }\n"
        test_src += "    assert_eq!{ Seperator::parse3('+', ' ', '1'), Some((Seperator::Add, 1)) }\n"
        # !!!! 17/7/8, this case is very very interesting
        # I created this case simply randomly manully, but it help me find the bug of current hash function and hash map design
        # hash = ch1 as u32 + ch2 as u32 * 256, and hashmap's bucket size is 38, and, most interestingly
        # ('{' as u32  + ' ' as u32 * 256) % 38 == ('!' as u32 + '=' as u32 * 256) % 38 == 31
        # which help me understand why there is first hashmap then hashset just a hashmap<T, ()>
        # because in this case if you do not have a key stored in hashmap to confirm equality then a bug happened
        test_src += "    assert_eq!{ Seperator::parse3('{', ' ', 'a'), Some((Seperator::LeftBrace, 1)) }\n"
        test_src += "    assert_eq!{ Seperator::parse3('&', '&', ' '), Some((Seperator::LogicalAnd, 2)) }\n"
        # 17/7/11 this case is manually created to prove that the original hash function has bug
        # original hash function is `lambda value: ord(value[0]) + ord(value[1]) * 256`
        # or `|ch1, ch2| ch1 as u32 + ch2 as u32 * 256` where high probability of hash coliision exists
        test_src += "    assert_eq!{ Seperator::parse3('Х', '9', ' '), None }\n"
        test_src += '}\n'

        return test_src

    def generate(self):
        def extend_list(l, item):
            l.extend(item)
            return l
        src = ''
        src += '///! fff-lang\n'
        src += '///!\n'
        src += '///! lexical/seperator\n'
        src += '///! Attention: contens are auto generated by token.py, do not modify this file\n\n'

        src += '#[allow(non_snake_case)]\n'
        src += '#[allow(non_upper_case_globals)]\n'
        src += 'pub mod SeperatorCategory {\n    '
        max_categories_len = len(max(self.cats, key = lambda cat: len(cat[0]))[0])
        src += '\n    '.join('pub const {space}{name}: u16 = {value};'.format(
                space = ' ' * (max_categories_len - len(cat_name)), name = cat_name,
                value = '0x%04x' % cat_value
        ) for (cat_name, cat_value) in self.cats)
        src += '\n}\n\n'

        src += '#[derive(Eq, PartialEq, Copy, Clone)]\n'
        src += 'pub enum Seperator {\n    '
        src += '\n    '.join('{},'.format(name) for name in map(lambda x: x.name, chain(self.len1s, self.len2s, self.len3s)))
        src += '\n}\n\n'

        # because hashf = lambda value: ord(value) % 37, so 37 is an invalid key
        LEN1_EMPTY_ENTRY = (37, 0)  
        # because hashf = lambda value: ord(value[0]) + ord(value[1]) * MAX, so MAX + MAX * MAX + 1 is invalid key
        LEN2_EMPTY_ENTRY = (UNICODE_MAX_CODEPOINT + UNICODE_MAX_CODEPOINT * UNICODE_MAX_CODEPOINT + 1, 0) # 
        len1_bucket = [LEN1_EMPTY_ENTRY for _ in range(37)] 
        len2_bucket1, len2_bucket2 = [LEN2_EMPTY_ENTRY for _ in range(22)], [LEN2_EMPTY_ENTRY for _ in range(22)]
        for sep in self.len1s:
            hashv = ord(sep.value)
            len1_bucket[hashv % 37] = (hashv, sep.index)
        for sep in self.len2s:
            hashv = ord(sep.value[0]) + ord(sep.value[1]) * UNICODE_MAX_CODEPOINT
            if len2_bucket1[hashv % 22] == LEN2_EMPTY_ENTRY:
                len2_bucket1[hashv % 22] = (hashv, sep.index - len(self.len1s))
            elif len2_bucket2[hashv % 22] == LEN2_EMPTY_ENTRY:
                len2_bucket2[hashv % 22] = (hashv, sep.index - len(self.len1s))
            else:
                assert not 'should not use more than 2 buckets'

        def format_array(array): # already stringified array
            array_src = ''
            max_length = len(max(array, key = len))
            item_per_line = int(76 / (max_length + 2)) # indent 4 char, not overwrite 80 char, each item follow with a comma and a space
            for index, item in enumerate(array):
                if index % item_per_line == 0:
                    array_src += '\n    '
                array_src += '{space}{item}, '.format(item = item, space = '') # used to make more beautiful format, but actually more ugly so abandoned
            return array_src

        src += 'const CHAR_MAX_CODEPOINT: u64 = 0x10FFFF;\n'
        src += 'const LEN1_EMPTY: (u32, u8) = (37, 0);\n'
        src += 'const LEN2_EMPTY: (u64, u8) = (' + str(LEN2_EMPTY_ENTRY[0]) + ', 0);\n'
        src += 'const LEN1_BUCKET: &[(u32, u8)] = &['
        len1_bucket_str = [str(entry) if entry != LEN1_EMPTY_ENTRY else 'LEN1_EMPTY' for entry in len1_bucket]
        src += format_array(len1_bucket_str)
        src += '\n];\n'
        src += 'const LEN2_BUCKET1: &[(u64, u8)] = &['
        len2_bucket1_str = [str(entry) if entry != LEN2_EMPTY_ENTRY else 'LEN2_EMPTY' for entry in len2_bucket1]
        src += format_array(len2_bucket1_str)
        src += '\n];\n'
        src += 'const LEN2_BUCKET2: &[(u64, u8)] = &['
        len2_bucket2_str = [str(entry) if entry != LEN2_EMPTY_ENTRY else 'LEN2_EMPTY' for entry in len2_bucket2]
        src += format_array(len2_bucket2_str)
        src += '\n];\n'
        src += 'impl Seperator {\n\n'
        src += '    pub fn parse1(ch: char) -> Option<Seperator> {\n'
        src += '        let hash = ch as u32;\n'
        src += '        match LEN1_BUCKET[(hash % 37) as usize] {\n'
        src += '            (key, _) if key != hash => None,\n'
        src += '            (_, index) => unsafe { Some(::std::mem::transmute(index as u8)) },\n'
        src += '        }\n'
        src += '    }\n'
        src += '    pub fn parse3(ch1: char, ch2: char, ch3: char) -> Option<(Seperator, usize)> {\n'
        src += '        let hash2 = ch1 as u64 + ch2 as u64 * CHAR_MAX_CODEPOINT;\n'
        src += '        let hash1 = ch1 as u32;\n'
        src += '        match &[ch1 as u8, ch2 as u8, ch3 as u8] {\n'   # if len(self.len3s) is large than 10 or 100, make it into hashmap, too
        for sep in self.len3s:
            src += '            b"%s" => unsafe { Some((::std::mem::transmute(%du8), 3)) },\n' % (sep.value, sep.index)
        src += '            _ => match LEN2_BUCKET1[(hash2 % 22) as usize] {\n'
        src += '                (key, _) if key != hash2 => match LEN2_BUCKET2[(hash2 % 22) as usize] {\n'
        src += '                    (key, _) if key != hash2 => match LEN1_BUCKET[(hash1 % 37) as usize] {\n'
        src += '                        (key, _) if key != hash1 => None,\n'
        src += '                        (_, index) => unsafe { Some((::std::mem::transmute(index), 1)) },\n'
        src += '                    },\n'
        src += '                    (_, index) => unsafe { Some((::std::mem::transmute(index + %d), 2)) },\n' % len(self.len1s)
        src += '                },\n'
        src += '                (_, index) => unsafe { Some((::std::mem::transmute(index + %d), 2)) },\n' % len(self.len1s)
        src += '            },\n'
        src += '        }\n'
        src += '    }\n'
        src += '}\n'

        src += 'impl ::std::fmt::Debug for Seperator {\n'
        src += '    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {\n'
        src += '        const SEP_VALUES: &[&str] = &['
        src += ', '.join(map(
            lambda x: '{opt_space}"{value}"'.format(value = x[1].value, opt_space = '\n            ' if x[0] % 12 == 0 else ''), enumerate(chain(self.len1s, self.len2s, self.len3s))))
        src += '\n'
        src += '        ];\n'
        src += '        unsafe { write!(f, "{}",\n'
        src += '            SEP_VALUES[::std::mem::transmute_copy::<Seperator, u8>(self) as usize]\n'
        src += '        ) }\n'
        src += '    }\n'
        src += '}\n'

        src += 'impl Seperator {\n\n'
        src += '    pub fn is_category(&self, cat: u16) -> bool {\n'
        src += '        const SEP_CATS: &[u16] = &['
        src += ', '.join(map(
            lambda x: '{opt_space}{value}'.format(value = x[1].cats_value, opt_space = '\n            ' if x[0] % 12 == 0 else ''), enumerate(chain(self.len1s, self.len2s, self.len3s))
        ))
        src += '\n'
        src += '        ];\n'
        src += '        unsafe {\n'
        src += '            (SEP_CATS[::std::mem::transmute_copy::<Seperator, u8>(self) as usize] & cat) == cat\n'
        src += '        }\n'
        src += '    }\n'
        src += '}\n'

        src += self.generate_tests()

        with codecs.open(SEPERATOR_TARGET_FILE, 'w', 'utf-8') as file:
            file.write(src)

class Keyword:
    def __init__(self, value, name, index, cat):
        self.value, self.name, self.index, self.cat = value, name, index, cat
    def __str__(self):
        return f'keyword({self.value}, {self.name}, {self.index}, {self.cat}: {self.cat_value})'
    def __lt__(self, rhs):
        return self.value < rhs.value
    def update_index(self, new_index):
        self.index = new_index
        return self
    def update_cat_value(self, cats):
        self.cat_value = cats[self.cat]
        return self
class Keywords:
    def __init__(self, filename):
        self.filename = filename
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
        self.cats = { 'InUse': 1, 'Primitive': 2, 'Reserved': 3 }
        self.items.sort()
        self.items = [keyword.update_index(index).update_cat_value(self.cats) for (index, keyword) in enumerate(self.items)]

    def __str__(self):
        return 'keyword in ' + self.filename + ':'          \
            + '\n  comments:\n  ' + '\n  '.join(self.comments) \
            + '\n  items:\n    ' + '\n    '.join(map(str, self.items))

    # format and write back
    def format(self):
        retval = '\n'.join(self.comments)
        max_value_length, max_name_length = 0, 0
        for kw in self.items:
            max_value_length = max(max_value_length, len(kw.value))
            max_name_length = max(max_name_length, len(kw.name))
        for kw in self.items:
            retval += "\n {space1}'{value}' => {space2}{name}, {cat},".format(
                space1 = ' ' * (max_value_length - len(kw.value)), value = kw.value,
                space2 = ' ' * (max_name_length - len(kw.name)), name = kw.name,
                cat = kw.cat
            )
        with open(self.filename, 'w') as file:
            file.write(retval)

    def generate_hash_specs(self):
        # 16557366432705, 43 => 137, 2
        # 16557366432689, 43 => 137, 2
        bucket_size, bucket_number, memory_use = check_hasher_perform(
            list(map(lambda x: x.value, self.items)),
            lambda value: reduce(lambda x, y: x * (ord(y) - 43) % 16557366432705, value, 1))
        print(f'keyword all: bucket size: {bucket_size}, number: {bucket_number}, memory use: {memory_use}')

    def generate_tests(self):
        test_src = ''

        test_src += '\n#[cfg(test)] #[test]\n'
        test_src += 'fn keyword_format() {\n\n'
        for _ in range(10):
            kw = random.choice(self.items)
            test_src += '    assert_eq!{ format!("{:?}", Keyword::' + kw.name + '), "' + kw.value + '" }\n'
        test_src += '}\n'

        test_src += '#[cfg(test)] #[test]\n'
        test_src += 'fn keyword_cat() {\n\n'
        cases = []
        for _ in range(10):
            kw = { 
                1: lambda: random.choice(list(filter(lambda x: x.cat in ['Primitive', 'InUse'], self.items))), 
                2: lambda: random.choice(list(filter(lambda x: x.cat == 'Reserved', self.items)))
            }[random.choice([1, 1, 1, 2])]()  # more primitive or inuse, less reserved
            for cat in set([random.choice(['InUse', 'Primitive', 'Reserved']), random.choice(['InUse', 'Primitive', 'Reserved'])]):
                if cat == 'Primitive':
                    cases.append('    assert_eq!{ Keyword::' + kw.name + '.is_primitive(), ' + ('true' if cat == kw.cat else 'false') + ' }\n')
                elif cat == 'Reserved':
                    cases.append('    assert_eq!{ Keyword::' + kw.name + '.is_reserved(), ' + ('true' if cat == kw.cat else 'false') + ' }\n')
                elif cat == 'InUse' and cat == kw.cat:
                    cases.append('    assert_eq!{ Keyword::' + kw.name + '.is_primitive(), false }\n')
                    cases.append('    assert_eq!{ Keyword::' + kw.name + '.is_reserved(), false }\n')
        for case in set(cases):
            test_src += case
        test_src += '}\n'

        test_src += '#[cfg(test)] #[test]\n'
        test_src += 'fn keyword_parse() {\n\n'
        test_src += '    assert_eq!{ Keyword::parse("fn"), Some(Keyword::Fn) }\n'
        test_src += '    assert_eq!{ Keyword::parse("await"), Some(Keyword::Await) }\n'
        test_src += '    assert_eq!{ Keyword::parse("一个chinese变量"), None }\n'
        test_src += '    assert_eq!{ Keyword::parse("a_中文_var"), None }\n'
        # this test case from v2_base help me find that if _invalid_key in BUCKET1 should still check BUCKET2
        test_src += '    assert_eq!{ Keyword::parse("as"), Some(Keyword::As) }\n'
        test_src += '}\n'

        return test_src

    def generate(self):
        src = ''
        src += '///! fff-lang\n'
        src += '///!\n'
        src += '///! lexical/keyword\n'
        src += '///! Attention: contens are auto generated by token.py, do not modify this file\n\n'

        src += '#[derive(Eq, PartialEq, Clone, Copy)]\n'
        src += 'pub enum Keyword {'
        src += ''.join(f'\n    {kw.name},' for kw in self.items)
        src += '\n}\n'

        src += '\nconst KEYWORD_VALUES: &[&str] = &['    # thanks for 1.18's const static default 'static
        src += ''.join('{opt_space}"{value}", '.format(value = kw.value, opt_space = '\n    ' if kw.index % 7 == 0 else '') for kw in self.items)
        src += '\n];\n'
        HASH_MAGIC = 16557366432705
        # this comes back to only store values, but later use the value as index into VALUES array and then check key equality there
        buckets = [[255, 255] for _ in range(137)]  # actually [(bucket1, bucket2)], but use array for mutablity
        for kw in chain(
            filter(lambda x: x.cat_value == self.cats['InUse'], self.items),
            filter(lambda x: x.cat_value == self.cats['Primitive'], self.items),
            filter(lambda x: x.cat_value == self.cats['Reserved'], self.items)):  # inuse in priority, then primitive, last reserved
            hashv = reduce(lambda x, y: x * (ord(y) - 43) % HASH_MAGIC, kw.value, 1)
            for char in kw.value:
                assert ord(char) >= 43
            # print(f'{kw}: {hashv}, {hashv % 137}')
            if buckets[hashv % 137][0] == 255:
                buckets[hashv % 137][0] = kw.index
            elif buckets[hashv % 137][1] == 255:
                buckets[hashv % 137][1] = kw.index
            else:
                assert not 'should not use more then 2 buckets'
        src += '\nconst EMPTY: u8 = 255;\n'
        src += 'const HASH_MAGIC: u64 = ' + str(HASH_MAGIC) + ';\n'
        src += 'const KEYWORD_BUCKET1: &[u8] = &['
        src += ''.join('{space}{value}, '.format(
            value = bucket1 if bucket1 != 255 else 'EMPTY', 
            space = '\n    ' if index % 16 == 0 else '') for (index, (bucket1, _)) in enumerate(buckets))
        src += '\n];\n'
        src += 'const KEYWORD_BUCKET2: &[u8] = &['
        src += ''.join('{space}{value}, '.format(
            value = bucket2 if bucket2 != 255 else 'EMPTY', 
            space = '\n    ' if index % 16 == 0 else '') for (index, (_, bucket2)) in enumerate(buckets))
        src += '\n];\n'
        src += 'impl Keyword {\n'
        src += '    pub fn parse(v: &str) -> Option<Keyword> {\n'
        src += '        let mut hash = 1u64;\n'
        src += '        for ch in v.chars() {\n'
        src += '            if ch as u32 <= 43 { return None; }\n'
        src += '            hash = (hash * (ch as u32 - 43u32) as u64) % HASH_MAGIC;\n'
        src += '        }\n'
        src += '        match KEYWORD_BUCKET1[(hash % 137) as usize] {\n'
        src += '            index if index != EMPTY && KEYWORD_VALUES[index as usize] == v\n'
        src += '                => Some(unsafe{ ::std::mem::transmute(index) }),\n'
        src += '            _empty_or_invalid_key => match KEYWORD_BUCKET2[(hash % 137) as usize] {\n'
        src += '                EMPTY => None,\n'
        src += '                index if KEYWORD_VALUES[index as usize] == v\n'
        src += '                    => Some(unsafe { ::std::mem::transmute(index) }),\n'
        src += '                _invalid_index => None,\n'
        src += '            },\n'
        src += '        }\n'
        src += '    }\n'
        src += '}\n'

        src += 'impl ::std::fmt::Debug for Keyword {\n'
        src += '    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {\n'
        src += '        write!(f, "{}", unsafe{ KEYWORD_VALUES[::std::mem::transmute_copy::<Keyword, u8>(self) as usize] })\n'
        src += '    }\n'
        src += '}\n\n'

        src += 'const KEYWORD_CATS: &[u8] = &['
        src += ''.join('{opt_space}{value}, '.format(value = kw.cat_value, opt_space = '\n    ' if kw.index % 16 == 0 else '') for kw in self.items)
        src += '\n];\n'
        src += 'impl Keyword {\n'
        src += '    pub fn is_primitive(&self) -> bool {\n'
        src += '        KEYWORD_CATS[unsafe{ ::std::mem::transmute_copy::<Keyword, u8>(self) as usize }] == ' + str(self.cats['Primitive']) + '\n'
        src += '    }\n'
        src += '    pub fn is_reserved(&self) -> bool {\n'
        src += '        KEYWORD_CATS[unsafe{ ::std::mem::transmute_copy::<Keyword, u8>(self) as usize }] == ' + str(self.cats['Reserved']) + '\n'
        src += '    }\n'
        src += '}\n'

        src += self.generate_tests()

        with codecs.open(KEYWORD_TARGET_FILE, 'w', 'utf-8') as file:
            file.write(src)

# main
seperators = Seperators(SEPERATOR_DEF_FILE)
keywords = Keywords(KEYWORD_DEF_FILE)
#print(seperators)
#print(keywords)
#seperators.format()
#keywords.format()
#seperators.generate_hash_specs()
#keywords.generate_hash_specs()
#seperators.generate()
keywords.generate()