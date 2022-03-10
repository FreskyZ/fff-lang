#!python3

# generate char extension method is_xid_{start|continue} and check_confusable for lexical parser
# include is_numeric_start and is_numeric_continue by the way

from collections import namedtuple
import math
import random
import re
import sys

from token import Separators, SEPARATOR_DEF_FILE, format_wrap, HashChecker

# IDENTIFIER AND PATTERN SYNTAX
# https://unicode.org/reports/tr31/
# https://unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt
DCP_FILE = '/tmp/DerivedCoreProperties.txt'
# EMOJI
# https://www.unicode.org/reports/tr51/
# https://www.unicode.org/Public/UCD/latest/ucd/emoji/emoji-data.txt
# additional acceptable identifier code points
# tr31 does not include them, but support them is fucking interesting
EMOJI_FILE = '/tmp/emoji-data.txt'
# SECURITY CONSIDERATIONS
# https://www.unicode.org/reports/tr36/
# https://unicode.org/Public/security/latest/confusables.txt
CONFUSABLE_FILE = '/tmp/confusables.txt'
# additional confusable included in this file
# - it makes me confuse that confusables.txt does not include them
#   e.g. CJK period in my original manual test
# - it also makes me confuse that rust parser says its data comes from
#   confusables.txt but included some of these but does not mention that
# - this file claims it comes from UnicodeData.txt but these additional items
#   seems to be added by human
# - there are approximately more than 100 more additional items, so need to 
#   parse this file, although this file claims it is not designed to be parsed
# https://www.unicode.org/Public/UCD/latest/ucd/NamesList.txt
NAME_FILE = '/tmp/NamesList.txt'

XID_OUTPUT_FILE = 'src/lexical/unicode/xid.rs'
CONFUSABLE_OUTPUT_FILE = 'src/lexical/unicode/security.rs'
TEST_OUTPUT_FILE = 'src/lexical/unicode/tests.rs'

# use $(unicode.py detail 2> unicode-detail.txt) with $(cargo run --features=trace_xid) to debug xid
DETAIL = 'detail' in sys.argv

RE_XID_START = re.compile('^(?P<begin>\w{4,5})(?:\.\.)?(?P<end>\w{4,5})?\s*;\sXID_Start')
RE_XID_CONTINUE = re.compile('^(?P<begin>\w{4,5})(?:\.\.)?(?P<end>\w{4,5})?\s*;\sXID_Continue')
RE_EMOJI = re.compile('^(?P<begin>\w{4,5})(?:\.\.(?P<end>\w{4,5}))?\s*;\sEmoji_Presentation')
RE_CONFUSABLE = re.compile('^(?P<from>\w{4,5}) ;\\t00(?P<to>\w{2}) ;.*\) (?P<fromname>[\w\s\-]+) ')
RE_NAME_START = re.compile('^00(?P<point>\w\w)\t(?P<name>[\w\s\-<>]+)$')
RE_NAME_CONFUSABLE = re.compile('^\tx \((?P<name>.*) - (?P<from>\w+)')

Run = namedtuple('Run', 'start, value, size')
Lookup = namedtuple('Lookup', 'start, run_index, offset')
Confusable = namedtuple('Confusable', ['f', 't', 'name'])

def collect_xid_points(header, dcp_file, expr):
    points = []
    vlranges = [] # very large ranges, for >0x20000 CJK ranges
    for match in (m for m in (expr.match(line) for line in open(dcp_file).readlines()) if m):
        begin = int('0x' + match.group('begin'), 16)
        end = int('0x' + match.group('end'), 16) if match.group('end') else 0
        if begin >= 0x20000:
            if end == 0:
                raise ValueError('unexpected not range very large item')
            vlranges.append((begin, end))
            continue
        if end:
            for point in range(begin, end + 1):
                points.append(point)
        else:
            points.append(begin)
    print(f'{header} points: {len(points)} very large ranges: {len(vlranges)}')
    return points, vlranges

def collect_emoji_points(emoji_file):
    points = []
    for match in (m for m in (RE_EMOJI.match(line) for line in open(emoji_file).readlines()) if m):
        begin = int('0x' + match.group('begin'), 16)
        end = int('0x' + match.group('end'), 16) if match.group('end') else 0
        assert begin < 0x20000 # no very large for emoji
        if end:
            for point in range(begin, end + 1):
                points.append(point)
        else:
            points.append(begin)
    if DETAIL:
        for point in points:
            print(chr(point), end='')
    print()
    print(f'emoji points: {len(points)}')
    return points

def process_xid_points_rle(header, points):

    ascii_points = [p for p in points if p < 128]
    ascii_words = [['0' for _ in range(0, 64)] for _ in range(0, 2)]
    for point in ascii_points:
        ascii_words[0 if point < 64 else 1][63 - point if point < 64 else 127 - point] = '1'
    ascii_words = [int(''.join(word), 2) for word in ascii_words]
    print(f'{header} ascii words {ascii_words}')

    points = [p for p in points if p >= 128]
    # element size in run
    # larger word means larger but less run, smaller word means smaller but more run
    # for xid bitset,
    # it seems that smaller word size uses less memory for run array (run size * run count)
    # but more run means larger lookup table or longer lookup time, so it is selected as 16bit
    word_size = 16

    word_count = max(points) // word_size + 1
    words = [['0' for _ in range(0, word_size)] for _ in range(0, word_count)]
    for point in points:
        word_index = point // word_size
        words[word_index][(word_index + 1) * word_size - point - 1] = '1'
    words = [int(''.join(word), 2) for word in words]
    print(f'{header} word count {len(words)}')

    # array of Run
    # start: starting code point
    # value: word value
    # size: run size, use array to mutate
    runs = []
    for (word_index, word) in enumerate(words):
        if len(runs) and word == runs[-1].value and runs[-1].size[0] < 256:
            runs[-1].size[0] += 1
        else:
            runs.append(Run(word_index * word_size, word, [1]))
    if DETAIL:
        for run_index, run in enumerate(runs):
            print(f'#{run_index} {run}', file=sys.stderr)
    print(f'{header} run count {len(runs)} run array size {len(runs) * (word_size // 8 + 1)} bytes')
    assert 512 < len(runs) < 1024

    # one lookup array item mapping to how many run element
    # or, after lookup find index in run array, how many run elements need to be inspected to find the correct one at max
    # larger lookup size means smaller lookup array size and longer lookup time
    # smaller lookup size means larger lookup array size and less lookup time
    lookup_size = 8
    # array of Lookup
    # start: start point
    # run_index: run array index
    # offset: run element offset, it is logically array index for this word when regarding one run element as an array of words
    lookups = []

    runs.reverse()
    lookup_count = max(points) // word_size // lookup_size
    for start_point in range(0, max(points), word_size * lookup_size):
        run_rev_index, run = next((i, r) for i, r in enumerate(runs) if start_point >= r.start)
        run_index = len(runs) - run_rev_index - 1
        offset = (start_point - run.start) // word_size
        lookups.append(Lookup(start_point, run_index, offset))
    runs.reverse()

    if DETAIL:
        for lookup_index, lookup in enumerate(lookups):
            print(f'#{lookup_index} start point {lookup.start} lookups #{lookup.run_index} {runs[lookup.run_index]} offset {lookup.offset}', file=sys.stderr)
    print(f'{header} lookup count {lookup_count} lookup index array size {lookup_count // 6 * 8} bytes lookup offset array size {lookup_count} bytes')
    return ascii_words, runs, lookups

def generate_header():
    b = ''
    b += '///! --------------------------------------------------------------------------------\n'
    b += '///! This code is auto generated by a tool\n'
    b += '///! Changes may cause incorrect behavior and will be lost if the code is regenerated\n'
    b += '///! --------------------------------------------------------------------------------\n'
    b += '\n'
    return b

def generate_xid_arrays(ident, asciis, runs, lookups, boundary, vlranges):
    b = ''

    b += f'const {ident}_ASCII_1: u64 = {asciis[0]};\n'
    b += f'const {ident}_ASCII_2: u64 = {asciis[1]};\n'

    b += f'const {ident}_NOT_LARGE_MAX: u32 = {boundary};\n'
    b += f'const {ident}_VERY_LARGE_RANGES: &[(u32, u32)] = &[\n'
    b += '    '
    for vlrange in vlranges:
        b += f'{vlrange}, '
    b = b[:-1]
    b += '\n'
    b += '];\n'

    # encode Run as binary struct { value: u16, size: u8 }[], little endian
    # to bytes
    run_bytes = []
    for run in runs:
        run_bytes.append(run.value % 256)
        run_bytes.append(run.value // 256)
        run_bytes.append(run.size[0] - 1) # size is 1 to 256, need to sub 1
    # pad
    if len(run_bytes) % 8:
        for _ in range(0, 8 - (len(run_bytes) % 8)):
            run_bytes.append(0)
    # to u64
    run_u64s = []
    for index in range(0, len(run_bytes) // 8):
        run_u64s.append(sum(run_bytes[index * 8 + i] * (256 ** i) for i in range(0, 8)))
    b += '\n'
    b += f'const {ident}_RUNS: &[u64] = &[ '
    for index, run_u64 in enumerate(run_u64s):
        if index % 6 == 0:
            b = b[:-1]
            b += '\n    '
        b += f'{run_u64}, '
    b.rstrip()
    b += '\n'
    b += '];\n'

    # encode lookup index by put every 6 index as u10 inside u64, start from smallest bit
    lookup_indices = [k.run_index for k in lookups]
    # pad
    if len(lookup_indices) % 6:
        for _ in range(0, 6 - (len(lookup_indices) % 6)):
            lookup_indices.append(0)
    # to u64
    lookup_index_u64s = []
    for index in range(0, len(lookup_indices) // 6):
        lookup_index_u64s.append(sum(lookup_indices[index * 6 + i] * (1024 ** i) for i in range(0, 6)))
    b += f'const {ident}_LOOKUP_INDICES: &[u64] = &[ '
    for index, lookup_index_u64 in enumerate(lookup_index_u64s):
        if index % 6 == 0:
            b = b[:-1]
            b += '\n    '
        b += f'{lookup_index_u64}, '
    b.rstrip()
    b += '\n'
    b += '];\n'

    # lookup offset is normal u8 array
    lookup_offsets = [k.offset for k in lookups]
    # pad
    if len(lookup_offsets) % 8:
        for _ in range(0, 8 - (len(lookup_offsets) % 8)):
            lookup_offsets.append(0)
    # to u64
    lookup_offset_u64s = []
    for index in range(0, len(lookup_offsets) // 8):
        lookup_offset_u64s.append(sum(lookup_offsets[index * 8 + i] * (256 ** i) for i in range(0, 8)))
    b += f'const {ident}_LOOKUP_OFFSETS: &[u64] = &[ '
    for index, lookup_offset_u64 in enumerate(lookup_offset_u64s):
        if index % 6 == 0:
            b = b[:-1]
            b += '\n    '
        b += f'{lookup_offset_u64}, '
    b.rstrip()
    b += '\n'
    b += '];\n'

    return b

def generate_xid_function(name, ident):
    b = ''
    
    b += f'pub fn {name}(c: char) -> bool {{\n'
    b += '    let cp = c as u32;\n'
    # most common case is 64 <= point < 128
    b += '    if cp & 0xFFFF_FFC0 == 0x40 {\n'
    b += '        let test = 1 << (cp & 0x3F);\n'
    b += f'        {ident}_ASCII_2 & test == test\n'
    # then < 64
    b += '    } else if cp & 0xFFFF_FFC0 == 0 {\n'
    b += '        let test = 1 << cp;\n'
    b += f'        {ident}_ASCII_1 & test == test\n'
    # then very large
    b += f'    }} else if cp > {ident}_NOT_LARGE_MAX {{\n'
    b += f'        {ident}_VERY_LARGE_RANGES.iter().any(|&(begin, end)| begin <= cp && end >= cp)\n'
    b += '    } else {\n'

    # RLE decoding part
    # it is logically
    # const LOOKUP_INDICES: uint10[];
    # const LOOKUP_OFFSETS: uint8[];
    # input char
    # var run_index = LOOKUP_INDICIES[char / 128]; // 128 is word size * lookup size
    # // this offset is the word offset between run (by run_index) starting code point to lookup starting code point, in which
    # // run starting code point is 16 * index for #index item in run array
    # // lookup starting code point is 128 * index for #index item in lookup arrray
    # var offset1 = LOOKUP_OFFSETS[char / 128];
    # // this offset is the word offset between lookup starting code point to input code point, or number of full words between lookup starting code point (inclusive) and input code point (exclusive)
    # var offset2 = (char % 128) / 16;
    # // then this offset is the word offset between run (by run_index) starting code point to input code point, or number of full words
    # var offset3 = offset1 + offset2;
    # const RUNS: { value: uint16, size: uint8 }[]; // .size=0 means run size 1, because max run size is 256, that uses .size=255
    # loop
    #     run = RUNS[run_index]
    #     if offset3 <= run.size // code point's word is in this run
    #         return test(run.value, char % 16)
    #     run_index += 1
    #     offset3 -= run.size
    b += '        const RUN_INDEX_SHIFTS: &[i32] = &[0, 10, 20, 30, 40, 50];\n'
    b += f'        macro_rules! run_byte {{ ($index:expr) => (({ident}_RUNS[$index >> 3] >> (($index & 7) << 3)) & 0xFF); }}\n'
    b += '        let lookup_index = (cp >> 7) as usize;\n'
    b += f'        let run_index_packed = {ident}_LOOKUP_INDICES[lookup_index / 6];\n'
    b += '        let mut run_index = ((run_index_packed >> RUN_INDEX_SHIFTS[lookup_index % 6]) & 0x3FF) as usize;\n'
    b += f'        let offset_packed = {ident}_LOOKUP_OFFSETS[lookup_index >> 3];\n'
    b += '        let offset_run_to_lookup = (offset_packed >> ((lookup_index & 7) << 3)) & 0xFF;\n'
    b += '        let offset_lookup_to_input = ((cp & 0x7F) >> 4) as u64;\n'
    b += '        let mut offset = offset_run_to_lookup + offset_lookup_to_input;\n'
    b += '        #[cfg(feature = "trace_xid")]\n'
    b += f'        println!("{ident} char {{c:?}} point {{cp}} lookup index {{lookup_index}} offset1 {{offset_run_to_lookup}} offset2 {{offset_lookup_to_input}}");\n'
    b += '        loop {\n'
    b += '            let run_size = run_byte!(run_index * 3 + 2);\n'
    b += '            if run_size >= offset {\n'
    b += '                let run_value = run_byte!(run_index * 3) + (run_byte!(run_index * 3 + 1) << 8);\n'
    b += '                let test = 1 << (cp & 0xF);\n'
    b += '                #[cfg(feature = "trace_xid")]\n'
    b += '                println!("run index {}, run size {} run value {:x} test bit {}", run_index, run_size, run_value, test);\n'
    b += '                break run_value & test == test;\n'
    b += '            } else {\n'
    b += '                #[cfg(feature = "trace_xid")]\n'
    b += '                println!("run index {} run size {} not enough, check next", run_index, run_size);\n'
    b += '                offset -= run_size + 1;\n'
    b += '                run_index += 1;\n'
    b += '            }\n'
    b += '        }\n'
    b += '    }\n'
    b += '}\n'

    return b

def generate_xid_tests(name, points, vlranges):
    b = ''

    b += '\n'
    b += '#[test]\n'
    b += f'fn test_{name}() {{\n'

    # manual or persisted tests
    if name == 'is_xid_start':
        b += '    assert_eq!(is_xid_start(\'a\'), true);\n'
        b += '    assert_eq!(is_xid_start(\'\\u{554A}\'), true);\n'
        b += '    assert_eq!(is_xid_start(\',\'), false);\n'
        b += '    assert_eq!(is_xid_start(\'\\u{FF0C}\'), false);\n'
        b += '    assert_eq!(is_xid_start(\'_\'), true);\n'
        b += '    assert_eq!(is_xid_start(\'.\'), false);\n'
        b += '    assert_eq!(is_xid_start(\'1\'), false);\n'
        b += '    assert_eq!(is_xid_start(\'\\u{1F21}\'), true);\n'
    else:
        b += '    assert_eq!(is_xid_continue(\'a\'), true);\n'
        b += '    assert_eq!(is_xid_continue(\'\\u{554A}\'), true);\n'
        b += '    assert_eq!(is_xid_continue(\',\'), false);\n'
        b += '    assert_eq!(is_xid_continue(\'\\u{FF0C}\'), false);\n'
        b += '    assert_eq!(is_xid_continue(\'_\'), true);\n'
        b += '    assert_eq!(is_xid_continue(\'.\'), false);\n'
        b += '    assert_eq!(is_xid_continue(\'1\'), true);\n'
        b += '    assert_eq!(is_xid_continue(\'\\u{1F21}\'), true);\n'


    used_points = [0x61, 0x554A, 0x44, 0xFF0C, 0x5F, 0x2E, 0x31, 0x1F21]
    for _ in range(0, 100):
        chartype = random.randint(0, 100)
        point = random.randint(0, 128) if chartype < 50 else random.randint(0, 0x20000) if chartype < 90 else random.randint(0, 0x10FFFF)
        if point in used_points:
            continue
        if 0xD800 <= point <= 0xE000:
            continue
        used_points.append(point)
        expected = point in points if point < 0x20000 else any(r for r in vlranges if r[0] <= point <= r[1])
        char = chr(point)
        unescape = '\\\'' if char == '\'' else '\\n' if point == 13 else '\\r' if point == 10 else '\\t' if char == '\t' else '\\\\' if char == '\\' else char
        b += f'    assert_eq!({name}(/* U+{point:04X} */ \'{unescape}\'), {"true" if expected else "false"});\n'

    b += '}\n'

    return b

def collect_confusables(separator_file, names_file, confusable_file):
    confusable_tos = [ord(c) for sep in Separators(separator_file).all_items for c in sep.value]
    # these are not defined in separator but used in parser
    confusable_tos.extend((ord(' '), ord('\\'), ord('_'), ord("'"), ord('"')))
    confusable_tos = [[c, ''] for c in sorted(set(confusable_tos))]

    current_to = 0
    confusables = []
    for line in open(names_file).readlines():
        match = RE_NAME_START.match(line)
        if match:
            point = int('0x' + match.group('point'), 16)
            if point >= 127:
                break
            confusable_to = next((t for t in confusable_tos if t[0] == point), None)
            if confusable_to is not None:
                current_to = point
                confusable_to[1] = match.group('name').strip()
            else:
                current_to = 0
        if current_to:
            match = RE_NAME_CONFUSABLE.match(line)
            if match:
                confusables.append(Confusable(int('0x' + match.group('from'), 16), current_to, match.group('name').upper()))

    

    for match in (m for m in (RE_CONFUSABLE.match(line) for line in open(confusable_file).readlines()) if m):
        from_point = int('0x' + match.group('from'), 16)
        to_point = int('0x' + match.group('to'), 16)
        if next((p for p in confusable_tos if p[0] == to_point), None):
            confusables.append(Confusable(from_point, to_point, match.group('fromname')))

    # it's more confuse that fullwidth comma is not in confusable.txt and NamesList.txt, which can be easily inputed in my environment
    # after searching NamesList.txt, I found that the confusable list is recursive. I prefer manually add for them
    # they are appended last to have highest priority
    # confusables.append(Confusable(0xb7, ord('`'), 'MIDDLE DOT'))
    confusables.append(Confusable(0x2014, ord('_'), 'EM DASH'))
    confusables.append(Confusable(0x2018, ord('\''), 'LEFT SINGLE QUOTATION MARK'))
    confusables.append(Confusable(0x2019, ord('\''), 'RIGHT SINGLE QUOTATION MARK'))
    confusables.append(Confusable(0x201C, ord('"'), 'LEFT DOUBLE QUOTATION MARK'))
    confusables.append(Confusable(0x201D, ord('"'), 'RIGHT DOUBLE QUOTATION MARK'))
    confusables.append(Confusable(0x2026, ord('^'), 'HORIZONTAL ELLIPSIS'))
    confusables.append(Confusable(0x3001, ord('\\'), 'IDEOGRAPHIC COMMA'))
    confusables.append(Confusable(0x3002, ord('.'), 'IDEOGRAPHIC FULL STOP'))
    confusables.append(Confusable(0x300A, ord('<'), 'LEFT DOUBLE ANGLE BRACKET'))
    confusables.append(Confusable(0x300B, ord('>'), 'RIGHT DOUBLE ANGLE BRACKET'))
    confusables.append(Confusable(0x3010, ord('['), 'LEFT BLACK LENTICULAR BRACKET'))
    confusables.append(Confusable(0x3011, ord(']'), 'RIGHT BLACK LENTICULAR BRACKET'))
    confusables.append(Confusable(0xFF1B, ord(';'), 'FULL WIDTH SEMICOLON'))
    confusables.append(Confusable(0xFF01, ord('!'), 'FULLWIDTH EXCLAMATION MARK'))
    confusables.append(Confusable(0xFF0C, ord(','), 'FULLWIDTH COMMA'))
    confusables.append(Confusable(0xFF08, ord('('), 'FULLWIDTH LEFT PARENTHESIS'))
    confusables.append(Confusable(0xFF09, ord(')'), 'FULLWIDTH RIGHT PARENTHESIS'))
    confusables.append(Confusable(0xFF1A, ord(':'), 'FULLWIDTH COLON'))
    # confusables.append(Confusable(0xFF1F, ord('?'), 'FULLWIDTH QUESTION MARK'))
    # confusables.append(Confusable(0xFFE5, ord('$'), 'FULLWIDTH YEN SIGN'))

    # reverse sort and dedup, which means newer items are kept
    new_confusables = []
    for c in list(sorted(confusables, key=lambda c: c.f)):
        if next((e for e in new_confusables if e.f == c.f), None) is None:
            new_confusables.append(c)
    confusables = list(sorted(new_confusables, key=lambda r: (r.t, r.f)))

    if DETAIL:
        for to in confusable_tos:
            print(f'confusable to U+{to[0]:04X} \'{chr(to[0])}\' {to[1]}')
        for conf in confusables:
            print(conf)
    print(f'confusable target count {len(confusable_tos)}')
    print(f'confusable count {len(confusables)}')
    return confusable_tos, confusables

def generate_confusable(confusable_tos, confusables):
    b = ''

    # # try find mod to make only one bucket
    # # result for v14.0 is % 491: 3 x 273 for 273 items, score 3.0
    # best_score = 100
    # best_score_moders = []
    # for i in range(len(confusables) // 6, 166):
    #     moder = i * 6 - 1
    #     result = HashChecker([c.f % moder for c in confusables])
    #     if result.score < best_score:
    #         best_score = result.score
    #         best_score_moders = [moder]
    #     elif result.score == best_score:
    #         best_score_moders.append(moder)
    #     print(f'% {moder}: {result}')
    #     moder = i * 6 + 1
    #     result = HashChecker([c.f % moder for c in confusables])
    #     if result.score < best_score:
    #         best_score = result.score
    #         best_score_moders = [moder]
    #     elif result.score == best_score:
    #         best_score_moders.append(moder)
    #     print(f'% {moder}: {result}')
    # print(f'best score {best_score} moders {best_score_moders}')
    # return b

    moder = 491
    hash_config = HashChecker([c.f % moder for c in confusables])
    assert hash_config.bucket_count == 3
    print(f'confusable config % {moder} {hash_config}')

    bucket1 = [0 for _ in range(0, hash_config.bucket_size)]
    bucket2 = [0 for _ in range(0, hash_config.bucket_size)]
    bucket3 = [0 for _ in range(0, hash_config.bucket_size)]
    for (name_index, confusable) in enumerate(confusables):
        subscription_index = next((i for i, t in enumerate(confusable_tos) if t[0] == confusable.t), None)
        assert subscription_index is not None, f"subscription index is none for {confusable}"
        hash_value = confusable.f % moder % hash_config.bucket_size
        if bucket1[hash_value] != 0:
            if bucket2[hash_value] != 0:
                bucket3[hash_value] = (confusable.f, name_index, subscription_index)
            else:
                bucket2[hash_value] = (confusable.f, name_index, subscription_index)
        else:
            bucket1[hash_value] = (confusable.f, name_index, subscription_index)
    b += '\n'
    b += 'const EMPTY: (u32, u16, u8) = (0, 0, 0);\n'
    b += 'const BUCKET1: &[(u32, u16, u8)] = &[\n'
    b += format_wrap(1, [str(i) if i != 0 else 'EMPTY' for i in bucket1])
    b += '\n'
    b += '];\n'
    b += 'const BUCKET2: &[(u32, u16, u8)] = &[\n'
    b += format_wrap(1, [str(i) if i != 0 else 'EMPTY' for i in bucket2])
    b += '\n'
    b += '];\n'
    b += 'const BUCKET3: &[(u32, u16, u8)] = &[\n'
    b += format_wrap(1, [str(i) if i != 0 else 'EMPTY' for i in bucket3])
    b += '\n'
    b += '];\n'
    b += 'const NAMES: &[&str] = &[\n'
    b += format_wrap(1, [f'"{c.name}"' for c in confusables])
    b += '\n'
    b += '];\n'
    b += 'const SUBSCRIPTIONS: &[(char, &str)] = &[\n'
    b += format_wrap(1, [f'(\'' + ("\\'" if chr(t[0]) == "'" else "\\\\" if chr(t[0]) == "\\" else chr(t[0])) + f'\', "{t[1]}")' for t in confusable_tos])
    b += '\n'
    b += '];\n'

    b += '\n'
    b += 'pub fn check_confusable(c: char) -> Option<(char, &\'static str, char, &\'static str)> {\n'
    b += '    let cp = c as u32;\n'
    b += f'    let index = (cp % {moder} % {hash_config.bucket_size}) as usize;\n'
    b += '    let (match1, match2, match3) = (BUCKET1[index], BUCKET2[index], BUCKET3[index]);\n'
    b += '    if match1.0 == cp {\n'
    b += '        let name = NAMES[match1.1 as usize];\n'
    b += '        let (subscription_char, subscription_name) = SUBSCRIPTIONS[match1.2 as usize];\n'
    b += '        Some((c, name, subscription_char, subscription_name))\n'
    b += '    } else if match2.0 == cp {\n'
    b += '        let name = NAMES[match2.1 as usize];\n'
    b += '        let (subscription_char, subscription_name) = SUBSCRIPTIONS[match2.2 as usize];\n'
    b += '        Some((c, name, subscription_char, subscription_name))\n'
    b += '    } else if match3.0 == cp {\n'
    b += '        let name = NAMES[match3.1 as usize];\n'
    b += '        let (subscription_char, subscription_name) = SUBSCRIPTIONS[match3.2 as usize];\n'
    b += '        Some((c, name, subscription_char, subscription_name))\n'
    b += '    } else {\n'
    b += '        None\n'
    b += '    }\n'
    b += '}\n'

    return b

def generate_confusable_tests(confusable_tos, confusables):
    b = ''
    def quote(c):
        return f"\{c}" if c == "'" or c == '\\' else c

    b += '\n'
    b += '#[test]\n'
    b += 'fn test_check_confusables() {\n'

    # full positive cover
    for c in confusables:
        to_name = next(t[1] for t in confusable_tos if t[0] == c.t)
        b += f'    assert_eq!(check_confusable(\'{quote(chr(c.f))}\'), Some((\'{quote(chr(c.f))}\', "{c.name}", \'{quote(chr(c.t))}\', "{to_name}")));\n'

    # random negative
    for _ in range(0, 100):
        point = random.randint(0, 0x20000)
        if 0xD800 <= point <= 0xE000:
            continue
        if next((t for t in confusables if t.f == point), None) is None:
            b += f'    assert_eq!(check_confusable(\'{quote(chr(point))}\'), None);\n'

    b += '}\n'

    return b

if __name__ == '__main__':

    emoji_points = collect_emoji_points(EMOJI_FILE)
    
    start_points, start_vlranges = collect_xid_points('start', DCP_FILE, RE_XID_START)
    start_points.append(0x5F) # allow underline as id start, this is optional according to standard
    start_points.extend(emoji_points)
    start_ascii, start_runs, start_lookups = process_xid_points_rle('start', start_points)
    start_arrays = generate_xid_arrays('XID_START', start_ascii, start_runs, start_lookups, max(start_points), start_vlranges)
    start_function = generate_xid_function('is_xid_start', 'XID_START')

    continue_points, continue_vlranges = collect_xid_points('continue', DCP_FILE, RE_XID_CONTINUE)
    continue_points.extend(emoji_points)
    continue_ascii, continue_runs, continue_lookups = process_xid_points_rle('continue', continue_points)
    continue_arrays = generate_xid_arrays('XID_CONTINUE', continue_ascii, continue_runs, continue_lookups, max(continue_points), continue_vlranges)
    continue_function = generate_xid_function('is_xid_continue', 'XID_CONTINUE')

    xid_code = generate_header() + start_arrays + '\n' + start_function + '\n' + continue_arrays + continue_function

    confusable_tos, confusables = collect_confusables(SEPARATOR_DEF_FILE, NAME_FILE, CONFUSABLE_FILE)
    confusables = [c for c in confusables if c.f not in emoji_points]
    confusable_code = generate_confusable(confusable_tos, confusables)

    start_test_code = generate_xid_tests('is_xid_start', start_points, start_vlranges)
    continue_test_code = generate_xid_tests('is_xid_continue', continue_points, continue_vlranges)
    confusable_test_code = generate_confusable_tests(confusable_tos, confusables)
    test_code = generate_header() + 'use super::*;\n' + start_test_code + continue_test_code + confusable_test_code

    with open(XID_OUTPUT_FILE, 'w') as file:
        file.write(xid_code)
    with open(CONFUSABLE_OUTPUT_FILE, 'w') as file:
        file.write(confusable_code)
    if 'test' in sys.argv:
        with open(TEST_OUTPUT_FILE, 'w') as file:
            file.write(test_code)
