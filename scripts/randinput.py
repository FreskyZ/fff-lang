#!python3
# random syntax parse test input generator

import os
import random
import sys
from functools import reduce

def list_to_str(chars):
    return reduce(lambda x, y: x + y, chars, '')

bin_digits = ['0', '1']
oct_digits = [chr(c) for c in range(48, 55)]
dec_digits = [chr(c) for c in range(48, 57)]
hex_digits = dec_digits * 2 + [chr(c) for c in range(65, 70)] + [chr(c) for c in range(97, 102)] # double dec digits to make every digit same possiblity
id_starts = [chr(c) for c in range(97, 122)] + [chr(c) for c in range(65, 80)] + ['_']
id_continues = id_starts + dec_digits
id_cjks = [chr(c) for c in range(0x4E00, 0xA014)]
str_lit_asciis = [chr(c) for c in range(32, 126)]
str_lit_escapes = ['\\n', '\\r', '\\t', '\\\\', '\\0', '\\\'', '\\"']
primitive_types = ['u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i33', 'i64', 'f32', 'f64', 'char', 'bool', 'string']
keywords = ['fn', 'if', 'else', 'while', 'break', 'continue', 'for', 'return', 'var', 'const', 'as', 'loop', 'this', 'self', 'true', 'false']
binary_operators = ['+', '*', '/', '%', '-', '&', '|', '^', '<', '>', '==', '<=', '>=', '<<', '>>', '&&', '||']
unary_operators = ['!', '-', '~', '&']

def get_random_bool():
    return random.choice(['true', 'false'])

def get_random_char_lit():
    return {
        1: lambda: random.choice(str_lit_asciis),
        2: lambda: random.choice(str_lit_escapes),
        3: lambda: f'\\u{random.randint(256, 65536):04X}',
        4: lambda: f'\\U{random.randint(65536, 0x10FFFF):08X}',
    }[random.choices([1, 2, 3, 4], weights=[100, 20, 3, 3])[0]]()

def get_random_str_lit():
    return ''.join(get_random_char_lit() for _ in range(random.randint(1, 16)))

def get_random_numeric():
    ''' 
    parameters in the impl is judged to make the result more 'natural'
    may generate overflow literal and
    literal starts with 0 but not an integral prefix
    treat them as random normal error
    '''
    int_postfixes = ['i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64']
    real_postfixes = ['r32', 'r64']
    
    return {
        1: lambda: '0b' + ''.join(random.choice(bin_digits) for _ in range(random.randint(1, 20))) 
            + (random.choice(int_postfixes) if random.randint(0, 100) > 60 else ''),
        2: lambda: '0o' + ''.join(random.choice(oct_digits) for _ in range(random.randint(1, 15))) 
            + (random.choice(int_postfixes) if random.randint(0, 100) > 60 else ''),
        3: lambda: '0x' + ''.join(random.choice(hex_digits) for _ in range(random.randint(1, 8))) 
            + (random.choice(int_postfixes) if random.randint(0, 100) > 60 else ''),
        4: lambda: '0d' + ''.join(random.choice(dec_digits) for _ in range(random.randint(1, 7))) 
            + (random.choice(int_postfixes) if random.randint(0, 100) > 60 else ''),
        5: lambda: ''.join(random.choice(dec_digits) for _ in range(random.randint(1, 8))) 
            + (random.choice(int_postfixes + real_postfixes) if random.randint(0, 100) > 75 else ''),
        6: lambda: ''.join(random.choice(dec_digits) for _ in range(random.randint(1, 8))) 
            + (('.' + ''.join(random.choice(dec_digits) for _ in range(random.randint(1, 8)))) if random.randint(0, 100) > 40 else '')
            + (('E'
                + { 1: '+', 2: '-', 3: ''}[random.choice([1, 2, 3])]
                + ''.join(random.choice(dec_digits) for _ in range(random.randint(1, 3)))) if random.randint(0, 100) > 60 else '')
            + (random.choice(real_postfixes) if random.randint(0, 100) > 75 else '')
    }[random.choice([1, 1, 2, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6])]()

def get_random_lit():
    return {
        1: lambda: f"'{get_random_char_lit()}'",
        2: lambda: f'"{get_random_str_lit()}"',
        3: lambda: get_random_numeric(),
        4: lambda: get_random_bool(),
    }[random.choice([1, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4])]()

def get_random_ident():
    return (random.choice(id_starts) if random.randint(0, 100) <= 93 else random.choice(id_cjks)) \
        + ''.join((random.choice(id_continues) if random.randint(0, 100) <= 93 else random.choice(id_cjks)) for _ in range(random.randint(0, 8)))

def _get_random_quoted_list(f, ch1, ch2, choocer): # item generator, quote char, list of 0-5 to specify relative possibility
    return {
        0: lambda: ch1 + ch2,
        1: lambda: ch1 + ', '.join(f() for _ in range(2)) + ',' + ch2,
        2: lambda: ch1 + ''.join(f() + ', ' for _ in range(3))[:-2] + ch2,
        3: lambda: ch1 + ''.join(f() + ', ' for _ in range(4))[:-2] + ch2,
        4: lambda: ch1 + ''.join(f() + ', ' for _ in range(5))[:-2] + ch2,
    }[random.choice(choocer)]()

def get_random_type(depth=0):
    if depth == 6:
        return random.choice(primitive_types)
    return {
        1: lambda: random.choice(primitive_types) if random.randint(0, 1) else get_random_ident(),
        2: lambda: '[' + get_random_ident() + '; ' + get_random_numeric() + ']',
        3: lambda: _get_random_quoted_list(lambda: get_random_type(depth + 1), '(', ')', [0, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4]),
        4: lambda: '&' + get_random_type(depth + 1),
        5: lambda: 'fn' \
            + _get_random_quoted_list(lambda: (get_random_ident() + ': ' if random.randint(0, 1) else '') + get_random_type(depth + 1), '(', ')', [0, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4]) \
            + ((' -> ' + get_random_type(depth + 1)) if random.randint(0, 1) else ''),
        6: lambda: ('::' if random.randint(0, 100) > 90 else '') \
            + '::'.join((get_random_ident() if random.randint(0, 100) < 85 else \
                (get_random_ident() + _get_random_quoted_list(lambda: get_random_type(depth + 1), '<', '>', [0, 1, 2, 2, 2, 2, 3, 4]))) for _ in range(random.randint(1, 6)))
    }[random.choice([1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 6, 6, 6, 6, 6])]()

def gen_random_primary_expr_test_case():
    return {
        1: get_random_ident,
        2: get_random_literal,
        3: lambda: '(' + gen_random_primary_expr_test_case() + ')',
        4: lambda: _get_random_quoted(gen_random_primary_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        5: lambda: _get_random_quoted(gen_random_primary_expr_test_case, '[', ']', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        6: lambda: '[' + gen_random_primary_expr_test_case() + ';' + gen_random_primary_expr_test_case() + ']',
    }[choice([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6])]()

def gen_random_postfix_expr_test_case():
    return {
        1: get_random_numeric,
        2: get_random_ident,
        3: lambda: gen_random_postfix_expr_test_case() + '.' + get_random_ident(),
        4: lambda: gen_random_postfix_expr_test_case() + _get_random_quoted(gen_random_postfix_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        5: lambda: gen_random_postfix_expr_test_case() + _get_random_quoted(gen_random_postfix_expr_test_case, '[', ']', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        6: lambda: gen_random_postfix_expr_test_case() + '.' + get_random_ident() + _get_random_quoted(gen_random_postfix_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
    }[choice([1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6])]()

def get_random_expr():
    pass

def gen_random_binary_expr_test_case():
    pass

def gen_random_expr_test_case():
    pass

if __name__ == '__main__':
    if len(sys.argv) == 3 and sys.argv[1] == 'bool':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_bool())
    elif len(sys.argv) == 3 and sys.argv[1] == 'char':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_char_lit())
    elif len(sys.argv) == 3 and sys.argv[1] == 'str':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_str_lit())
    if len(sys.argv) == 3 and sys.argv[1] == 'num':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_numeric())
    if len(sys.argv) == 3 and sys.argv[1] == 'lit':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_lit())
    elif len(sys.argv) == 3 and sys.argv[1] == 'ident':
        for _ in range(0, int(sys.argv[2])):
            print(get_random_ident())
    elif len(sys.argv) == 3 and sys.argv[1] == 'type':
        for _ in range(0, int(sys.argv[2])):
            print('$ ' + get_random_type())
    else:
        print('randinput.py bool|char|str|num|lit|ident|type 10')

# print([get_random_typeuse() for _ in range(0, 9)])

# print(reduce(lambda x, y: x + y + '\n', [gen_random_primary_expr_test_case() for _ in range(0, 10)], ''))
# print(reduce(lambda x, y: x + y + '\n', [gen_random_postfix_expr_test_case() for _ in range(0, 10)], ''))
#expr = gen_random_binary_expr_test_case()

# expr = choice(list(map(str, range(10))))
# for _ in range(20):
#     expr += ' ' + random.choice(binary_operators) + ' ' + choice(list(map(str, range(10))))
# print(expr)
