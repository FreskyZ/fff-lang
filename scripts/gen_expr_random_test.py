import random
from random import choice
import os
from functools import reduce

def list_to_str(chars):
    return reduce(lambda x, y: x + y, chars, '')

def get_random_bool():
    return choice([True, False])

oct_chars = list(map(str, range(48, 55)))
int_chars = list(map(chr, range(48, 57)))
hex_chars = int_chars + int_chars + ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F'] # double int chars to make every digit same possibility
def get_random_number():
    ''' 
    parameters in the impl is judged to make the result more 'natural'
    may generate overflow literal and
    literal starts with 0 but not an integral prefix
    treat them as random normal error
    '''
    # 0b [0|1]+ [\u\i]
    # 0o [0-7]+ [\u\i]
    # 0x [0-9A-Fa-f]+ [\u\i]
    # 0d [0-9]+ [\u\i]
    # [0-9]+ [\u\i\f]
    # [0-9]+ [\. [0-9]+] [(E|e) [0-9]+] [\f]
    int_postfixes = ['i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64']
    real_postfixes = ['f32', 'f64']
    
    return {
        1: lambda: '0b' + list_to_str([choice(['0', '1']) for _ in range(choice(range(1, 20)))]) 
            + (choice(int_postfixes) if choice(range(0, 100)) > 60 else ''),
        2: lambda: '0o' + list_to_str([choice(oct_chars) for _ in range(choice(range(1, 15)))]) 
            + (choice(int_postfixes) if choice(range(0, 100)) > 60 else ''),
        3: lambda: '0x' + list_to_str([choice(hex_chars) for _ in range(choice(range(1, 8)))]) 
            + (choice(int_postfixes) if choice(range(0, 100)) > 60 else ''),
        4: lambda: '0d' + list_to_str([choice(int_chars) for _ in range(choice(range(1, 7)))]) 
            + (choice(int_postfixes) if choice(range(0, 100)) > 60 else ''),
        5: lambda: list_to_str([choice(int_chars) for _ in range(choice(range(1, 8)))]) 
            + (choice(int_postfixes + real_postfixes) if choice(range(0, 100)) > 75 else ''),
        6: lambda: list_to_str([choice(int_chars) for _ in range(choice(range(1, 8)))]) 
            + (('.' + list_to_str([choice(int_chars) for _ in range(choice(range(1, 8)))])) if choice(range(0, 100)) > 40 else '')
            + (('E' 
                + { 1: '+', 2: '-', 3: ''}[choice([1, 2, 3])]
                + list_to_str([choice(int_chars) for _ in range(choice(range(1, 3)))])) if choice(range(0, 100)) > 60 else '')
            + (choice(real_postfixes) if choice(range(0, 100)) > 75 else '')
    }[choice([1, 1, 2, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6])]()

binary_operators = ['+', '*', '/', '%', '-', '&', '|', '^', '<', '>', '==', '<=', '>=', '<<', '>>', '&&', '||']
def get_random_binary_operator():
    return choice(binary_operators)

unary_operators = ['!', '-', '~']
def get_random_unary_operator():
    return choice(unary_operators)

primitive_types = ['u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i33', 'i64', 'f32', 'f64', 'char', 'bool', 'string']
keywords = ['fn', 'if', 'else', 'while', 'break', 'continue', 'for', 'return', 'var', 'const', 'as', 'loop', 'this', 'true', 'false'] + primitive_types
lower_chars = list(map(chr, range(97, 122)))
upper_chars = list(map(chr, range(65, 80)))
def get_random_identifier():
    def generate_ident_char():
        yield choice(lower_chars + upper_chars)
        while True:
            yield choice(lower_chars + upper_chars + int_chars)
    generator = generate_ident_char()
    return list_to_str([next(generator) for _ in range(choice(range(1, 9)))])

def get_random_string():
    return list_to_str([choice(list(map(chr, range(32, 126))) + ['\\t', '\\n', '\\r', '\\\\']) for _ in range(choice(range(1, 9)))])

def get_random_literal():
    return {
        1: lambda: '"%s"' % get_random_string(),
        2: lambda: "'%s'" % get_random_string()[0],
        3: lambda: get_random_number(),
        4: lambda: 'true' if get_random_bool() else 'false',
        5: lambda: '()',
    }[choice([1, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5])]()

def _get_random_quoted(f, ch1, ch2, choocer): # item generator, quote char, list of 0-5 to specify relative possibility
    return {
        0: lambda: ch1 + ch2,
        1: lambda: ch1 + list_to_str([f() for _ in range(2)]) + ',' + ch2,
        2: lambda: ch1 + list_to_str([f() + ', ' for _ in range(3)])[:-2] + ch2,
        3: lambda: ch1 + list_to_str([f() + ', ' for _ in range(4)])[:-2] + ch2,
        4: lambda: ch1 + list_to_str([f() + ', ' for _ in range(5)])[:-2] + ch2,
    }[choice(choocer)]()

def get_random_typeuse():
    return {
        1: lambda: choice(primitive_types) if get_random_bool() else get_random_identifier(),
        2: lambda: '[' + (get_random_typeuse() if choice(range(0, 100)) > 50 else get_random_identifier()) + ']',
        3: lambda: _get_random_quoted(get_random_typeuse, '(', ')', [0, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4]),
        4: lambda: '[' + get_random_identifier() + ';' + get_random_number() + ']'
    }[choice([1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4])]()

def gen_random_primary_expr_test_case():
    return {
        1: get_random_identifier,
        2: get_random_literal,
        3: lambda: '(' + gen_random_primary_expr_test_case() + ')',
        4: lambda: _get_random_quoted(gen_random_primary_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        5: lambda: _get_random_quoted(gen_random_primary_expr_test_case, '[', ']', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        6: lambda: '[' + gen_random_primary_expr_test_case() + ';' + gen_random_primary_expr_test_case() + ']',
    }[choice([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6])]()

def gen_random_postfix_expr_test_case():
    return {
        1: get_random_number,
        2: get_random_identifier,
        3: lambda: gen_random_postfix_expr_test_case() + '.' + get_random_identifier(),
        4: lambda: gen_random_postfix_expr_test_case() + _get_random_quoted(gen_random_postfix_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        5: lambda: gen_random_postfix_expr_test_case() + _get_random_quoted(gen_random_postfix_expr_test_case, '[', ']', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
        6: lambda: gen_random_postfix_expr_test_case() + '.' + get_random_identifier() + _get_random_quoted(gen_random_postfix_expr_test_case, '(', ')', [0, 1, 2, 2, 2, 2, 2, 3, 3, 4]),
    }[choice([1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6])]()

def get_random_expr():
    pass

def gen_random_binary_expr_test_case():
    pass

def gen_random_expr_test_case():
    pass

# print([get_random_number() for _ in range(0, 9)])
# print([get_random_binary_operator() for _ in range(0, 9)])
# print([get_random_unary_operator() for _ in range(0, 9)])
# print([get_random_identifier() for _ in range(0, 9)])
# print([get_random_string() for _ in range(0, 9)])
# print([get_random_typeuse() for _ in range(0, 9)])
# print([get_random_literal() for _ in range(0, 9)])

# print()
# print(reduce(lambda x, y: x + y + '\n', [gen_random_primary_expr_test_case() for _ in range(0, 10)], ''))
# print(reduce(lambda x, y: x + y + '\n', [gen_random_postfix_expr_test_case() for _ in range(0, 10)], ''))

#expr = gen_random_postfix_expr_test_case()
expr = gen_random_primary_expr_test_case()
print(expr)
os.system('echo "%s" | clip' % expr)
