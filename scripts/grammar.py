#!python3

# patch token.py
# I literal really completely don't understand why my module is preferred when import statement is in stdlib's code
import sys
__import__('token')
mytoken = sys.modules.pop('token')
sys.modules['mytoken'] = mytoken
import importlib.util
import_spec = importlib.util.spec_from_file_location('token', '/usr/lib/python3.8/token.py')
std_token = importlib.util.module_from_spec(import_spec)
import_spec.loader.exec_module(std_token)
sys.modules['token'] = std_token

import traceback
from mytoken import Separators, Keywords, SEPARATOR_DEF_FILE, KEYWORD_DEF_FILE
import ast

INPUT_FILE = 'scripts/grammar.txt'

# a postfixable terminal or non terminal
class Symbol(object):
    def __init__(self, name, postfix):
        self.name = name
        self.postfix = postfix # Optional[Literal['opt', 'many']]
        self.refrule = None # none for terminal
        # avoid long isinstance
        self.is_group = False
        self.is_symbol = True
    def __str__(self):
        return f'{self.name}{"?" if self.postfix == "opt" else "*" if self.postfix == "many" else ""}'
    @property
    def is_terminal(self):
        return all(c.isupper() or c.isnumeric() for c in self.name if c != '_')
    @property
    def is_non_terminal(self):
        return all(c.islower() for c in self.name if c != '_')

# some symbol, maybe alternative or sequence
class Group(object):
    def __init__(self, group_type, items, postfix):
        self.group_type = group_type # Literal['alt', 'seq']
        self.items = items           # list[Union[Symbol, Group]]
        self.postfix = postfix       # Optional[Literal['opt', 'many']]
        # avoid long isinstance
        self.is_group = True
        self.is_symbol = False
    def __str__(self):
        if self.is_alt:
            return '(' + ' | '.join(map(str, self.items)) + ')'
        else:
            return '(' + ' '.join(map(str, self.items)) + ')' + ('?' if self.postfix == 'opt' else '*' if self.postfix == 'many' else '')
    @property
    def is_alt(self):
        return self.group_type == 'alt'
    @property
    def is_seq(self):
        return self.group_type == 'seq'

class Rule(object):
    def __init__(self, line, name, production):
        self.line = line
        self.name = name
        self.production = production # always Group, even for one symbol
        self.refnode = None # the Node instance in ast.py
        self.refrules = []  # all Symbol.refrule
    def __str__(self):
        production = str(self.production)[1:-1]
        return f"{self.name} = {production}"
    @property
    def node_display(self):
        return self.name.replace('_', '-')

SEPARATORS = ('(', ')', '?', '*', '=', '|') # separators for input file, not those in token-separator.txt

# tokenize one line from input file
def tokenize(row):
    tokens = []
    current = None # current identifier
    for c in row:
        if c in SEPARATORS:
            if current:
                tokens.append(current)
                current = None
            tokens.append(c)
        elif c.isalnum() or c == '_':
            if current:
                current += c
            else:
                current = c
        elif c.isspace():
            if current:
                tokens.append(current)
                current = None
    if current:
        tokens.append(current)
    return tokens

# parse one rule
# # you amazingly need this formal parser to parse grammar definition
class Parser(object):
    def __init__(self, line, raw):
        self.line = line
        self.raw = raw
        self.tokens = tokenize(raw)
        # print(self.tokens)
        self.i = 0 # current index in tokens

    # peek(0) is current, it's shorter than `self.current`
    def peek(self, n):
        return self.tokens[self.i + n] if self.i + n < len(self.tokens) else None
    def move_next(self):
        self.ensure(self.i < len(self.tokens), f'move_next should not be called when EOL, i = {self.i}')
        self.i += 1
        return self.tokens[self.i - 1]

    def abort(self, message = 'invalid rule'):
        print(f'error line {self.line} {message}:\n   raw: {self.raw}\n   tokens: {self.tokens}')
        raise ValueError
    def ensure(self, condition, message = 'invalid rule'):
        if not condition:
            self.abort(message)

    def parse_rule(self):
        self.ensure(self.peek(0) not in SEPARATORS, 'expect identifier')
        name = self.move_next()
        self.ensure(self.peek(0) == '=', 'expect `=`')
        self.move_next()
        production = self.parse_production()
        return Rule(self.line, name, production)

    def parse_alternative(self):
        items = []
        while True:
            self.ensure(self.peek(0) not in SEPARATORS, f'expect identifier at {self.i}')
            items.append(self.move_next())
            if self.peek(0) is None or self.peek(0) == ')':
                break
            self.ensure(self.peek(0) == '|', f'expect `|` at {self.i}')
            self.move_next()
        return Group('alt', [Symbol(i, None) for i in items], None)

    def parse_basic(self):
        self.ensure(self.peek(0) not in SEPARATORS, f'parse_basic: expect identifier at {self.i}')
        value = self.move_next()
        postfix = ('opt' if self.move_next() == '?' else 'many') if self.peek(0) in ('?', '*') else None
        return Symbol(value, postfix)

    def parse_group(self):
        self.ensure(self.peek(0) == '(', f'parse_group: expect `(` at {self.i}')
        self.move_next()
        inner = self.parse_production()
        self.ensure(self.peek(0) == ')', f'parse_group: expect `)` at {self.i}')
        self.move_next()
        inner.postfix = ('opt' if self.move_next() == '?' else 'many') if self.peek(0) in ('?', '*') else None
        return inner

    def parse_component(self):
        return self.parse_basic() if self.peek(0) != '(' else self.parse_group()

    def parse_sequence(self):
        items = []
        while self.peek(0) is not None and self.peek(0) != ')':
            items.append(self.parse_component())
        return Group('seq', items, None)

    def parse_production(self):
        return self.parse_alternative() if self.peek(0) != '(' and self.peek(1) == '|' else self.parse_sequence()

def load(input_file):
    raws = []
    for line, raw in enumerate(map(str.strip, open(input_file).readlines())):
        if raw.startswith('|'):
            raws[-1][1] += raw
        elif raw and not raw.startswith('#'):
            raws.append([line + 1, raw])

    rules = []
    has_error = False # if one rule is invalid, continue
    for line, raw in raws:
        try:
            rules.append(Parser(line, raw).parse_rule())
        except ValueError:
            has_error = True
            print(traceback.format_exc())
            continue
    return rules, has_error

def validate(rules):
    has_error = False
    def log(message):
        print(message)
        has_error = True

    # allow by data structure not allow by self grammar / parser structure but may cause by my error:
    # - production as group has postfix
    # - one item group and is not production
    # - alternative only one item
    # - alternative item not symbol
    # - alternative item has postfix
    # allow by self grammar but incorrect
    # - not postfixed and not alternative group
    # internal invalid reference
    # - unknown non terminal name
    # - not used non terminal, except entry
    # external inconsistency
    # - separator and keyword should be defined separator and keyword should be defined in 
    #   token-keyword.txt and token-separator.txt and name is simple upper case, keyword cannot be reserved
    # - for non terminal, except binary expr, in ast.txt should exist same display name replace hyphen with underscore,
    #   struct should have same non terminal reference (index/slice), and repeat is consistent (slice), enum should have same variants

    keywords = Keywords(KEYWORD_DEF_FILE)
    separators = Separators(SEPARATOR_DEF_FILE)
    KNOWN_OTHER_TERMINALS = ['IDENT', 'LITERAL', 'LABEL', 'NUMBER']

    all_used_keywords = [] # Keyword.name
    all_used_separators = [] # Separator.name

    # prefix: message prefix 'line {} rule {}'
    # non_terminals: referenced non terminals
    def visit_group(prefix, group, level, non_terminals):
        # group postfix
        if level == 0 and group.postfix is not None:
            log(f'{prefix} whole production cannot have postfix')
        # item length
        if len(group.items) == 0:
            log(f'{prefix} no item group')
        elif level != 0 and len(group.items) == 1:
            log(f'{prefix} single item group')
        elif group.is_alt and len(group.items) == 1:
            log(f'{prefix} single item alternative')
        # alternative
        if group.is_alt and any(i for i in group.items if not i.is_symbol):
            log(f'{prefix} non symbol item in alternative')
        elif group.is_alt and any(i for i in group.items if i.postfix is not None):
            log(f'{prefix} symbol item has postfix')
        # not needed group
        if group.is_seq and level != 0 and group.postfix is None:
            log(f'{prefix} not needed no postfix group')
        # internal unknown reference
        for item in (i for i in group.items if i.is_symbol):
            # but first a symbol name need to be either complete lowercase or complete upper case
            if not item.is_terminal and not item.is_non_terminal:
                log(f'{prefix} neither complete lower nor complete upper symbol name {item.name}')
            if item.is_non_terminal:
                item.refrule = next((r for r in rules if r.name == item.name), None)
                if item.refrule is None:
                    log(f'{prefix} unknown non terminal {item.name}')
                else:
                    non_terminals.append(item.refrule)
            else:
                keyword = next((kw for kw in keywords.items if kw.name.lower() == item.name.lower()), None)
                separator = next((sep for sep in separators.all_items if sep.name.lower() == item.name.lower()), None)
                if keyword is not None:
                    if keyword.cat_value == 0:
                        log(f'{prefix} keyword {keyword.name} cannot be reserved because it is used')
                    elif keyword.cat_value == 1 and 'primitive_type' not in prefix:
                        log(f'{prefix} primitive keyword can only be used in primitive_type')
                    elif 'primitive_type' in prefix and keyword.cat_value != 1:
                        log(f'{prefix} primitive_type can only have primitive keyword')
                    elif keyword.cat_value == 2 and 'simple_segment' not in prefix:
                        log(f'{prefix} maybe identifier keyword can only be used in simple_segment')
                elif separator is not None:
                    all_used_separators.append(separator.name)
                elif item.name not in KNOWN_OTHER_TERMINALS:
                    log(f'{prefix} unknown terminal {item.name}')

        for item in (i for i in group.items if i.is_group):
            visit_group(prefix, item, level + 1, non_terminals)
    
    nodes, has_ast_error, _node_counts = ast.load(ast.DEF_FILE)
    has_error = has_error or has_ast_error

    all_refrules = []
    for rule in rules:
        non_terminals = []
        visit_group(f'line {rule.line} rule {rule.name}:', rule.production, 0, non_terminals)
        rule.refrules = non_terminals
        all_refrules += non_terminals

        # check with ast type def
        # but expr data structure is really different from expr grammar, regard expr as some black box in following check
        if not rule.name.endswith('expr') and rule.name != 'label':
            node = next((n for n in nodes if n.display == rule.node_display), None)
            if node is None:
                log(f'rule {rule.name} not found in ast.txt')
            else:
                rule.refnode = node
                # print(f'rule {rule.name} found node {node.display}')

    # the actual state of grammar and ast type def (include the previous exclude expr)
    # shows that these should not be regard as strong validation error and furthur restriction is not helpful
    if False:
        for rule in (r for r in rules if r.refnode is not None):
            refrule_names = set(r.node_display for r in rule.refrules)
            refnode_names = set(f.refnode.display for f in rule.refnode.fields if f.refnode is not None)
            for refrule_name in refrule_names:
                if refrule_name not in refnode_names and refrule_name not in ('range_expr', 'label'):
                    log(f'rule {rule.name} refrule {refrule_name} not found in node {rule.refnode.display}')
            for refnode_name in refnode_names:
                if refnode_name not in refrule_names:
                    log(f'node {rule.refnode.display} refnode {refnode_name} not found in rule {rule.name}')

    for rule in rules:
        if rule.name != rules[0].name and rule not in all_refrules:
            log(f'line {rule.line} rule {rule.name} is not used by any other')
    for separator in separators.all_items:
        if separator.name not in all_used_separators:
            log(f'separator {separator.name} ({separator.value}) not used')

    return has_error

if __name__ == '__main__':
    rules, has_error = load(INPUT_FILE)
    has_validate_error = validate(rules)
    if False:
        for rule in rules:
            print(rule)
    else:
        print(f'{len(rules)} rules')
    if has_error or has_validate_error:
        exit(1)

    