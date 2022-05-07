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

import itertools
import traceback
from mytoken import Separators, Keywords, SEPARATOR_DEF_FILE, KEYWORD_DEF_FILE
import ast

INPUT_FILE = 'scripts/grammar.txt'
OUTPUT_FILE = 'scripts/grammar.txt.output'

SEPARATORS = ('(', ')', '?', '*', '=', '|') # separators for input file, not those in token-separator.txt
OTHER_TERMINALS = ['IDENT', 'LITERAL', 'LABEL', 'NUMBER'] # other not keyword/separator terminals

# a postfixable terminal or non terminal
class Symbol(object):
    def __init__(self, name, postfix):
        self.name = name
        self.postfix = postfix # Optional[Literal['opt', 'any']]
        self.refrule = None # none for terminal
        self.display = None # keyword/separator value if keyword/separator
        # avoid long isinstance
        self.is_group = False
        self.is_symbol = True
    def __str__(self):
        return f'{self.name}{"?" if self.postfix == "opt" else "*" if self.postfix == "any" else ""}'
    def __eq__(self, rhs):
        return self.name == rhs.name and self.postfix == rhs.postfix
    @property
    def is_non_terminal(self):
        return all(c.islower() for c in self.name if c not in ('_', '-'))
    @property
    def is_terminal(self):
        return all(c.isupper() or c.isnumeric() for c in self.name if c not in ('_', '-'))

# some symbol, maybe alternative or sequence
class Group(object):
    def __init__(self, group_type, items, postfix):
        self.group_type = group_type # Literal['alt', 'seq']
        self.items = items           # list[Union[Symbol, Group]]
        self.postfix = postfix       # Optional[Literal['opt', 'any']]
        # avoid long isinstance
        self.is_group = True
        self.is_symbol = False
    def __str__(self):
        if self.is_alt:
            return '(' + ' | '.join(map(str, self.items)) + ')'
        else:
            return '(' + ' '.join(map(str, self.items)) + ')' + ('?' if self.postfix == 'opt' else '*' if self.postfix == 'any' else '')
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
        # always Group, even for one symbol
        self.production = production
        # rule/node consistency check
        self.refnode = None # the Node instance in ast.py
        self.refrules = []  # all Symbol.refrule
        # parse table construction
        self.firstsubset = [] # rule name, see usage
        self.firsts = []  # terminal names, that upper case one
        self.followsubsetfirst = [] # rule name, see usage
        self.followsubsetfollow = []
        self.follows = [] # terminal names, that upper case one
        self.emptyable = False # referenced as opt or any, or exist an alternative `rule-name = epsilon`

    def __str__(self):
        production = str(self.production)[1:-1]
        return f"{self.name} = {production}"

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
        elif c.isalnum() or c == '-' or c == '_':
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
        postfix = ('opt' if self.move_next() == '?' else 'any') if self.peek(0) in ('?', '*') else None
        return Symbol(value, postfix)

    def parse_group(self):
        self.ensure(self.peek(0) == '(', f'parse_group: expect `(` at {self.i}')
        self.move_next()
        inner = self.parse_production()
        self.ensure(self.peek(0) == ')', f'parse_group: expect `)` at {self.i}')
        self.move_next()
        inner.postfix = ('opt' if self.move_next() == '?' else 'any') if self.peek(0) in ('?', '*') else None
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

def validate(rules, separators, keywords):
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
                    item.refrule.emptyable = item.refrule.emptyable or item.postfix is not None
                    non_terminals.append(item.refrule)
            else:
                keyword = next((kw for kw in keywords.items if kw.name.lower() == item.name.lower()), None)
                separator = next((sep for sep in separators.all_items if sep.name.lower() == item.name.lower()), None)
                if keyword is not None:
                    item.display = f'`{keyword.value}`'
                    if keyword.cat_value == 0:
                        log(f'{prefix} keyword {keyword.name} cannot be reserved because it is used')
                    elif keyword.cat_value == 1 and 'primitive-type' not in prefix:
                        log(f'{prefix} primitive keyword can only be used in primitive-type')
                    elif 'primitive-type' in prefix and keyword.cat_value != 1:
                        log(f'{prefix} primitive-type can only have primitive keyword')
                    elif keyword.cat_value == 2 and 'normal-segment-base' not in prefix:
                        log(f'{prefix} maybe identifier keyword can only be used in normal-segment-base')
                elif separator is not None:
                    item.display = f'`{separator.value}`'
                    all_used_separators.append(separator.name)
                elif item.name not in OTHER_TERMINALS:
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
        # but expr data structure is really different from expr grammar
        if False:
            node = next((n for n in nodes if n.display == rule.name), None)
            if node is None:
                log(f'rule {rule.name} not found in ast.txt')
            else:
                rule.refnode = node
                # print(f'rule {rule.name} found node {node.display}')

    # the actual state of grammar and ast type def (include the previous exclude expr)
    # shows that these should not be regard as strong validation error and furthur restriction is not helpful
    if False:
        for rule in (r for r in rules if r.refnode is not None):
            refrule_names = set(r.name for r in rule.refrules)
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

# simple rule without group, by splitting group into something like original_rule_name-2-3
def flatten(rules):
    frules = [] # still Group but item is always Symbol
    intermediate_count, reuse_count = 0, 0

    # prefix is current rule name
    def visit_group(group, level, parent_name):
        nonlocal intermediate_count, reuse_count
        fitems = [] # list[str]
        for index in range(0, len(group.items)):
            if group.items[index].is_group:
                group.items[index] = visit_group(group.items[index], level + 1, f'{parent_name}-{index + 1}')
        if level > 0:
            # postfix should leave in the to be replaced symbol, but not in production level group
            postfix = group.postfix
            group.postfix = None

            identical_rule = next((f for f in frules if f.production.items == group.items), None)
            # ATTENTION disable reuse here
            # identical rule does not affect the correctness of conflict check, but increase conflict analysis difficulty
            if False: # identical_rule is not None:
                reuse_count += 1
                new_symbol = Symbol(identical_rule.name, postfix)
                new_symbol.refrule = identical_rule
                identical_rule.emptyable = identical_rule.emptyable or postfix is not None
                return new_symbol
            else:
                intermediate_count += 1
                frules.append(Rule(0, parent_name, group))
                frules[-1].emptyable = postfix is not None
                new_symbol = Symbol(parent_name, postfix)
                new_symbol.refrule = frules[-1]
                return new_symbol
    for rule in rules:
        visit_group(rule.production, 0, rule.name)
        frules.append(rule)
    return frules, (intermediate_count, reuse_count)

# prepare first set and follow set
def prepare(rules, separators, keywords):

    # stack: visit stack, or visiting rule names
    def calculate_first(rule, stack):
        if len(rule.firsts):
            return
        if rule.name in stack:
            raise ValueError(f'infinite loop (left recursive) when calculate first {rule.name}: {stack}')
        stack.append(rule.name)
        if rule.production.is_seq:
            for item in rule.production.items:
                if item.is_terminal:
                    rule.firsts.append(item.display or item.name)
                else:
                    calculate_first(item.refrule, stack)
                    # append to firstsubset when appending from other rule
                    # this may help identify first source and implmentation of maybe_* functions
                    rule.firstsubset.append(item.refrule.name)
                    rule.firsts += item.refrule.firsts
                if item.postfix is None: # opt and any are epsilonable
                    break
            else:
                if rule.name != 'module':
                    print(f'all items of {rule.name} is optional, need concat follow if this really happens')
        else:
            for item in rule.production.items:
                if item.is_terminal:
                    rule.firsts.append(item.display or item.name)
                else:
                    calculate_first(item.refrule, stack)
                    rule.firstsubset.append(item.refrule.name)
                    rule.firsts += item.refrule.firsts
        stack.pop()

    # stack: indention
    def log(stack, message):
        if False:
            print(len(stack) * '  ' +  message)

    # stack will be used if all item after current item is optional, don't know what means when infinite loop
    def calculate_follow(rule, stack):
        log(stack, f'begin calculate follow {rule.name}')
        if len(rule.follows):
            log(stack, f'end with skip calculate follow {rule.name}')
            return
        if rule.name in stack:
            raise ValueError(f'infinite loop (don\'t know why) when calculate follow {rule.name}: {stack}')
        stack.append(rule.name)
        for refedrule in (r for r in rules if rule.name in (s.name for s in r.production.items)):
            if refedrule.production.is_seq:
            # nothing prevents from multiple reference, although I don't think that will happen
                for index in range(0, len(refedrule.production.items)):
                    if refedrule.production.items[index].name == rule.name:
                        # direct last item
                        if index + 1 == len(refedrule.production.items):
                            log(stack, f'direct last, forward to {refedrule.name}')
                            calculate_follow(refedrule, stack)
                            rule.followsubsetfollow.append(refedrule.name)
                            rule.follows += refedrule.follows
                        else:
                            for index in range(index + 1, len(refedrule.production.items)):
                                item = refedrule.production.items[index]
                                if item.is_terminal:
                                    log(stack, f'normal terminal, append {item.name}')
                                    rule.follows.append(item.display or item.name)
                                else:
                                    log(stack, f'normal non terminal, append first from {item.refrule.name}')
                                    rule.followsubsetfirst.append(item.refrule.name)
                                    rule.follows += item.refrule.firsts
                                if item.postfix is None:
                                    break
                            else:
                                log(stack, f'strike through, forward to {refedrule.name}')
                                calculate_follow(refedrule, stack)
                                rule.followsubsetfollow.append(refedrule.name)
                                rule.follows += refedrule.follows

            else:
                calculate_follow(refedrule, stack)
                rule.followsubsetfollow.append(refedrule.name)
                rule.follows += refedrule.follows
        stack.pop()
        log(stack, f'end calculate follow {rule.name}')

    rules[0].follows.append('<dollar>')
    for rule in rules:
        calculate_first(rule, [])
    # follow may append from first, need to calculate first first
    for rule in rules:
        calculate_follow(rule, [])

    # finalize
    primitive_types = [s.display for s in next(r for r in rules if r.name == 'primitive-type').production.items]
    other_assign_operators = [s.display for s in next(r for r in rules if r.name == 'assign-expr-stmt-2').production.items if s.name != 'EQ']
    for rule in rules:
        # sort dedup
        rule.firstsubset = list(sorted(set(rule.firstsubset)))
        rule.followsubsetfirst = list(sorted(set(rule.followsubsetfirst)))
        rule.followsubsetfollow = list(sorted(set(rule.followsubsetfollow)))
        rule.firsts = list(sorted(set(rule.firsts)))
        rule.follows = list(sorted(set(rule.follows)))

        # pack: primitive type list and other assign op list is boring to read
        if all(s in rule.firsts for s in primitive_types):
            rule.firsts = [s for s in rule.firsts if s not in primitive_types]
            rule.firsts.insert(0, 'primitive-type')
        if all(s in rule.firsts for s in other_assign_operators):
            rule.firsts = [s for s in rule.firsts if s not in other_assign_operators]
            rule.firsts.insert(0, 'other-assign-expr')

        if all(s in rule.follows for s in primitive_types):
            rule.follows = [s for s in rule.follows if s not in primitive_types]
            rule.follows.insert(0, 'primitive-type')
        if all(s in rule.follows for s in other_assign_operators):
            rule.follows = [s for s in rule.follows if s not in other_assign_operators]
            rule.follows.insert(0, 'other-assign-expr')

# # expected to actual construct table, but this actually checks conflicts and return
def construct(rules):
    # first first conflict: for one cell (for one step in parsing), there are 2 or more alternatives to choose, exclude epsilon one
    for rule in (r for r in rules if r.production.is_alt):
        for alt1, alt2 in itertools.combinations((i.refrule for i in rule.production.items if i.refrule is not None), 2):
            intersection = set(alt1.firsts) & set(alt2.firsts)
            if len(intersection):
                print(f'[I/I] `{rule.name} = {alt1.name}` and `{rule.name} = {alt2.name}`: {", ".join(list(sorted(intersection)))}')

    # first follow conflict: if rule produce epsilon (or optional/any, or Rule.emptyable),
    # if there is intersection between its own first set and follow set,
    # when meet terminal in the interesction, can not decide whether this rule or follow of the rule
    for rule in (r for r in rules if r.emptyable):
        intersection = set(rule.firsts) & set(rule.follows)
        if len(intersection):
            print(f'[I/O] {rule.name}: {", ".join(list(sorted(intersection)))}')

if __name__ == '__main__':
    keywords = Keywords(KEYWORD_DEF_FILE)
    separators = Separators(SEPARATOR_DEF_FILE)

    rules, has_error = load(INPUT_FILE)
    has_validate_error = validate(rules, separators, keywords)
    if False:
        for rule in rules:
            print(rule)
    else:
        print(f'{len(rules)} rules')
    if has_error or has_validate_error:
        exit(1)

    rules, counts = flatten(rules)
    prepare(rules, separators, keywords)
    table = construct(rules)

    print(f'{len(rules)} flatten rules ({counts[0]} intermediate {counts[1]} reuse)')
    with open(OUTPUT_FILE, 'w') as file:
        for rule in rules:
            print(file=file)
            print(rule, file=file)
            print(f'   FIRST: {", ".join((["<epsilon>"] + rule.firsts) if rule.emptyable else rule.firsts)}', file=file)
            if rule.production.is_seq and len(rule.firstsubset):
                print(f'   FIRSTSUBSET: {", ".join(rule.firstsubset)}', file=file)
            print(f'   FOLLOW: {", ".join(rule.follows)}', file=file)
            if len(rule.followsubsetfirst):
                print(f'   FOLLOWSUBSETFIRST: {", ".join(rule.followsubsetfirst)}', file=file)
            if len(rule.followsubsetfollow):
                print(f'   FOLLOWSUBSETFOLLOW: {", ".join(rule.followsubsetfollow)}', file=file)
