#!python3

# generate ast related source code according to ast actual definitions in ast.txt
#
# ast structure is rapidly changing currently and may change much more in future with development of this language,
# it's slow and boring to change all the definition, impl node, impl visitor etc. with actually simple mechanical code
#
# the recommend method for rust is using derive macros, e.g. #[derive(Node)] , BUT that's not my principle, because
#   - you cannot see generated code easily/directly (that's unstable or external dependency)
#   - macro implementation require external dependency (although they are official)
#   - I need some level of semantic information of the ast structure, e.g. checking no unknown "terminal" fields,
#     but macro is syntax level (I mean, rustc syntax level), 
#     that may can be achieved by comparing token's string representaion, but that must be unconvenient
# so, my solution is similar to keyword/separator/unicode, writing dsl in text and using manual code generation in python

from collections import namedtuple
from itertools import takewhile
import re

DEF_FILE = 'scripts/ast.txt'
ENTRY_FILE = 'src/syntaxw/ast.rs'
SPAN_FILE = 'src/syntaxw/ast/span.rs'
NEW_FILE = 'src/syntaxw/ast/new.rs'
CMP_FILE = 'src/syntaxw/ast/cmp.rs'
UGLY_FILE = 'src/syntaxw/ast/ugly.rs'
VISIT_TRAIT_FILE = 'src/syntaxw/visit.rs'
VISIT_FILE = 'src/syntaxw/ast/visit.rs'

TERMINAL_TYPES = ['Span', 'IsId', 'IdSpan', 'Keyword', 'Separator', 'Numeric', 'LitValue', 'FileId', 'bool']
LIFETIME = "<'a>"

# special: 'vec' | 'option'
# name: TERMINAL_TYPES | other node name
# TODO merge field type into field
class FieldType(namedtuple('FieldType', 'base special is_terminal')):
    def display(self, nodes):
        b = ''
        b += 'Slice<\'a, ' if self.special == 'vec' else 'Option<' if self.special == 'option' else ''
        if self.is_terminal:
            b += self.base
        else:
            refnode = next(n for n in nodes if n.name == self.base)
            if self.special != 'vec':
                if isinstance(refnode, Enum):
                    b += f'Index<\'a, '
                else:
                    b += f'Index<\'a, '
            b += self.base
            if refnode.need_lifetime():
                b += LIFETIME
            if self.special != 'vec':
                b += '>'
        b += '>' if self.special else ''
        return b

# line: line in def file
# type_: FieldType
Field = namedtuple('Field', 'line name type_')
# line: line in def file
# type_: node name
# TODO: change type_ to Struct instance
Variant = namedtuple('Variant', 'line name type_ display')

# TODO: try merge struct and enum into (node_type, ..., items)
class Enum(namedtuple('Enum', 'line name display variants')):
    def is_all_terminal(self):
        return False # any variant is true lead this to true, they are directly all true for now
    def need_lifetime(self):
        return not self.is_all_terminal()
    def maybe_lifetime(self):
        return LIFETIME if self.need_lifetime() else ''
    def name_and_lifetime(self):
        return f'{self.name}{LIFETIME}'
    def visit_name(self):
        return 'visit_' + '_'.join(self.display.split('-'))

class Struct(namedtuple('Struct', 'line name display fields')):
    def is_all_terminal(self):
        return sum([int(f.type_.is_terminal) for f in self.fields]) == len(self.fields)
    def need_lifetime(self):
        return not self.is_all_terminal()
    def maybe_lifetime(self):
        return LIFETIME if self.need_lifetime() else ''
    def name_and_lifetime(self):
        return self.name + ('' if self.is_all_terminal() else LIFETIME)
    def visit_name(self):
        return 'visit_' + '_'.join(self.display.split('-'))

RE_ENUM = re.compile(r'enum (?P<name>\w+) "(?P<display>[\w-]+)"')
RE_STRUCT = re.compile(r'struct (?P<name>\w+) "(?P<display>[\w-]+)"')
RE_FIELD = re.compile(r'(?P<name>[\w#]+): (?P<rawtype>[\w<>]+)')
RE_VARIANT = re.compile(r'(?P<name>\w+)\((?P<type>\w+)\) "(?P<display>[\w-]+)"')
RE_OPTION = re.compile(r'Option<(?P<base>\w+)>')
RE_VEC = re.compile(r'Vec<(?P<base>\w+)>')

def match_any(expressions, input):
    for index, expr in enumerate(expressions):
        match = expr.match(input)
        if match:
            return index, match
    return -1, None

def readlines(filename):
    with open(filename) as f:
        return f.readlines()

def parse_def(def_file):
    results, has_error = [], False
    current = None
    for line, row in enumerate(map(str.strip, open(def_file).readlines())):
        if not len(row) or row[0:2] == '//':
            continue
        line = line + 1
        match_index, match = match_any([RE_STRUCT, RE_ENUM, RE_FIELD, RE_VARIANT], row)
        if match_index in (0, 1):
            if current is not None:
                results.append(current)
            current = (Struct if match_index == 0 else Enum)(line, match.group('name'), match.group('display'), [])
        elif match_index == 2:
            rawtype = match.group('rawtype')
            type_match_index, type_match = match_any([RE_OPTION, RE_VEC], rawtype)
            if type_match_index == 0:
                field_type = FieldType(type_match.group('base'), 'option', type_match.group('base') in TERMINAL_TYPES)
            elif type_match_index == 1:
                field_type = FieldType(type_match.group('base'), 'vec', type_match.group('base') in TERMINAL_TYPES)
            else:
                field_type = FieldType(rawtype, None, rawtype in TERMINAL_TYPES)
            current.fields.append(Field(line, match.group('name'), field_type))
        elif match_index == 3:
            current.variants.append(Variant(line, match.group('name'), match.group('type'), match.group('display')))
        elif row == 'Global "global",':
            current.variants.append(Variant(line, 'Global', None, 'global'))
        else:
            has_error = True
            print(f'line {line}: unknown format: {row}')
    results.append(current)
    return list(sorted(results, key=lambda n: n.name)), has_error

def validate_def(nodes):
    # check duplicate node display
    # check struct field type is terminal, node name, or special of terminal or node name
    # check enum variant type is node
    # duplicate struct/enum name, duplicate field/variant name will be rust compile error, but duplicate variant base type is checked
    has_error = False
    struct_count, field_count, enum_count, variant_count = 0, 0, 0, 0
    duplicate_displays = [d for d, c in [(d, len([1 for n in nodes if n.display == d])) for d in set([n.display for n in nodes])] if c > 1]
    if len(duplicate_displays):
        print(f'duplicate node display {duplicate_displays}')
    for node in nodes:
        if isinstance(node, Struct):
            struct_count += 1
            if not len(node.fields):
                has_error = True
                print(f'line {node.line} no field')
            for field in node.fields:
                field_count += 1
                if not field.type_.is_terminal and field.type_.base not in (node.name for node in nodes):
                    has_error = True
                    print(f'line {field.line} field type {field.type_.base} unknown')
        else:
            enum_count += 1
            if not len(node.variants):
                has_error = True
                print(f'line {node.line} no variant')
            duplicate_base_types = [t for t, c in [(t, len([1 for v in node.variants if v.type_ == t])) for t in set([v.type_ for v in node.variants])] if c > 1]
            if len(duplicate_base_types):
                has_error = True
                print(f'line {node.line} enum {node.name} has duplicate variant base type {duplicate_base_types}')
            duplicate_displays = [d for d, c in [(d, len([1 for v in node.variants if v.display == d])) for d in set([v.display for v in node.variants])] if c > 1]
            if len(duplicate_displays):
                has_error = True
                print(f'line {node.line} enum {node.name} has duplicate display {duplicate_displays}')
            for variant in node.variants:
                variant_count += 1
                if variant.type_ is not None and variant.type_ not in (node.name for node in nodes):
                    has_error = True
                    print(f'line {variant.line} variant type {variant.type_} unknown')

    return struct_count, field_count, enum_count, variant_count, has_error

def dump_def(nodes, counts, detail):
    if detail:
        for node in nodes:
            if isinstance(node, Struct):
                print(f'struct {node.name} "{node.display}" {node.visit_name()}{" (all terminal)" if node.is_all_terminal() else ""}')
                for field in node.fields:
                    print(f'    field {field.name}: {field.type_.base} {field.type_.special}')
            else:
                print(f'enum {node.name} "{node.display}" {node.visit_name()}')
                for variant in node.variants:
                    print(f'    variant {variant.name} ({variant.type_}) "{variant.display}"')
    print(f'{len(nodes)} nodes ({counts[0]} structs {counts[1]} fields, {counts[2]} enums {counts[3]} variants)')

def generate_header():
    b = ''
    b += '///! --------------------------------------------------------------------------------\n'
    b += '///! This code is auto generated by a tool $repo/scripts/ast.py\n'
    b += '///! Changes may cause incorrect behavior and will be lost if the code is regenerated\n'
    b += '///! --------------------------------------------------------------------------------\n'
    b += '\n'
    return b

# TODO: try inline enum tag into index, because all enum does not have more than 16 items, or likely will not be more than 32 items in the future
# so use first 4 bit or 5 bit to represent enum tag is enough
def generate_entry(nodes, nalines):
    b = ''.join(takewhile(lambda r: r.strip() != "// AUTOGEN", nalines))
    default_header = generate_header()
    if b[:len(default_header)] == default_header:
        b = b[len(default_header):]

    b += '// AUTOGEN'
    for node in nodes:
        b += '\n'
        if isinstance(node, Struct):
            b += f'pub struct {node.name_and_lifetime()} {{\n'
            for field in node.fields:
                b += f'    pub {field.name}: {field.type_.display(nodes)},\n'
            b += '}\n'
        else:
            b += f'pub enum {node.name}<\'a> {{\n'
            for variant in node.variants:
                if variant.type_ is None:
                    b += '    Global,\n'
                else:
                    refnode = next(n for n in nodes if n.name == variant.type_)
                    b += f'    {variant.name}(Index<\'a, {variant.type_}{refnode.maybe_lifetime()}>),\n'
            b += '}\n'
    return b

def generate_span(nodes):
    b = ''
    b += '///! span() method for enums\n'
    b += '\n'
    b += 'use super::*;\n'

    for node in nodes:
        if isinstance(node, Enum):
            b += '\n'
            b += f'impl{node.maybe_lifetime()} {node.name_and_lifetime()} {{\n'
            b += '    pub fn span(&self, arena: &Arena) -> Span {\n'
            b += '        match self {\n'
            for variant in node.variants:
                if variant.type_ is None:
                    b += f'            {node.name}::{variant.name} => Span::new(0, 0),\n'
                else:
                    refnode = next(n for n in nodes if n.name == variant.type_)
                    b += f'            {node.name}::{variant.name}(n) => arena.get(n).span,\n'
            b += '        }\n'
            b += '    }\n'
            b += '}\n'
    return b

# replaced by new_in! macro, but keep for similar things to copy
def generate_new(nodes):
    b = ''
    b += '// new_in(arena) constructors\n'
    b += '\n'
    b += 'use super::*;\n'

    for node in nodes:
        b += '\n'
        b += f'impl{node.maybe_lifetime()} {node.name_and_lifetime()} {{\n'
        if isinstance(node, Struct):
            b += '    #[inline(always)]\n' # if this is not forceinline, then the |&mut Node| part is meaningless
        else:
            b += '    #[inline]\n'
        b += '    pub fn new_in'
        if not node.need_lifetime():
            b += LIFETIME
        if isinstance(node, Struct):
            b += '(\n'
            b += '        arena: &\'a Arena,\n'
            for field in node.fields:
                b += f'        {field.name}: {field.type_.display(nodes)},\n'
            b += '    ) -> Index<\'a, Self> {\n'
            b += f'        arena.emplace(|n: &mut Self| {{\n'
            for field in node.fields:
                b += f'            n.{field.name} = {field.name};\n'
            b += '        })\n'
        else:
            b += f'(arena: &\'a Arena, value: {node.name_and_lifetime()}) -> Index<\'a, Self> {{\n'
            b += f'        arena.emplace(|n: &mut Self| {{ *n = value; }})\n'
        b += '    }\n'
        b += '}\n'

    return b

def generate_cmp(nodes, nalines):
    b = ''.join(takewhile(lambda r: r.strip() != "// AUTOGEN", nalines))
    default_header = generate_header()
    if b[:len(default_header)] == default_header:
        b = b[len(default_header):]

    b += '// AUTOGEN'
    for node in nodes:
        b += '\n'
        b += f'impl{node.maybe_lifetime()} Eq for {node.name_and_lifetime()} {{\n'
        b += '    fn eq(&self, rhs: &Self, '
        b += '_' if node.is_all_terminal() else 'arena'
        b += ': &Arena) -> bool {\n'
        if isinstance(node, Struct):
            for (index, field) in enumerate([f for f in node.fields if f.type_.is_terminal]):
                b += '        '
                if index:
                    b += '&& '
                b += f'self.{field.name} == rhs.{field.name}\n'
            for field in [f for f in node.fields if not f.type_.is_terminal]:
                b += '        '
                if field.type_.special == 'option':
                    b += f'&& seq!(option, self.{field.name}, rhs.{field.name}, arena)\n'
                elif field.type_.special == 'vec':
                    b += f'&& seq!(slice, self.{field.name}, rhs.{field.name}, arena)\n'
                else:
                    b += f'&& seq!(index, self.{field.name}, rhs.{field.name}, arena)\n'
        else:
            b += '        match (self, rhs) {\n'
            for variant in node.variants:
                if variant.type_ is None:
                    b += f'            ({node.name}::{variant.name}, {node.name}::{variant.name}) => true,\n'
                else:
                    b += f'            ({node.name}::{variant.name}(lhs), {node.name}::{variant.name}(rhs)) => seq!(variant, lhs, rhs, arena),\n'
            b += '            _ => false,\n'
            b += '        }\n'
        b += '    }\n'
        b += '}\n'

    return b

def generate_visit_trait(nodes, nalines):
    b = ''.join(takewhile(lambda r: r.strip() != "// AUTOGEN", nalines))
    default_header = generate_header()
    if b[:len(default_header)] == default_header:
        b = b[len(default_header):]

    b += '    // AUTOGEN\n'
    for node in nodes:
        b += f'    fn {node.visit_name()}<\'a, \'b: \'a>(&mut self, node: &\'b '
        b += node.name_and_lifetime()
        b += ', arena: &\'a Arena) -> Self::Result { node.walk(arena, self) }\n'
    b += '}\n'

    return b

def generate_visit(nodes):
    b = ''
    b += '///! impl Visit for each node\n'
    b += '\n'
    b += 'use super::super::visit::{Visit, Visitor};\n'
    b += 'use super::*;\n'

    for node in nodes:
        b += '\n'
        b += f'impl{node.maybe_lifetime()} Visit for {node.name_and_lifetime()} {{\n'
        b += '    fn accept<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {\n'
        b += f'        v.{node.visit_name()}(self, arena)\n'
        b += '    }\n'
        if not node.is_all_terminal():
            b += '    fn walk<V: Visitor>(&self, arena: &Arena, v: &mut V) -> V::Result {\n'
            if isinstance(node, Struct):
                ntfields = [f for f in node.fields if not f.type_.is_terminal]
                for index, field in enumerate(ntfields):
                    refnode = next(n for n in nodes if n.name == field.type_.base)
                    if field.type_.special == 'option':
                        b += f'        if let Some({field.name}) = &self.{field.name} {{\n'
                        b += f'            v.{refnode.visit_name()}(arena.get({field.name}), arena)?;\n'
                        b += '        }\n'
                    elif field.type_.special == 'vec':
                        iter_var = field.name[:-1]
                        iter_var = 'r#' + iter_var if iter_var in ['type', 'where'] else iter_var
                        b += f'        for {iter_var} in arena.get_iter(&self.{field.name}) {{\n'
                        b += f'            v.{refnode.visit_name()}({iter_var}, arena)?;\n'
                        b += '        }\n'
                    else:
                        b += f'        v.{refnode.visit_name()}(arena.get(&self.{field.name}), arena)'
                    if field.type_.special is None:
                        if index != len(ntfields) - 1:
                            b += '?;'
                        b += '\n'
                if ntfields[-1:][0].type_.special is not None:
                    b += '        Default::default()\n'
            else:
                b += '        match self {\n'
                for variant in node.variants:
                    if variant.type_ is None:
                        b += f'            {node.name}::{variant.name} => Default::default(),\n'
                    else:
                        refnode = next(n for n in nodes if n.name == variant.type_)
                        b += f'            {node.name}::{variant.name}(n) => v.{refnode.visit_name()}(arena.get(n), arena),\n'
                b += '        }\n'
            b += '    }\n'
        b += '}\n'

    return b

def generate_debug(nodes, nalines):
    b = ''.join(takewhile(lambda r: r.strip() != "// AUTOGEN", nalines))
    default_header = generate_header()
    if b[:len(default_header)] == default_header:
        b = b[len(default_header):]

    b += '    // AUTOGEN'
    for node in nodes:
        b += '\n'
        b += f'    fn {node.visit_name()}<\'a, \'b: \'a>(&mut self, node: &\'b {node.name_and_lifetime()}, '
        b += '_' if node.is_all_terminal() else 'arena'
        b += ': &\'a Arena) -> Self::Result {\n'
        if isinstance(node, Struct):
            b += f'        self.start_struct("{node.name}")?\n'
            for field in node.fields:
                if field.type_.base == 'LitValue': # LitValue is not Copy
                    b += f'            .lit_value("{field.name}", &node.{field.name})?\n'
                elif field.type_.is_terminal:
                    b += f'            .field("{field.name}", node.{field.name})?\n'
                elif field.type_.special == 'option':
                    b += f'            .optional_index("{field.name}", &node.{field.name}, arena)?\n'
                elif field.type_.special == 'vec':
                    b += f'            .slice("{field.name}", &node.{field.name}, arena)?\n'
                else:
                    b += f'            .index("{field.name}", &node.{field.name}, arena)?\n'
            b += '            .end_struct()\n'
        else:
            b += '        match node {\n'
            for variant in node.variants:
                if variant.type_ is None:
                    b += f'            {node.name}::{variant.name} => EmptyResult(self.0.write_str("Global").is_ok()),\n'
                else:
                    refnode = next(n for n in nodes if n.name == variant.type_)
                    b += f'            {node.name}::{variant.name}(n) => self.variant("{variant.name}", n, arena),\n'
            b += '        }\n'
        b += '    }\n'
    b += '}\n'
    return b

if __name__ == '__main__':
    nodes, has_parse_error = parse_def(DEF_FILE)
    counts = validate_def(nodes)
    if has_parse_error or counts[4]:
        print('abort due to error')
        exit()
    dump_def(nodes, counts, False)

    for fn, file in [ \
        (lambda n: generate_entry(n, readlines(ENTRY_FILE)), ENTRY_FILE), \
        (generate_span, SPAN_FILE), \
        # (generate_new, NEW_FILE), \
        (lambda n: generate_visit_trait(n, readlines(VISIT_TRAIT_FILE)), VISIT_TRAIT_FILE), \
        (generate_visit, VISIT_FILE), \
        (lambda n: generate_cmp(n, readlines(CMP_FILE)), CMP_FILE), \
        (lambda n: generate_debug(n, readlines(UGLY_FILE)), UGLY_FILE), \
    ]:
        code = generate_header() + fn(nodes)
        print(f'write {file} {len(code.splitlines())} lines')
        with open(file, 'w') as w:
            w.write(code)
