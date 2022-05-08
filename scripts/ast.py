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
ENTRY_FILE = 'src/syntax/ast.rs'
VISIT_TRAIT_FILE = 'src/syntax/visit.rs'
SPAN_FILE = 'src/syntax/ast/span.rs'
NEW_FILE = 'src/syntax/ast/new.rs'
CMP_FILE = 'src/syntax/ast/cmp.rs'
UGLY_FILE = 'src/syntax/ast/ugly.rs'
VISIT_FILE = 'src/syntax/ast/visit.rs'
PROFILE_FILE = 'src/syntax/ast/profile.rs'

TERMINAL_TYPES = ['Span', 'IsId', 'IdSpan', 'Keyword', 'Separator', 'Numeric', 'LitValue', 'FileId', 'bool', 'i32']

class Field(object):
    def __init__(self, line, name, type_base, special, display):
        self.line = line           # definition line in ast.txt
        self.name = name           # field name
        self.type_base = type_base # type name base, or node name for variant, none for PathSegment::Global
        self.special = special     # 'vec' | 'opt', should be none for variant
        self.display = display     # display, not used currently
        self.refnode = None        # node if type name is not terminal, assign later

    # the actual rust type include lifetime, optional, index or slice
    full_type = property(lambda this: this.format_type(False))
    # constructor method (in generate_new) use vec instead of slice
    new_type = property(lambda this: this.format_type(True))

    def format_type(self, new):
        if self.refnode is None and self.special is None:
            return self.type_base
        elif self.refnode is None and self.special == 'opt':
            return f'Option<{self.type_base}>'
        elif self.special is None and self.refnode.is_struct:
            return f"Index<{self.refnode.full_name}>"
        elif self.special is None:
            return f"{self.refnode.full_name}"
        elif self.special == 'opt' and self.refnode.is_struct:
            return f"Option<Index<{self.refnode.full_name}>>"
        elif self.special == 'opt':
            return f"Option<{self.refnode.full_name}>"
        elif self.special == 'vec' and self.refnode.is_struct and new:
            return f"Vec<Index<{self.refnode.full_name}>>"
        elif self.special == 'vec' and self.refnode.is_struct:
            return f"Slice<Index<{self.refnode.full_name}>>"
        elif self.special == 'vec' and new:
            return f"Vec<{self.refnode.full_name}>"
        elif self.special == 'vec':
            return f"Slice<{self.refnode.full_name}>"
        else:
            raise ValueError(f'unknown state line {self.line} type_base {self.type_base} special {self.special}')

class Node(object):
    def __init__(self, line, node_type, name, display):
        self.line = line           # definition line in ast.txt
        self.node_type = node_type # 'struct' | 'enum'
        self.name = name           # node name
        self.display = display     # node display name
        self.fields = []           # Field[], append later
    @property
    def is_struct(self):
        return self.node_type == 'struct'
    @property
    def is_enum(self):
        return self.node_type == 'enum'
    @property
    def has_non_terminal(self):
        return any(f.refnode is not None for f in self.fields)
    # this was self.name + self.lifetime, but this is used a lot so keep it
    @property
    def full_name(self):
        return self.name
    # struct as parameter is normally Index, enum is simple full name
    @property
    def parameter_name(self):
        return f'Index<{self.full_name}>' if self.is_struct else self.full_name
    @property
    def visit_name(self):
        return 'visit_' + '_'.join(self.display.split('-'))
    @property
    def emplace_name(self):
        return 'emplace_' + '_'.join(self.display.split('-'))

def match_any(expressions, input):
    for index, expr in enumerate(expressions):
        match = expr.match(input)
        if match:
            return index, match
    return -1, None

def load(def_file):
    RE_ENUM = re.compile(r'enum (?P<name>\w+) "(?P<display>[\w-]+)"')
    RE_STRUCT = re.compile(r'struct (?P<name>\w+) "(?P<display>[\w-]+)"')
    RE_FIELD = re.compile(r'(?P<name>[\w#]+): (?P<rawtype>[\w\s]+),')
    RE_VARIANT = re.compile(r'(?P<name>\w+)\((?P<type>\w+)\) "(?P<display>[\w-]+)"')

    results = []
    current = None
    has_error = False
    for line, row in enumerate(map(str.strip, open(def_file).readlines())):
        if not len(row) or row[0:2] == '//':
            continue
        line = line + 1
        match_index, match = match_any([RE_STRUCT, RE_ENUM, RE_FIELD, RE_VARIANT], row)
        if match_index in (0, 1):
            if current is not None:
                results.append(current)
            current = Node(line, 'struct' if match_index == 0 else 'enum', match.group('name'), match.group('display'))
        elif match_index == 2:
            raw = match.group('rawtype')
            type_base, special = (raw[5:], 'vec') if raw[:4] == 'many' else (raw[6:], 'opt') if raw[:5] == 'maybe' else (raw, None)
            current.fields.append(Field(line, match.group('name'), type_base, special, None))
        elif match_index == 3:
            current.fields.append(Field(line, match.group('name'), match.group('type'), None, match.group('display')))
        elif row == 'Global "global",':
            current.fields.append(Field(line, 'Global', None, None, 'global'))
        else:
            has_error = True
            print(f'line {line}: unknown format: {row}')
    results.append(current)
    results = list(sorted(results, key=lambda n: n.name))

    # assign refnode
    # check duplicate node display
    # check struct field type is terminal or known node name
    # check enum variant type is known node name
    # duplicate struct/enum name, duplicate field/variant name will be rust compile error, but duplicate variant base type is checked
    struct_count, field_count, enum_count, variant_count = 0, 0, 0, 0
    duplicate_displays = [d for d, c in [(d, len([1 for n in results if n.display == d])) for d in set([n.display for n in results])] if c > 1]
    if len(duplicate_displays):
        has_error = True
        print(f'duplicate node display {duplicate_displays}')
    for node in results:
        if node.is_struct:
            struct_count += 1
            if not len(node.fields):
                has_error = True
                print(f'line {node.line} struct {node.name} no field')
            for field in node.fields:
                field_count += 1
                if field.type_base not in TERMINAL_TYPES:
                    field.refnode = next((r for r in results if r.name == field.type_base), None)
                    if field.refnode is None:
                        has_error = True
                        print(f'line {field.line} struct {node.name} field type {field.type_base} unknown')
        else:
            enum_count += 1
            if not len(node.fields):
                has_error = True
                print(f'line {node.line} enum {node.name} no variant')
            duplicate_base_types = [t for t, c in [(t, len([1 for v in node.fields if v.type_base == t])) for t in set([v.type_base for v in node.fields])] if c > 1]
            if len(duplicate_base_types):
                has_error = True
                print(f'line {node.line} enum {node.name} has duplicate variant base type {duplicate_base_types}')
            duplicate_displays = [d for d, c in [(d, len([1 for v in node.fields if v.display == d])) for d in set([v.display for v in node.fields])] if c > 1]
            if len(duplicate_displays):
                has_error = True
                print(f'line {node.line} enum {node.name} has duplicate display {duplicate_displays}')
            for field in node.fields:
                variant_count += 1
                if field.type_base not in TERMINAL_TYPES and field.type_base is not None:
                    field.refnode = next((r for r in results if r.name == field.type_base), None)
                    if field.refnode is None:
                        has_error = True
                        print(f'line {field.line} enum {node.name} variant {field.name} type {field.type_base} unknown')
    return results, has_error, (struct_count, field_count, enum_count, variant_count)

def generate_header():
    b = ''
    b += '///! ---------------------------------------------------------------------------------\n'
    b += '///! This code is auto generated by a tool $repo/scripts/ast.py\n'
    b += '///! Changes may cause incorrect behavior and will be lost if the code is regenerated.\n'
    b += '///! ---------------------------------------------------------------------------------\n'
    b += '\n'
    return b
HEADER = generate_header()

def generate_entry(b, nodes):
    b += '// AUTOGEN'
    for node in nodes:
        b += '\n'
        if node.is_enum:
            b += '#[derive(Clone, Copy)]\n'
        b += f'pub {node.node_type} {node.full_name} {{\n'
        if node.is_struct:
            for field in node.fields:
                b += f'    pub {field.name}: {field.full_type},\n'
        else:
            for variant in node.fields:
                if variant.type_base is None:
                    b += '    Global,\n'
                else:
                    b += f'    {variant.name}({variant.full_type}),\n'
        b += '}\n'
    return b

def generate_span(b, nodes):
    b += '// AUTOGEN'
    for node in (n for n in nodes if n.is_enum):
        b += '\n'
        b += f'impl WithSpan for {node.full_name} {{\n'
        b += '    fn span(self, arena: &Arena) -> Span {\n'
        b += '        match self {\n'
        for variant in node.fields:
            if variant.type_base is None:
                b += f'            {node.name}::{variant.name} => Span::new(0, 0),\n'
            else:
                b += f'            {node.name}::{variant.name}(n) => arena.get(n).span,\n'
        b += '        }\n'
        b += '    }\n'
        b += '}\n'
    return b

def generate_new(b, nodes):

    b += '// AUTOGEN'
    for node in (n for n in nodes if n.is_enum):
        for variant in node.fields:
            if variant.refnode is not None:
                b += '\n'
                b += f'impl From<{variant.refnode.parameter_name}> for {node.full_name} {{\n'
                b += f'    fn from(index: {variant.refnode.parameter_name}) -> Self {{\n'
                b += f'        {node.name}::{variant.name}(index)\n'
                b += '    }\n'
                b += '}\n'

    b += 'pub trait EmplaceConcrete: EmplaceConcreteHelper {\n'
    for node in (n for n in nodes if n.is_struct):
        b += '\n'
        b += f'    fn {node.emplace_name}(\n'
        b += '        &self,\n'
        for field in node.fields:
            b += f'        {field.name}: {field.new_type},\n'
        b += f'    ) -> Index<{node.full_name}>;\n'
    b += '}\n'

    b += '\n'
    b += 'impl EmplaceConcrete for Arena {\n'
    for node in (n for n in nodes if n.is_struct):
        b += '\n'
        b += '    #[inline]\n'
        b += f'    fn {node.emplace_name}(\n'
        b += '        &self,\n'
        for field in node.fields:
            b += f'        {field.name}: {field.new_type},\n'
        b += f'    ) -> Index<{node.full_name}> {{\n'
        for field in node.fields:
            if field.full_type != field.new_type:
                b += f'        let {field.name} = self.build_slice({field.name});\n'
        b += f'        self.emplace(|n: &mut {node.name}| {{\n'
        for field in node.fields:
            b += f'            n.{field.name} = {field.name};\n'
        b += '        })\n'
        b += '    }\n'
    b += '}\n'

    return b

def generate_cmp(b, nodes):
    b += '// AUTOGEN'
    for node in nodes:
        b += '\n'
        # not {node.lifetime} because struct always is Index and enum always have lifetime
        b += f'impl Eq for {node.parameter_name} {{\n'
        b += f'    fn eq(&self, rhs: &Self, arena: &Arena) -> bool {{\n'
        if node.is_struct:
            b += '        let (lhs, rhs) = (arena.get(*self), arena.get(*rhs));\n'
            b += '\n'
            for (index, field) in enumerate(f for f in node.fields if f.refnode is None):
                b += f'        {"&& " if index else ""}lhs.{field.name} == rhs.{field.name}\n'
            for field in (f for f in node.fields if f.refnode is not None):
                b += f'        && Eq::eq(&lhs.{field.name}, &rhs.{field.name}, arena)\n'
        else:
            b += '        match (self, rhs) {\n'
            for variant in node.fields:
                if variant.type_base is None:
                    b += f'            ({node.name}::{variant.name}, {node.name}::{variant.name}) => true,\n'
                else:
                    b += f'            ({node.name}::{variant.name}(lhs), {node.name}::{variant.name}(rhs)) => Eq::eq(lhs, rhs, arena),\n'
            b += '            _ => false,\n'
            b += '        }\n'
        b += '    }\n'
        b += '}\n'
    return b

def generate_visit_trait(b, nodes):
    b += '    // AUTOGEN\n'
    for node in nodes:
        b += f'    fn {node.visit_name}(&mut self, node: {node.parameter_name}, arena: &Arena) -> Self::Result {{ node.walk(arena, self) }}\n'
    b += '}\n'
    return b

def generate_visit(b, nodes):
    b += '///! impl Visit for each node\n'
    b += '\n'
    b += 'use super::super::visit::{Visit, Visitor};\n'
    b += 'use super::*;\n'

    for node in nodes:
        b += '\n'
        b += f'impl Visit for {node.parameter_name} {{\n'
        b += '    fn accept<V: Visitor>(self, arena: &Arena, v: &mut V) -> V::Result {\n'
        b += f'        v.{node.visit_name}(self, arena)\n'
        b += '    }\n'
        if node.has_non_terminal:
            b += '    fn walk<V: Visitor>(self, arena: &Arena, v: &mut V) -> V::Result {\n'
            if node.is_struct:
                b += '        let this = arena.get(self);\n'
                ntfields = [f for f in node.fields if f.refnode is not None]
                for index, field in enumerate(ntfields):
                    if field.special == 'opt':
                        b += f'        if let Some({field.name}) = this.{field.name} {{\n'
                        b += f'            v.{field.refnode.visit_name}({field.name}, arena)?;\n'
                        b += '        }\n'
                    elif field.special == 'vec':
                        iter_var = field.name[:-1]
                        iter_var = 'r#' + iter_var if iter_var in ('type', 'where') else iter_var
                        b += f'        for &{iter_var} in arena.get_iter(this.{field.name}) {{\n'
                        b += f'            v.{field.refnode.visit_name}({iter_var}, arena)?;\n'
                        b += '        }\n'
                    else:
                        b += f'        v.{field.refnode.visit_name}(this.{field.name}, arena)'
                    if field.special is None:
                        if index != len(ntfields) - 1:
                            b += '?;'
                        b += '\n'
                if ntfields[-1:][0].special is not None:
                    b += '        Default::default()\n'
            else:
                b += '        match self {\n'
                for variant in node.fields:
                    if variant.type_base is None:
                        b += f'            {node.name}::{variant.name} => Default::default(),\n'
                    else:
                        b += f'            {node.name}::{variant.name}(n) => v.{variant.refnode.visit_name}(n, arena),\n'
                b += '        }\n'
            b += '    }\n'
        b += '}\n'

    return b

def generate_debug(b, nodes):
    b += '    // AUTOGEN'
    for node in nodes:
        b += '\n'
        b += f'    fn {node.visit_name}(&mut self, node: {node.parameter_name}, arena: &Arena) -> Self::Result {{\n'
        if node.is_struct:
            b += '        let (node, _guard) = self.enter(node, arena);\n'
            b += f'        self.start_struct("{node.name}")?\n'
            for field in node.fields:
                if field.type_base == 'LitValue': # LitValue is not Copy
                    b += f'            .lit_value("{field.name}", &node.{field.name})?\n'
                elif field.refnode is None:
                    b += f'            .field("{field.name}", node.{field.name})?\n'
                else:
                    method_base = 'optional' if field.special == 'opt' else 'slice' if field.special == 'vec' else 'forward'
                    b += f'            .{method_base}("{field.name}", node.{field.name}, arena)?\n'
            b += '            .end_struct()\n'
        else:
            b += '        match node {\n'
            for variant in node.fields:
                if variant.type_base is None:
                    b += f'            {node.name}::{variant.name} => EmptyResult(self.0.write_str("{node.name}::{variant.name}").is_ok()),\n'
                else:
                    b += f'            {node.name}::{variant.name}(n) => self.variant("{node.name}::{variant.name}", n, arena),\n'
            b += '        }\n'
        b += '    }\n'
    b += '}\n'
    return b

def generate_profile(b, nodes):
    b += '// AUTOGEN\n'
    # only struct is used here, because this arena refactor
    # literally removes any memory used by tagged union itself (expect its children)
    structs = [n for n in nodes if n.is_struct]

    b += '// use static name list because they are known at code generation time\n'
    b += 'static NAMES: &[&str] = &[\n'
    for node in structs:
        b += f'    "{node.display}",\n'
    b += '];\n'

    # it is here because node count is only known here and may change
    b += '\n'
    # less size array can use derive(Default) but not this one
    b += 'pub struct MemoryProfiler {\n'
    b += f'    v: [Entry; {len(structs)}],\n'
    b += '}\n'
    b += '\n'
    b += 'impl MemoryProfiler {\n'
    b += '    pub fn new() -> Self {\n'
    b += f'        Self{{ v: [Entry{{ count: 0, size: 0 }}; {len(structs)}] }}\n'
    b += '    }\n'
    b += '    pub fn coverage(&self) -> (usize, usize, String) {\n'
    b += '        (self.v.iter().filter(|e| e.count > 0).count(), self.v.len(),\n'
    b += '            self.v.iter().enumerate().filter(|(_, e)| e.count == 0).map(|(i, _)| NAMES[i]).collect::<Vec<_>>().join(", "))\n'
    b += '    }\n'
    b += '}\n'

    b += '\n'
    b += 'impl Visitor for MemoryProfiler {\n'
    b += '    type Result = EmptyResult;\n'
    b += '    // enum types are all default here because they literal does not use any memory on its own in this arena refactor\n'
    b += '\n'
    TRACE = False # when stack corrupts, need some tracing method
    for index, node in enumerate(structs):
        b += f'    fn {node.visit_name}(&mut self, node: {node.parameter_name}, arena: &Arena) -> Self::Result {{\n'
        if TRACE:
            b += f'        println!("visiting {node.name} {{}}", node.as_raw());\n'
        # note: a rare case that use .name not .full_name because it's unnecessary to write lifetime here
        b += f'        self.v[{index}].count(size_of::<{node.name}>());\n'
        if TRACE:
            b += '        let result = node.walk(arena, self);\n'
            b += f'        println!("visited {node.name} {{}}", node.as_raw());\n'
            b += '        result'
        else:
            b += '        node.walk(arena, self)\n'
        b += '    }\n'
    b += '}\n'
    return b

if __name__ == '__main__':
    nodes, has_error, counts = load(DEF_FILE)
    if has_error:
        print('abort due to error')
        exit()
    if False:
        for node in nodes:
            print(f'{node.node_type} {node.full_name} "{node.display}" {node.visit_name}')
            for field in node.fields:
                print(f'    {"field" if node.is_struct else "variant"} {field.name}: {field.full_type}')
    print(f'{len(nodes)} nodes ({counts[0]} structs {counts[1]} fields, {counts[2]} enums {counts[3]} variants)')

    for partial, fn, filename in (\
        (1, generate_entry, ENTRY_FILE),\
        (1, generate_span, SPAN_FILE),\
        (1, generate_new, NEW_FILE),\
        (1, generate_visit_trait, VISIT_TRAIT_FILE),\
        (0, generate_visit, VISIT_FILE),\
        (1, generate_cmp, CMP_FILE),\
        (1, generate_debug, UGLY_FILE),\
        (1, generate_profile, PROFILE_FILE),\
    ):
        b = ''
        if partial:
            with open(filename) as file:
                lines = file.readlines()
                b = ''.join(takewhile(lambda r: r.strip() != '// AUTOGEN', lines))
                if b[:len(HEADER)] == HEADER:
                    b = b[len(HEADER):]
        code = HEADER + fn(b, nodes)
        print(f'write {filename} {len(code.splitlines())} lines')
        with open(filename, 'w') as file:
            file.write(code)
