# refactor to span helper script for syntax

import os
import re
from glob import glob
import colorama

colorama.init()
pattern = re.compile(r'make_strpos!\(1, (\d*), 1, (\d*)\)')

SYNTAX_SRC = r"..\semantic\src"
for src_file_path in [y for x in os.walk(SYNTAX_SRC) for y in glob(os.path.join(x[0], '*.rs'))]:
    new_src = ''
    with open(src_file_path) as src_file:
        src = src_file.read()
        print(colorama.Back.GREEN + '\nsrc file: ' + src_file_path + colorama.Style.RESET_ALL)
        src = src.replace('make_str_pos', 'make_strpos')
        src = src.replace('StringPosition', 'Span')

        src_segments = []
        prev_last_index = 0
        for match in re.finditer(pattern, src):
            src_segments.append(src[prev_last_index: match.regs[0][0]])  # previous normal part
            src_segments.append('make_span!(')
            src_segments.append(str(int(src[match.regs[1][0]: match.regs[1][1]]) - 1))
            src_segments.append(', ')
            src_segments.append(str(int(src[match.regs[2][0]: match.regs[2][1]]) - 1))
            src_segments.append(')')
            prev_last_index = match.regs[0][1]
            has_match = True
        src_segments.append(src[prev_last_index: ])
        src = ''.join(src_segments)

        if list(re.findall(re.compile('make_strpos!'), src)):
            print(colorama.Back.RED + 'ATTENTION: NOT ALL `make_strpos` REPLACED!!' + colorama.Style.RESET_ALL)
        new_src = src
    with open(src_file_path, 'w') as src_file:
        src_file.write(new_src)

# type_use, primary
