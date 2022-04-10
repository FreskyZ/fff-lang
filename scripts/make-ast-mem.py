#!python3
# run with ./scripts/make-ast-mem.py > tests/ast/mem.txt

import os
from glob import glob

for filename in sorted(glob('tests/ast/*.f3')):
    print(filename, flush=True)
    os.system(f'target/debug/ffc --print ast-mem {filename}')