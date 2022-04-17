#!python3
# run with ./scripts/make-ast-mem.py > tests/ast/mem.txt

import os
from glob import glob

os.system(f'cargo build --bin ffc')
for filename in sorted(glob('tests/ast/*.f3')):
    print(filename)
    os.system(f'target/debug/ffc --print ast --debug memory {filename} >{filename[:-2]}stdout')
