import random
import os
from functools import reduce

operands = [str(i) for i in range(0, 9)]
operators = ['+', '*', '/', '%', '-', '&', '|', '^', '<', '>', '==', '<=', '>=', '<<', '>>', '&&', '||']
expr = reduce(
    lambda x, y: x + y, 
    [' %s %s' % (random.choice(operators), random.choice(operands)) for _ in range(random.choice(range(20)))], 
    random.choice(operands))
print(expr)
os.system('echo "%s" | clip' % expr)