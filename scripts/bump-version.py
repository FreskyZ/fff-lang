from common import solution
import sys

if len(sys.argv) < 2:
    print("require 2 args")
    exit()
    
solution.set_version(sys.argv[1])