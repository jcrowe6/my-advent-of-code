import sys
from time import time

input_filename = sys.argv[1]

# data structure to load input into

with open(input_filename) as f:
    for line in f.readlines():
        # load line into data structure
        pass 

st = time()

# solve

solution = 1
print(solution)
print(f'{(time()-st):.4f}s')