import sys
from time import time


input_filename = sys.argv[1]

instructions = []

with open(input_filename) as f:
    for line in f.readlines():
        instructions.append((line[0], int(line[1:])))

st = time()

# convert to pos num for R and neg for L
abs_instrs = [amt if dirn == 'R' else -amt for dirn,amt in instructions]

curr_val = 50
n_at_0 = 0
for inst in abs_instrs:
    curr_val = (curr_val+inst) % 100
    if curr_val == 0:
        n_at_0 += 1

print(n_at_0)
print(f'{(time()-st):.4f}s')