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
    new_total = curr_val+inst
    new_val = (new_total) % 100

    # feels like divide may give answer...
    n_times_into_100 = abs(new_total) // 100
    
    # undercounts 0/negative (doesn't count until -100) so add 1 if so, and aren't already at 0 (would double count)
    times_at_0_while_turning = n_times_into_100 + (1 if new_total <= 0 and curr_val > 0 else 0) 

    # print(f'{curr_val}->{new_total}->{new_val}', times_at_0_while_turning)

    n_at_0 += times_at_0_while_turning

    curr_val = new_val

print(n_at_0)
print(f'{(time()-st):.4f}s')