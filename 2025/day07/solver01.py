import sys
from time import time

input_filename = sys.argv[1]

rows = []

with open(input_filename) as f:
    for line in f.readlines():
        rows.append(list(line.strip()))


# idea
# after each row, compute the indices beams will be leaving (and thus entering the next row)
# at the same time return the number of splitters activated
# prev beams is a set of indices
def compute_beams(prev_beams, curr_row):
    new_beams = set()
    splits = 0
    for i in range(len(curr_row)):
        val = curr_row[i]
        if i in prev_beams:  # beam above this spot
            if val == ".":  # empty space, it continues
                new_beams.add(i)
            if val == "^":  # splitter, L/R get beams (checked that ^^ does not exist)
                new_beams.add(i - 1)
                new_beams.add(i + 1)
                splits += 1
    return new_beams, splits


st = time()

first_row = rows[0]
first_beams = set(filter(lambda i: first_row[i] == "S", range(len(first_row))))
curr_beams = first_beams
total_splits = 0
for row in rows[1:]:
    new_beams, splits = compute_beams(curr_beams, row)
    curr_beams = new_beams
    total_splits += splits

print(total_splits)
print(f"{(time() - st):.4f}s")
