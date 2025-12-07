import sys
from time import time

input_filename = sys.argv[1]

rows = []

with open(input_filename) as f:
    for line in f.readlines():
        rows.append(list(line.strip()))


# idea
# after each row, compute the indices the particle could take (could take the same index twice)
# prev beams is a list of indices the particle could come from
def compute_beams(prev_paths, curr_row):
    new_paths = []
    for i in range(len(curr_row)):
        val = curr_row[i]
        n_paths_incoming = sum([1 if pi == i else 0 for pi in prev_paths])
        if n_paths_incoming > 0:  # at least once path entering here
            if val == ".":  # empty space, they continue
                new_paths += [i] * n_paths_incoming
            if val == "^":  # splitter, L/R get beams (checked that ^^ does not exist)
                new_paths += [(i - 1)] * n_paths_incoming
                new_paths += [(i + 1)] * n_paths_incoming
    return new_paths


st = time()

first_row = rows[0]
first_path = list(filter(lambda i: first_row[i] == "S", range(len(first_row))))
curr_paths = first_path
total_splits = 0
for row in rows[1:]:
    new_paths = compute_beams(curr_paths, row)
    # print(curr_paths, new_paths)
    curr_paths = new_paths

print(len(curr_paths))
print(f"{(time() - st):.4f}s")
