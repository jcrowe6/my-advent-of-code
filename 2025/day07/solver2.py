import sys
from time import time

input_filename = sys.argv[1]

rows = []

with open(input_filename) as f:
    for line in f.readlines():
        rows.append(list(line.strip()))


# idea
# after each row, compute the indices the particle could take (could take the same index twice)
# prev beams is a DICT of indices/counts the particle could come from
# too slow with lists
# they are just maintaining a count of indices,
# use dict instead
def compute_beams(prev_paths: dict[int, int], curr_row: list[str]):
    new_paths = {}
    for i in range(len(curr_row)):
        val = curr_row[i]
        n_paths_incoming = prev_paths[i] if i in prev_paths else 0
        if n_paths_incoming > 0:  # at least once path entering here
            if val == ".":  # empty space, they continue
                if i not in new_paths:
                    new_paths[i] = 0
                new_paths[i] += n_paths_incoming
            if val == "^":  # splitter, L/R get beams (checked that ^^ does not exist)
                if i - 1 not in new_paths:
                    new_paths[i - 1] = 0
                new_paths[i - 1] += n_paths_incoming
                if i + 1 not in new_paths:
                    new_paths[i + 1] = 0
                new_paths[i + 1] += n_paths_incoming
    return new_paths


st = time()

first_row = rows[0]
first_idx = list(filter(lambda i: first_row[i] == "S", range(len(first_row))))[0]
curr_paths = {first_idx: 1}
for row in rows[1:]:
    new_paths = compute_beams(curr_paths, row)
    # print(curr_paths, new_paths)
    curr_paths = new_paths

solution = sum(curr_paths.values())
print(solution)
print(f"{(time() - st):.4f}s")
