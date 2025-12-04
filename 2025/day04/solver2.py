import sys
from time import time

input_filename = sys.argv[1]

rollmap = []

with open(input_filename) as f:
    for line in f.readlines():
        row = list(line.strip())
        rollmap.append(row)


# return list of chars in the 8 adjacent positions
def get_neighbors(rollmap, i, j):
    out = []
    for id in range(-1, 2):
        for jd in range(-1, 2):
            if id != 0 or jd != 0:
                newi = i + id
                newj = j + jd
                if (
                    newi >= 0
                    and newi < len(rollmap)
                    and newj >= 0
                    and newj < len(rollmap[0])
                ):
                    out.append(rollmap[newi][newj])
    return out


def count_neighbor_rolls(rollmap, i, j):
    neighbors = get_neighbors(rollmap, i, j)
    return len(list(filter(lambda c: c == "@", neighbors)))


# Perform one loop removing all possible
# modifies the provided rollmap
# returns number of rolls removed
def remove_accessible_rolls(rollmap):
    removed = 0
    for i in range(len(rollmap)):
        for j in range(len(rollmap[0])):
            # has a roll and is accessible
            if rollmap[i][j] == "@" and count_neighbor_rolls(rollmap, i, j) < 4:
                rollmap[i][j] = "."
                removed += 1
    return removed


st = time()

solution = 0
rolls_left = True
while rolls_left:
    n_removed = remove_accessible_rolls(rollmap)
    solution += n_removed
    rolls_left = n_removed > 0

print(solution)
print(f"{(time() - st):.4f}s")
