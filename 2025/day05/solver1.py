import sys
from time import time

input_filename = sys.argv[1]

ranges = []
ingredients = []

past_ranges = False
with open(input_filename) as f:
    for line in f.readlines():
        if line == "\n":
            past_ranges = True
            continue
        if not past_ranges:
            range_ = tuple(map(int, line.strip().split("-")))
            ranges.append(range_)
        else:
            ingredients.append(int(line.strip()))


def in_a_range(ingredient, ranges):
    for s, e in ranges:
        if ingredient >= s and ingredient <= e:
            return True
    return False


# could do a merging operation on ranges but don't think it's neccessary

st = time()

solution = sum(map(lambda i: in_a_range(i, ranges), ingredients))

print(solution)
print(f"{(time() - st):.4f}s")
