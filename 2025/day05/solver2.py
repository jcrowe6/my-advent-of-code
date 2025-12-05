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


# now gotta merge ranges that's the whole problem

st = time()

# sort by start index
ranges.sort(key=lambda x: x[0])

# think we just compare end to next start and merge if >=
# taking max of the two ends as new end
i = 0
while i < len(ranges) - 1:
    cs, ce = ranges[i]
    ns, ne = ranges[i + 1]
    if ce >= ns:  # merge
        new_end = max(ce, ne)
        new_range = (cs, new_end)
        ranges.pop(i)
        ranges.pop(i)
        ranges.insert(i, new_range)
        continue
    i += 1

# now it's just sum of sizes of ranges
solution = sum(map(lambda x: x[1] - x[0] + 1, ranges))

print(solution)
print(f"{(time() - st):.4f}s")
