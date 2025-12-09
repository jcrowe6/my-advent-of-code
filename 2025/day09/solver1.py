import sys
from time import time

input_filename = sys.argv[1]

coords = []

with open(input_filename) as f:
    for line in f.readlines():
        coords.append(tuple(map(int, line.strip().split(","))))


def coords_to_area(c1, c2):
    x1, y1 = c1
    x2, y2 = c2
    w = abs(x1 - x2) + 1
    h = abs(y1 - y2) + 1
    return w * h


st = time()

max_area = 0
for i in range(len(coords) - 1):
    for j in range(1, len(coords)):
        c1 = coords[i]
        c2 = coords[j]
        # print(c1, c2, coords_to_area(c1, c2))
        a = coords_to_area(c1, c2)
        max_area = max(max_area, a)

solution = max_area
print(solution)
print(f"{(time() - st):.4f}s")
