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

edge_rects = []
for c1,c2 in zip(coords, coords[1:]+[coords[0]]):
    ex1, ey1 = c1
    ex2, ey2 = c2
    eleft_x = min(ex1, ex2)
    eright_x = max(ex1, ex2)
    etop_y = min(ey1, ey2)
    ebottom_y = max(ey1, ey2)
    edge_rects.append((eleft_x, etop_y, eright_x, ebottom_y))


def rect_in_edges(c1, c2, edge_rects):
    x1, y1 = c1
    x2, y2 = c2
    # interior rectangle (excluding edges)
    left_x = min(x1, x2)+1
    right_x = max(x1, x2)-1
    top_y = min(y1, y2)+1
    bottom_y = max(y1, y2)-1

    for (eleft_x, etop_y, eright_x, ebottom_y) in edge_rects:
        above = ebottom_y < top_y
        below = etop_y > bottom_y
        left = eright_x < left_x
        right = eleft_x > right_x
        if not (above or below or left or right): # Must be intersecting
            return True
    return False

max_area = 0
for i in range(len(coords) - 1):
    for j in range(i, len(coords)):
        c1 = coords[i]
        c2 = coords[j]
        # print(c1, c2, coords_to_area(c1, c2))
        if not rect_in_edges(c1, c2, edge_rects):
            a = coords_to_area(c1, c2)
            max_area = max(max_area, a)

solution = max_area
print(solution)
print(f"{(time() - st):.4f}s")
