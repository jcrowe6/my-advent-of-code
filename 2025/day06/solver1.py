import sys
from time import time

input_filename = sys.argv[1]

numbers = []
ops = []

with open(input_filename) as f:
    lines = f.readlines()
    for i, line in enumerate(lines):
        strs = line.strip().split()
        if i < len(lines) - 1:  # numbers
            numbers.append(list(map(int, strs)))
        else:  # ops
            ops = strs


st = time()

solution = 0
for i in range(len(numbers[0])):
    op = ops[i]
    if op == "+":
        solution += sum(lst[i] for lst in numbers)
    elif op == "*":
        ans = 1
        for lst in numbers:
            ans *= lst[i]
        solution += ans

print(solution)
print(f"{(time() - st):.4f}s")
