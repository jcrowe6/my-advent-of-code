import sys
from time import time

input_filename = sys.argv[1]

numbers_strs = []
ops_str = []
# input parsing is the real challenge here
with open(input_filename) as f:
    lines = f.readlines()
    for i, line in enumerate(lines):
        if i < len(lines) - 1:  # numbers
            numbers_strs.append(line)
        else:  # ops
            ops_str = line

st = time()

# numbers[i] is a list of the numbers for problem i
# just go 1 char at a time parsing the numbers
numbers = []
# holds the sublist, gets reset every space column
tmp_nums = []
ops = []
for ci in range(len(numbers_strs[0])):
    op = ops_str[ci] if ci < len(ops_str) else ""
    op = op.strip()
    num = ""
    for ri in range(len(numbers_strs)):
        num += numbers_strs[ri][ci]
    num = num.strip()
    if len(op) + len(num) == 0:  # space column
        numbers.append(tmp_nums.copy())
        tmp_nums = []
        continue
    tmp_nums.append(int(num))
    if op != "":
        ops.append(op)

solution = 0
for i in range(len(numbers)):
    op = ops[i]
    if op == "+":
        solution += sum(numbers[i])
    elif op == "*":
        ans = 1
        for num in numbers[i]:
            ans *= num
        solution += ans

print(solution)
print(f"{(time() - st):.4f}s")
