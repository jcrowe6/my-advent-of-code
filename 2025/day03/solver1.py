import sys
from time import time

input_filename = sys.argv[1]

banks = []

with open(input_filename) as f:
    for line in f.readlines():
        bank = [int(c) for c in line.strip()]
        banks.append(bank)

st = time()

# sufficient to maximize first digit, then second digit
# just need to take leftmost max first digit
# which can never be in the last position


def max_joltage(bank: list[int]):
    max_tens = 0
    max_tens_i = -1
    for i, e in enumerate(bank[:-1]):
        if e > max_tens:
            max_tens = e
            max_tens_i = i
    # then take max from remaining list
    max_ones = max(bank[max_tens_i + 1 :])
    return (10 * max_tens) + max_ones


solution = sum([max_joltage(b) for b in banks])
print(solution)
print(f"{(time() - st):.4f}s")
