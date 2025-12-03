import sys
from time import time

input_filename = sys.argv[1]

banks = []

with open(input_filename) as f:
    for line in f.readlines():
        bank = [int(c) for c in line.strip()]
        banks.append(bank)

st = time()

# I think my intuition for the greedy solution still applies
# maximize digits starting with most significant
# except now with 12 digits, we can't consider the last 11 digits when finding the max
# and so on for the remaining


# given bank (or what's left of a bank after taking earlier digits)
# give the maximum joltage for the digit-th place by excluding the last digit-1 values
# return the digit and what remains of the bank to be selected by future digits
def max_joltage_for_digit(bank, digit):
    if digit == 1:  # no restriction for last digit
        return max(bank), bank
    max_ = 0
    max_i = -1
    for i, e in enumerate(bank[: -(digit - 1)]):
        if e > max_:
            max_ = e
            max_i = i
    return max_, bank[max_i + 1 :]


def max_joltage(bank: list[int]):
    currbank = bank
    joltage = 0
    for digit in range(12, 0, -1):
        max_digit, currbank = max_joltage_for_digit(currbank, digit)
        joltage += max_digit * (10 ** (digit - 1))
    return joltage


solution = sum([max_joltage(b) for b in banks])
print(solution)
print(f"{(time() - st):.4f}s")
