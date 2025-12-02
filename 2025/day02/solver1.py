import sys
from time import time
from math import log10

input_filename = sys.argv[1]

ranges = []

with open(input_filename) as f:
    for line in f.readlines():
        ranges = line.split(",")
        ranges = list(map(lambda s: tuple(map(int, s.split("-"))), ranges))


def n_digits(num: int):
    return int(log10(num)) + 1


# "doubles" the number giving an invalid
# ex: 123->123123
def invalidate_number(lower):
    ndigits = n_digits(lower)
    return lower + ((10**ndigits) * lower)


# return first invalid that could possibly be in a range starting with
# a number with n digits
# and returns the "lower" section of it which will have half as many digits
def get_first_invalid_and_lower(ndigits):
    if ndigits % 2 == 1:
        ndigits += 1  # if odd next invalid will be the next even number of digits
    # if ndigits == 2:
    #     return 11,1
    lower = 10 ** ((ndigits // 2) - 1)
    upper = 10 ** (ndigits - 1)
    return lower + upper, lower


# yields all invalids that could exist in provided range
def invalid_range_generator(start, end):
    ndigits = n_digits(start)
    truestart, truestart_lower = get_first_invalid_and_lower(ndigits)
    curr_invalid = truestart
    curr_lower = truestart_lower
    while curr_invalid <= end:
        yield curr_invalid
        curr_lower += 1
        curr_invalid = invalidate_number(curr_lower)


# Idea: generate invalid id's and check for presence in range
def sum_invalids(range_: tuple):
    s, e = range_
    sum_invalid = 0
    for invalid in invalid_range_generator(s, e):
        if invalid >= s:  # is truly invalid
            sum_invalid += invalid
    return sum_invalid


st = time()

solution = sum(map(sum_invalids, ranges))

print(solution)
print(f"{(time() - st):.4f}s")
