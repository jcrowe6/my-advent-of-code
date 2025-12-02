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


# repeats the base number n times giving an invalid number
# (12,3) -> 121212
def repeat_number(base, n):
    ndigits = n_digits(base)
    out = base
    for i in range(n - 1):
        mult = 10 ** (ndigits * (i + 1))
        out += base * mult
    return out


st = time()

# Idea: generate all invalid id's up to required num of digits
# which is 10 digits from -> max([n_digits(e) for s, e in ranges])
# then can just check ranges

invalids = set()

for digits in range(2, 11):  # no invalid single digit numbers
    # number of digits of subnumbers that could be repeated to make an invalid number of length digits
    factor_digits = list(filter(lambda d: digits % d == 0, range(1, digits)))
    for subnum_digits in factor_digits:
        # get number of times needed to repeat
        times = digits // subnum_digits
        # iter over entire range of subnum digits
        min_ = 10 ** (subnum_digits - 1)
        max_ = min_ * 10  # end not inclusive in range()
        for base in range(min_, max_):
            invalid = repeat_number(base, times)
            invalids.add(invalid)

# now it's easy
solution = 0
for s, e in ranges:
    for i in range(s, e + 1):
        if i in invalids:
            solution += i

print(solution)
print(f"{(time() - st):.4f}s")
