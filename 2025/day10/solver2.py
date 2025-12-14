import sys
from time import time
import numpy as np
from scipy.optimize import milp, LinearConstraint, Bounds
import itertools

input_filename = sys.argv[1]

# data structure to load input into

alltargets = []
allbuttons = []

with open(input_filename) as f:
    for line in f.readlines():
        words = line.split()
        target = words[-1]
        buttons = words[1:-1]
        alltargets.append([int(n) for n in target[1:-1].split(",")])
        buttons = list(map(lambda x: list(map(int, x[1:-1].split(","))), buttons))
        allbuttons.append(buttons)

# Awesome current solution should work with slight modification
# Button / target vecs are now in the integer field
# Nullspace might not exist now... except with negative button presses...
# simple to check and skip those that have.
# but wait... the search space is now infinite...

# after some research, it's not possible with this approach.
# need to use specialized integer linear solver


def get_A_and_b(buttons: list[int], target: list[int]):
    A_list = []
    out_size = len(target)
    for btn in buttons:
        vec = [0] * out_size
        for idx in btn:
            vec[idx] = 1
        A_list.append(vec)
    A_mtx = np.array(A_list).T
    b_mtx = np.array(target)
    return A_mtx, b_mtx


st = time()

sum_least_presses = 0
for i in range(len(alltargets)):
    target = alltargets[i]
    buttons = allbuttons[i]

    A_mtx, b = get_A_and_b(buttons, target)
    n = A_mtx.shape[1]

    # need Ax = b
    constraints = LinearConstraint(A_mtx, lb=b, ub=b)

    # need x >= 0
    bounds = Bounds(lb=0)

    # integers only
    integrality = np.ones(n)

    # l1 norm
    c = np.ones(n)

    res = milp(c=c, constraints=constraints, bounds=bounds, integrality=integrality)

    x_sol = np.round(res.x).astype(int)

    sum_least_presses += sum(x_sol)


solution = sum_least_presses
print(solution)
print(f"{(time() - st):.4f}s")
