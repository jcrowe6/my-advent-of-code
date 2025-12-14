import sys
from time import time
from sympy import Matrix
from sympy.polys.matrices import DomainMatrix
from sympy import GF, init_printing, linsolve, symbols
import itertools

input_filename = sys.argv[1]

# data structure to load input into

alltargets = []
allbuttons = []

with open(input_filename) as f:
    for line in f.readlines():
        words = line.split()
        target = words[0]
        buttons = words[1:-1]
        alltargets.append([c for c in target[1:-1]])
        buttons = list(map(lambda x: list(map(int, x[1:-1].split(","))), buttons))
        allbuttons.append(buttons)

# idea: convert target and buttons into unit vectors
# and find linear combinations modulo 2 that result in the target vector

# I had never heard of doing matrix math over fields other than the reals
# so had to consult the internet and research ways of solving this
# eventually found out sympy can
# but it's hard to get it to give you all solutions
# in this code I find the RREF of the augmented matrix (in GF(2))
# and use that to get 1 solution, then iterate over all possible null vectors
# adding that in (mod 2) to get all solutions.
# from there it's easy to keep track of the best solution


def create_augmented_matrix(buttons: list[int], target: list[str]):
    aug = []
    for btn in buttons:
        vec = [0] * out_size
        for idx in btn:
            vec[idx] = 1
        aug.append(vec)
    tgt_vec = [0 if c == "." else 1 for c in target]
    aug.append(tgt_vec)
    dm = DomainMatrix.from_list(aug, domain=GF(2)).transpose()
    return dm


st = time()

sum_least_presses = 0
for i in range(len(alltargets)):
    target = alltargets[i]
    buttons = allbuttons[i]
    out_size = len(target)

    aug_dm = create_augmented_matrix(buttons, target)

    rref_aug, pivots = aug_dm.rref()
    rref_aug = rref_aug.to_Matrix()
    # print(pivots)
    x_particular = Matrix.zeros(rref_aug.shape[1] - 1, 1)
    for row_i, col_i in enumerate(pivots):
        x_particular[col_i] = rref_aug[row_i, -1]
    # print(x_particular)
    null_basis: Matrix = aug_dm[:, :-1].nullspace().to_Matrix().transpose()
    ns_dim = null_basis.shape[1]
    if ns_dim == 0:
        sum_least_presses += sum(x_particular)
        continue
    least_presses = None
    for coeffs in itertools.product([0, 1], repeat=ns_dim):
        coeffs = Matrix(coeffs)
        null_vec = null_basis.multiply(coeffs)
        x: Matrix = x_particular + null_vec
        x = x.applyfunc(lambda x: x % 2)  #
        n_presses = sum(x)
        if least_presses is None or n_presses < least_presses:
            least_presses = n_presses
    sum_least_presses += least_presses

solution = sum_least_presses
print(solution)
print(f"{(time() - st):.4f}s")
