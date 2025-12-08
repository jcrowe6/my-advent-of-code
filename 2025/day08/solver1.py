import sys
from time import time
import numpy as np
from scipy.spatial import KDTree

input_filename = sys.argv[1]

# should use kd tree for getting dists
coords = []

with open(input_filename) as f:
    for line in f.readlines():
        c = tuple(map(float, line.strip().split(",")))
        coords.append(c)
coord_mtx = np.array(coords)


# need to store circuits with quick lookup
class Circuits:
    def __init__(self):
        self.group_to_idxs: dict[int, set] = {}
        self.idx_to_group: dict[int, int] = {}
        self.next_unused_group_id = 0

    # put index 1 and 2 in same group
    def connect(self, i1, i2):
        i1_group = self.idx_to_group.get(i1)
        i2_group = self.idx_to_group.get(i2)
        if i1_group is not None and i2_group is not None:
            if i1_group == i2_group:
                raise Exception("Trying to connect idxs in same group id already")
            # Merge groups, drop greater group id
            min_group_id = min(i1_group, i2_group)
            other_group_id = max(i1_group, i2_group)
            min_group_idxs = self.group_to_idxs[min_group_id]
            other_group_idxs = self.group_to_idxs[other_group_id]
            # add to smaller group
            for idx in other_group_idxs:
                self.idx_to_group[idx] = min_group_id
            self.group_to_idxs[min_group_id] = min_group_idxs.union(other_group_idxs)
            # drop larger group
            self.group_to_idxs.pop(other_group_id)
        elif i1_group is not None:
            self.idx_to_group[i2] = i1_group
            self.group_to_idxs[i1_group].add(i2)
        elif i2_group is not None:
            self.idx_to_group[i1] = i2_group
            self.group_to_idxs[i2_group].add(i1)
        else:  # neither are in groups, use new id
            new_id = self.next_unused_group_id
            self.next_unused_group_id += 1
            self.idx_to_group[i1] = new_id
            self.idx_to_group[i2] = new_id
            self.group_to_idxs[new_id] = set([i1, i2])

    def is_index_in_circuit(self, i):
        return self.idx_to_group.get(i) is not None

    def get_indexs_circuit_group_id(self, i):
        group_id = self.idx_to_group.get(i)
        if group_id is None:
            return None
        return group_id


# for a given index in coord_mtx
# returns the index of the nearest neighbor that's valid and it's dist
def nearest_valid_neighbor(kdtree: KDTree, circuits: Circuits, idx):
    coord = coord_mtx[idx]
    dists, nbr_idxs = kdtree.query(
        coord, k=1000
    )  # may not need this many options, may need more...
    dists = dists[1:]  # first elem will always be this index itself
    nbr_idxs = nbr_idxs[1:]
    idx_groupid = circuits.get_indexs_circuit_group_id(idx)
    for dist, nbr_i in zip(
        dists, nbr_idxs
    ):  # go through neighbors in increasing order of distance
        # only not valid if we're already in the same circuit together
        if (
            idx_groupid is not None
            and idx_groupid == circuits.get_indexs_circuit_group_id(nbr_i)
        ):
            # print(f"Skipping min pair {idx, nbr_i}")
            continue
        # valid pairing!
        return dist, nbr_i
    raise Exception(f"No valid pairing found in {len(dists)} neighbors")


# O(n) with kd_tree vs n^2
def get_min_dist_valid_idx_pair(kdtree: KDTree, circuits: Circuits):
    min_dist = None
    min_idx_pair = None
    for idx in range(kdtree.n):
        dist, nbr_idx = nearest_valid_neighbor(kdtree, circuits, idx)
        if min_dist is None or dist < min_dist:
            min_dist = dist
            min_idx_pair = (idx, nbr_idx)
    return min_idx_pair


st = time()

kdtree = KDTree(coord_mtx)
circuits = Circuits()

for _ in range(1000 - 1):
    idx1, idx2 = get_min_dist_valid_idx_pair(kdtree, circuits)
    # print(idx1, idx2)
    circuits.connect(idx1, idx2)
    # print(circuits.group_to_idxs)

group_sizes = list(map(len, circuits.group_to_idxs.values()))
group_sizes.sort()
print(group_sizes)
top3 = group_sizes[-3:]
solution = 1
for v in top3:
    solution *= v
print(solution)
print(f"{(time() - st):.4f}s")
