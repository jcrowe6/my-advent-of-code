import sys
from time import time
import numpy as np
from scipy.spatial import distance_matrix

input_filename = sys.argv[1]

# hmm
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
        self.n_junctions_connected = 0

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

    def indexes_in_same_circuit(self, idx1, idx2):
        group1 = self.idx_to_group.get(idx1)
        group2 = self.idx_to_group.get(idx2)
        if group1 is None or group2 is None:
            return False
        if group1 == group2:
            return True
        return False

    def is_index_in_circuit(self, i):
        return self.idx_to_group.get(i) is not None

    def get_indexs_circuit_group_id(self, i):
        group_id = self.idx_to_group.get(i)
        if group_id is None:
            return None
        return group_id


st = time()

circuits = Circuits()

dist_matrix = distance_matrix(coord_mtx, coord_mtx)
all_pair_dists = []
for i in range(len(coord_mtx) - 1):
    for j in range(i + 1, len(coord_mtx)):
        all_pair_dists.append((dist_matrix[i, j], i, j))

all_pair_dists.sort(key=lambda x: x[0])

last_connected = None
for dist, idx1, idx2 in all_pair_dists:  # getting pairs in increasing order of distance
    if not circuits.indexes_in_same_circuit(idx1, idx2):
        circuits.connect(idx1, idx2)
        last_connected = (idx1, idx2)
    # check fully connected
    # when there is 1 group of size len(coords)
    if len(circuits.group_to_idxs) == 1:
        group = [v for v in circuits.group_to_idxs.values()][0]
        if len(group) == len(coord_mtx):
            break
last_i1, last_i2 = last_connected
last_x1 = coord_mtx[last_i1][0]
last_x2 = coord_mtx[last_i2][0]

print(int(last_x1 * last_x2))
print(f"{(time() - st):.4f}s")
