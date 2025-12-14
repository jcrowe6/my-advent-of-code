import sys
from time import time

input_filename = sys.argv[1]

# data structure to load input into
# adjacency list
network: dict[str, set[str]] = {}

with open(input_filename) as f:
    for line in f.readlines():
        nodes_str = line.strip().split()
        src = nodes_str[0][:-1]
        nbrs = nodes_str[1:]
        if src not in network:
            network[src] = set()
        for nbr in nbrs:
            if nbr not in network:
                network[nbr] = set()
            network[src].add(nbr)


# it better be a DAG otherwise the answer might be infinity
def dfs_count_paths(network: dict[str, set[str]], start_node: str, target_node: str):
    stack = [start_node]
    n_paths = 0
    while len(stack):
        node = stack.pop()
        if node == target_node:
            n_paths += 1
        for nbr in network[node]:
            stack.append(nbr)
    return n_paths


st = time()

n_paths = dfs_count_paths(network, "you", "out")

solution = n_paths
print(solution)
print(f"{(time() - st):.4f}s")
