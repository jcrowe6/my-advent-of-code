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
    memo = {}

    # count paths from node to target node
    def dfs(node):
        if node == target_node:
            return 1
        if node in memo:
            return memo[node]

        count = 0
        for nbr in network[node]:
            count += dfs(nbr)
        memo[node] = count
        return count

    return dfs(start_node)


st = time()

# all paths from svr -> [dac AND fft] -> out
# == svr -> dac -> fft -> out
# +  svr -> fft -> dac -> out
# counting paths can do
# fft_to_out + dac_to_fft + svr_to_dac +
# dac_to_out + fft_to_dac + svr_to_fft
# actually not exactly. if fft->dac is impossible that leg is 0
# multiply? maybe
# yes that works

svr_to_dac = dfs_count_paths(network, "svr", "dac")
dac_to_fft = dfs_count_paths(network, "dac", "fft")
fft_to_out = dfs_count_paths(network, "fft", "out")

svr_to_fft = dfs_count_paths(network, "svr", "fft")
fft_to_dac = dfs_count_paths(network, "fft", "dac")
dac_to_out = dfs_count_paths(network, "dac", "out")

# print(svr_to_dac, dac_to_fft, fft_to_out)
# print(svr_to_fft, fft_to_dac, dac_to_out)

solution = (svr_to_dac * dac_to_fft * fft_to_out) + (
    svr_to_fft * fft_to_dac * dac_to_out
)
print(solution)
print(f"{(time() - st):.4f}s")
