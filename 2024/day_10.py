example = '''\
0123
1234
8765
9876
'''

example2 = '''\
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
'''

# input = example2.splitlines()

with open('inputs/day_10.txt', 'r') as input_file:
    input = input_file.read().splitlines()

def neighbor_coords(i, j):
    rows, cols = len(input), len(input[0])
    coords = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    return [(n_i, n_j) for n_i, n_j in coords if 0 <= n_i < rows and 0 <= n_j < cols]

graph = {}
trailheads = []
for i in range(len(input)):
    for j in range(len(input)):
        height = int(input[i][j])
        if height == 0:
            trailheads.append((i, j))

        for n_i, n_j in neighbor_coords(i, j):
            if int(input[n_i][n_j]) == height + 1:
                graph.setdefault((i, j), []).append((n_i, n_j))

def bfs(graph, start):
    visited = set()
    queue = [start]
    while queue:
        node = queue.pop(0)
        if node not in visited:
            visited.add(node)
            neighbors = graph.get(node, [])
            queue.extend(neighbors)
    return visited

score = 0
for trailhead in trailheads:
    reachable_peaks = set()
    for i, j in bfs(graph, trailhead):
        height = int(input[i][j])
        if height == 9:
            reachable_peaks.add((i, j))
    score += len(reachable_peaks)

# pt1: 798
print(score)


def count_paths_dfs(graph, start):
    i, j = start
    if int(input[i][j]) == 9:
        return 1

    # No need to worry about cycle because neighbors are always increasing in height
    return sum(count_paths_dfs(graph, neighbor) for neighbor in graph.get(start, []))

score = 0
for trailhead in trailheads:
    score += count_paths_dfs(graph, trailhead)

# pt2: 1816
print(score)
