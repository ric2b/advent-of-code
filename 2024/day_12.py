from itertools import chain

example = '''\
AAAA
BBCD
BBCC
EEEC
'''

example2 = '''\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
'''

example3 = '''\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
'''

# input = example.splitlines()
# input = example2.splitlines()
# input = example3.splitlines()

with open('inputs/day_12.txt', 'r') as input_file:
    input = input_file.read().splitlines()

def raw_neighbor_coords(i, j):
    return [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

def neighbor_coords(i, j):
    rows, cols = len(input), len(input[0])
    return [(n_i, n_j) for n_i, n_j in raw_neighbor_coords(i, j) if 0 <= n_i < rows and 0 <= n_j < cols]

graph = {}
for i, row in enumerate(input):
    for j, plot_type in enumerate(row):
        current_regions = graph.setdefault(plot_type, [])
        regions_joined = []

        for region in current_regions:
            if (i, j) in region:
                regions_joined.append(region)
                for n_i, n_j in neighbor_coords(i, j):
                    if input[n_i][n_j] == plot_type:
                        region.add((n_i, n_j))

        if len(regions_joined) > 1:
            for region in regions_joined:
                current_regions.remove(region)
            graph[plot_type].append(set.union(*regions_joined))

        if not regions_joined:
            new_region = {(i, j)}
            for n_i, n_j in neighbor_coords(i, j):
                if input[n_i][n_j] == plot_type:
                    new_region.add((n_i, n_j))
            current_regions.append(new_region)

def region_price(region):
    area = len(region)
    perimeter = 0
    for i, j in region:
        for n_i, n_j in raw_neighbor_coords(i, j):
            if (n_i, n_j) not in region:
                perimeter += 1
    return area * perimeter

# pt1: 1464678
print(sum([region_price(region) for region in chain(*graph.values())]))
