from itertools import combinations

example = '''\
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
'''

# input = example.splitlines()

with open('inputs/day_08.txt', 'r') as input_file:
    input = input_file.read().splitlines()

antenna_locations = {}
for i in range(len(input)):
    for j in range(len(input[0])):
        if input[i][j] not in ['.']:
            antenna_locations.setdefault(input[i][j], []).append((i, j))

def within_bounds(p):
    return (0 <= p[0] < len(input)) and (0 <= p[1] < len(input[0]))

antinodes = set()
for frequency in antenna_locations:
    for a, b in combinations(antenna_locations[frequency], 2):
        delta_row = b[0] - a[0]
        delta_col = b[1] - a[1]

        antinode_1 = (a[0] - delta_row, a[1] - delta_col)
        antinode_2 = (b[0] + delta_row, b[1] + delta_col)

        if within_bounds(antinode_1):
            antinodes.add(antinode_1)

        if within_bounds(antinode_2):
            antinodes.add(antinode_2)

# pt1: 301
print(len(antinodes))

def distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

def gradient(p1, p2):
    if p1[0] == p2[0]:
        return 'h'
    if p1[1] == p2[1]:
        return 'v'
    return (p2[1] - p1[1]) / (p2[0] - p1[0])

antinodes = set()

for i in range(len(input)):
    for j in range(len(input[0])):
        p = (i, j)
        for frequency in antenna_locations:
            for a, b in combinations(antenna_locations[frequency], 2):
                gradient_a_b = gradient(a, b)
                distance_a_b = distance(a, b)

                if gradient_a_b == gradient(a, p):
                    if distance(p, a) % distance_a_b == 0 or distance(p, b) % distance_a_b == 0:
                        antinodes.add(p)
                        antinodes.add(a)
                        antinodes.add(b)

# pt2: 1019
print(len(antinodes))
