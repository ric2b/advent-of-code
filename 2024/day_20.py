import math
from itertools import combinations

example = '''\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
'''

input = example.strip()

with open('inputs/day_20.txt', 'r') as input_file:
    input = input_file.read().strip()

def load_map(raw_map):
    return [list(row) for row in raw_map.split('\n')]

def neighbors(map, position):
    i, j = position
    for y, x in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if 0 <= y < len(map) and 0 <= x < len(map[0]) and map[y][x] != '#':
            yield (y, x)

def djikstra(map, start, end):
    distances = { start: 0 }
    previous = { start: set() }
    queue = { start }

    while queue:
        current = min(queue, key=distances.get)
        if current == end:
            return distances, previous
        queue.remove(current)

        for neighbor in neighbors(map, current):
            new_distance = distances[current] + 1

            if new_distance < distances.get(neighbor, math.inf):
                previous[neighbor] = { current }
                distances[neighbor] = new_distance
                queue.add(neighbor)

def find(map, c):
    for i, row in enumerate(map):
        for j, cell in enumerate(row):
            if cell == c:
                return i, j

map = load_map(input)
start, end = find(map, 'S'), find(map, 'E')

distances, previous = djikstra(map, start, end)
no_cheats_time = distances[end]

def manhattan_distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

cheats_time_saved = {}
for p1, p2 in combinations(distances, 2):
    if p1 != p2 and manhattan_distance(p1, p2) == 2:
        cheats_time_saved[p1, p2] = abs(distances[p1] - distances[p2]) - manhattan_distance(p1, p2)

# pt1: 1323
print(len([cheat for cheat in cheats_time_saved if cheats_time_saved[cheat] >= 100]))

cheats_time_saved = {}
for p1, p2 in combinations(distances, 2):
    if p1 != p2 and manhattan_distance(p1, p2) <= 20:
        cheats_time_saved[p1, p2] = abs(distances[p1] - distances[p2]) - manhattan_distance(p1, p2)

# pt2: 983905
print(len([cheat for cheat in cheats_time_saved if cheats_time_saved[cheat] >= 100]))
