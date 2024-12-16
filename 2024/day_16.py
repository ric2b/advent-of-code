import math

example = '''\
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
'''

example2 = '''\
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
'''

# input = example
# input = example2

with open('inputs/day_16.txt', 'r') as input_file:
    input = input_file.read()

map = [list(row) for row in input.strip().split('\n')]

def is_reverse(movement1, movement2):
    di, dj = movement1
    dy, dx = movement2
    return di == -dy and dj == -dx

def neighbors(map, position, movement_to_position):
    i, j = position
    for y, x in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if map[y][x] != '#' and 0 <= y < len(map) and 0 <= x < len(map[0]):
            movement_to_neighbor = (y - i, x - j)

            if not is_reverse(movement_to_position, movement_to_neighbor):
                yield ((y, x), movement_to_neighbor)

def find(map, c):
    for i, row in enumerate(map):
        for j, cell in enumerate(row):
            if cell == c:
                return i, j

start, end = find(map, 'S'), find(map, 'E')

graph = {}
queue = [(start, (0, 1))]
while queue:
    position, movement_to_position = queue.pop(0)

    node_neighbors = list(neighbors(map, position, movement_to_position))
    graph[position, movement_to_position] = node_neighbors

    for node_neighbor in node_neighbors:
        if node_neighbor not in graph:
            queue.append(node_neighbor)

def djikstra(graph, start, end):
    distances = { (start, (0, 1)): 0 }
    queue = { (start, (0, 1)) }

    while queue:
        current_node = min(queue, key=distances.get)
        current_position, movement_to_current = current_node

        if current_position == end:
            return distances[current_node]
        queue.remove(current_node)

        for neighbor_node in graph[current_node]:
            neighbor, movement_to_neighbor = neighbor_node

            distance_to_neighbor = 1 if movement_to_current == movement_to_neighbor else 1000 + 1
            new_distance = distances[current_node] + distance_to_neighbor

            if neighbor_node not in distances or new_distance < distances[neighbor_node]:
                distances[neighbor_node] = new_distance
                queue.add(neighbor_node)

# pt1: 108504
print(djikstra(graph, start, end))

# pt2: 538

def djikstra_distances(graph, start, end):
    distances = { (start, (0, 1)): 0 }
    previous = { (start, (0, 1)): set() }
    queue = { (start, (0, 1)) }

    while queue:
        current_node = min(queue, key=distances.get)
        current_position, movement_to_current = current_node

        if current_position == end:
            return distances, previous
        queue.remove(current_node)

        for neighbor_node in graph[current_node]:
            neighbor, movement_to_neighbor = neighbor_node

            distance_to_neighbor = 1 if movement_to_current == movement_to_neighbor else 1000 + 1
            new_distance = distances[current_node] + distance_to_neighbor

            if new_distance < distances.get(neighbor_node, math.inf):
                previous[neighbor_node] = { current_node }
                distances[neighbor_node] = new_distance
                queue.add(neighbor_node)
            if new_distance == distances.get(neighbor_node):
                previous[neighbor_node].add(current_node)

distances, previous = djikstra_distances(graph, start, end)
end_states = [node for node in distances if node[0] == end]
lowest_score = min(distances[node] for node in end_states)

queue = {node for node in end_states if distances[node] == lowest_score}
tiles = set()

while queue:
    current_node = queue.pop()
    tiles.add(current_node[0])
    if current_node not in previous:
        breakpoint()
    queue |= previous[current_node]

# pt2: 538
print(len(tiles))
