import math

example = '''\
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
'''

# input = example.splitlines()
# width, height, fallen = 7, 7, 12

with open('inputs/day_18.txt', 'r') as input_file:
    input = input_file.read().splitlines()
width, height, fallen = 71, 71, 1024

map = [['.' for j in range(width)] for i in range(height)]

for row in input[:fallen]:
    x, y = [int(c) for c in row.split(',')]
    map[y][x] = '#'

start, end = (0, 0), (height - 1, width - 1)

def neighbors(map, position):
    i, j = position
    for y, x in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
        if 0 <= y < len(map) and 0 <= x < len(map[0]) and map[y][x] != '#' :
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


distances, previous = djikstra(map, start, end)
# pt1: 284
print(distances[end])

# pt2: 51,50
for row in input[fallen:]:
    x, y = [int(c) for c in row.split(',')]
    map[y][x] = '#'

    if not djikstra(map, start, end):
        print(f'{x},{y}')
        break
