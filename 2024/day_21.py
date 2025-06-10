from frozendict import frozendict
from functools import cache
from itertools import combinations
import math

example = '''\
029A
980A
179A
456A
379A
'''

input = example.splitlines()

# with open('inputs/day_21.txt', 'r') as input_file:
    # input = input_file.readlines()

# 789
# 456
# 123
# .0A
numeric = {
    '7': {'v': '4', '>': '8'},
    '8': {'v': '5', '<': '7', '>': '9'},
    '9': {'v': '6', '<': '8'},
    '4': {'v': '1', '>': '5', '^': '7'},
    '5': {'v': '2', '<': '4', '>': '6', '^': '8'},
    '6': {'v': '3', '<': '5', '^': '9'},
    '1': {'>': '2', '^': '4'},
    '2': {'v': '0', '<': '1', '>': '3', '^': '5'},
    '3': {'v': 'A', '<': '2', '^': '6'},
    '0': {'^': '2', '>': 'A'},
    'A': {'<': '0', '^': '3'},
}

# .^A
# <v>
# directional = {
#     '^': {'v': 'v', '>': 'A'},
#     'A': {'v': '>', '<': '^'},
#     '<': {'>': 'v'},
#     'v': {'<': '<', '>': '>', '^': '^'},
#     '>': {'<': 'v', '^': 'A'},
# }

directional_pad = frozendict({
    '^': ('v', 'A'),
    'A': ('>', '^'),
    '<': ('v'),
    'v': ('<', '>', '^'),
    '>': ('v', 'A'),
})

@cache
def dfs(graph, start, end):
    for neighbor in graph[start]:
        yield [start, *dfs(graph, neighbor, end)]

breakpoint()
@cache
def cost(direction, depth):
    if depth == 0:
        return 1

    return djikstra(directional, 'A', direction, depth - 1)

# In summary, there are the following keypads:
    # One directional keypad that you are using.
    # Two directional keypads that robots are using.
    # One numeric keypad (on a door) that a robot is using.

def djikstra(graph, start, end, depth):
    distances = { start: 0 }
    previous = { start: None }
    queue = { start }

    while queue:
        current = min(queue, key=distances.get)
        if current == end:
            return distances, previous
        queue.remove(current)

        for direction, neighbor in graph[current].items():
            new_distance = distances[current] + cost(graph, direction, depth - 1)

            if new_distance < distances.get(neighbor, math.inf):
                previous[neighbor] = current
                distances[neighbor] = new_distance
                queue.add(neighbor)

def shortest_path(graph, start, end, depth):
    distances, previous = djikstra(graph, start, end, depth)
    path = []
    current = end
    while previous[current]:
        path.append(current)
        current = previous[current]
    return list(reversed(path))

total_score = 0
for code in input:
    # print(code)
    current_button = 'A'
    sequence = []
    for next_button in code:
        sequence += shortest_path(numeric, current_button, next_button, depth=1)
        current_button = next_button

    print(sequence)
    score = int(code[:-1]) * len(sequence)
    print(score)
    total_score += score

# 68 * 29 = 1972
# 60 * 980 = 58800
# 68 * 179 = 12172
# 64 * 456 = 29184
# 64 * 379 = 24256
# 126384

# pt1:
print(total_score)
