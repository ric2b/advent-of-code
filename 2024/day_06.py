example = '''\
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
'''

# input = example.splitlines()

with open('inputs/day_06.txt', 'r') as input_file:
    input = input_file.read().splitlines()

height = len(input)
width = len(input[0])

directions = { '^': -1j, 'v': 1j, '<': -1, '>': 1 }

def find_start(map):
    for row in range(height):
        for col in range(width):
            cell = map[row][col]
            if cell in ['^', 'v', '<', '>']:
                speed = directions[cell]

                return ((row * 1j + col), speed)

starting_position, starting_speed = find_start(input)

def simulate_path(current_position, speed, map):
    visited = set()

    while (current_position, speed) not in visited:
        visited.add((current_position, speed))

        potential_position = current_position + speed

        if not ((0 <= potential_position.imag < height) and (0 <= potential_position.real < width)):
            return set([position for position, speed in visited])

        if map[int(potential_position.imag)][int(potential_position.real)] == '#':
            speed *= 1j
        else:
            current_position = potential_position

    return 'loop'


# pt1: 5145
visited_positions = simulate_path(starting_position, starting_speed, input)
print(len(visited_positions))

map = [list(s) for s in input]

looping_possibilities = set()
for position in visited_positions:
    if position == starting_position:
        continue

    map[int(position.imag)][int(position.real)] = '#'

    if simulate_path(starting_position, starting_speed, map) == 'loop':
        looping_possibilities.add(position)

    map[int(position.imag)][int(position.real)] = '.'

# pt2: 1523
print(len(looping_possibilities))
