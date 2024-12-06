import re

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
    input = input_file.readlines()

height = len(input)
width = len(input[0])

speed = (0, 0)
visited_positions = set()

for row in range(height):
    for col in range(width):
        if input[row][col] in ['^', 'v', '<', '>']:
            visited_positions.add((row, col))

            match input[row][col]:
                case '^':
                    speed = (-1, 0)
                case 'v':
                    speed = (1, 0)
                case '<':
                    speed = (0, -1)
                case '>':
                    speed = (0, 1)

current_position = visited_positions.pop()

while True:
    visited_positions.add(current_position)
    print(current_position)

    potential_position = current_position[0] + speed[0], current_position[1] + speed[1]

    if  potential_position[0] >= height or potential_position[0] < 0 or potential_position[1] >= width or potential_position[1] < 0:
        break

    if input[potential_position[0]][potential_position[1]] == '#':
        match speed:
            case (-1, 0):
                speed = (0, 1)
            case (1, 0):
                speed = (0, -1)
            case (0, -1):
                speed = (-1, 0)
            case (0, 1):
                speed = (1, 0)
        current_position = current_position[0] + speed[0], current_position[1] + speed[1]
    else:
        current_position = potential_position

# pt1:
print(len(visited_positions))
