import re

example = '''\
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
'''

example2 = '''\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
'''

# input = example
# input = example2

with open('inputs/day_15.txt', 'r') as input_file:
    input = input_file.read()

raw_map, raw_movements = input.split('\n\n')
map = [list(row) for row in raw_map.split('\n')]

def parse_movement(movement):
    match movement:
        case '^':
            return -1, 0
        case 'v':
            return 1, 0
        case '<':
            return 0, -1
        case '>':
            return 0, 1
        case _:
            raise ValueError(f'Invalid movement: {movement}')

movements = [parse_movement(movement) for movement in ''.join([x.strip() for x in raw_movements])]

def find_robot(map):
    for i, row in enumerate(map):
        if '@' in row:
            return i, row.index('@')

def move(map, location, movement):
    x, y = location
    dx, dy = movement
    entity = map[x][y]

    if entity != '@' and entity != 'O':
        return False

    new_x = x + dx
    new_y = y + dy

    if map[new_x][new_y] == '#' or new_x < 0 or new_x >= len(map) or new_y < 0 or new_y >= len(map[0]):
        return False

    if map[new_x][new_y] == 'O':
        if not move(map, (new_x, new_y), movement):
            return False

    map[x][y] = '.'
    map[new_x][new_y] = entity
    return True

for movement in movements:
    move(map, find_robot(map), movement)

# for row in map:
    # print(''.join(row))


box_gps = []
for i, row in enumerate(map):
    for j, entity in enumerate(row):
        if entity == 'O':
            box_gps.append((100 * i + j))


# part 1:
print(sum(box_gps))
