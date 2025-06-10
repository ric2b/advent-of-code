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

example3 = '''\
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^
'''

# input = example
# input = example2
input = example3

# with open('inputs/day_15.txt', 'r') as input_file:
    # input = input_file.read()

raw_map, raw_movements = input.split('\n\n')

def load_map(raw_map):
    return [list(row) for row in raw_map.split('\n')]

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

movements = [parse_movement(movement) for movement in ''.join([row.strip() for row in raw_movements])]

def find_robot(map):
    for i, row in enumerate(map):
        if '@' in row:
            return i, row.index('@')

def move(map, location, movement):
    y, x = location
    dy, dx = movement
    entity = map[y][x]

    if entity != '@' and entity != 'O':
        return False

    new_y = y + dy
    new_x = x + dx

    if map[new_y][new_x] == '#' or new_y < 0 or new_y >= len(map) or new_x < 0 or new_x >= len(map[0]):
        return False

    if map[new_y][new_x] == 'O':
        if not move(map, (new_y, new_x), movement):
            return False

    map[y][x] = '.'
    map[new_y][new_x] = entity
    return True

map = load_map(raw_map)
for movement in movements:
    move(map, find_robot(map), movement)

box_gps = []
for i, row in enumerate(map):
    for j, entity in enumerate(row):
        if entity == 'O':
            box_gps.append((100 * i + j))


# part 1: 1360570
print(sum(box_gps))

def widen_map(map):
    new_map = []
    for row in map:
        new_row = []
        for cell in row:
            if cell == '#':
                new_row.extend(['#', '#'])
            elif cell == 'O':
                new_row.extend(['[', ']'])
            elif cell == '.':
                new_row.extend(['.', '.'])
            elif cell == '@':
                new_row.extend(['@', '.'])
            else:
                raise ValueError(f'Invalid cell: {cell}')
        new_map.append(new_row)
    return new_map

map = widen_map(load_map(raw_map))

for row in map:
    print(''.join(row))

def invert_movement(movement):
    return -movement[0], -movement[1]

def wide_move(map, location, movement, test=False):
    y, x = location
    dy, dx = movement
    entity = map[y][x]

    if entity == '.':
        return True
    if entity == '#':
        return False

    new_y = y + dy
    new_x = x + dx

    if dy == 0:
        if entity == '[':
            if not wide_move(map, (new_y, new_x + 1), movement):
                return False
            map[y][x], map[y][x + 1], map[y][x + 2] = '.', '[', ']'
            return True
        if entity == ']':
            if not wide_move(map, (new_y, new_x - 1), movement):
                return False
            map[y][x - 2], map[y][x - 1], map[y][x] = '[', ']', '.'
            return True

    if entity == '[':
        moved_left, moved_right = wide_move(map, (new_y, new_x), movement), wide_move(map, (new_y, new_x + 1), movement)
        if not moved_left or not moved_right:
            if not moved_left and moved_right:
                wide_move(map, (new_y, new_x + 1), invert_movement(movement))
            if moved_left and not moved_right:
                wide_move(map, (new_y, new_x), invert_movement(movement))
            return False

        map[y][x], map[y][x + 1] = '.', '.'
        map[new_y][new_x], map[new_y][new_x + 1] = '[', ']'
        return True

    if entity == ']':
        moved_left, moved_right = wide_move(map, (new_y, new_x - 1), movement), wide_move(map, (new_y, new_x), movement)

        if not moved_left or not moved_right:
            if not moved_left and moved_right:
                wide_move(map, (new_y, new_x - 1), invert_movement(movement))
            if moved_left and not moved_right:
                wide_move(map, (new_y, new_x), invert_movement(movement))
            return False

        map[y][x], map[y][x - 1] = '.', '.'
        map[new_y][new_x - 1], map[new_y][new_x] = '[', ']'
        return True

    if not wide_move(map, (new_y, new_x), movement):
        return False

    map[y][x] = '.'
    map[new_y][new_x] = entity
    return True

for movement in movements:
    wide_move(map, find_robot(map), movement)

for row in map:
    print(''.join(row))

box_gps = []
for i, row in enumerate(map):
    for j, entity in enumerate(row):
        if entity == '[':
            box_gps.append((100 * i + j))

# part 2:
print(sum(box_gps))
