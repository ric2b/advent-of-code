import re

example = '''\
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
'''

# input = example.splitlines()
# height = 7
# width = 11

with open('inputs/day_14.txt', 'r') as input_file:
    input = input_file.read().splitlines()
height = 103
width = 101

half_width = width // 2
half_height = height // 2

robots = []
for raw_robot in input:
    px, py, vx, vy = map(int, re.search(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)', raw_robot).groups())
    robots.append({ 'px': px, 'py': py, 'vx': vx, 'vy': vy })

ticks = 100
quadrants = { 0: [], 1: [], 2: [], 3: [], None: [] }

for robot in robots:
    final_x = (robot['px'] + ticks * robot['vx']) % width
    final_y = (robot['py'] + ticks * robot['vy']) % height

    if final_x > half_width and final_y > half_height:
        quadrants[0].append(robot)
    elif final_x < half_width and final_y > half_height:
        quadrants[1].append(robot)
    elif final_x < half_width and final_y < half_height:
        quadrants[2].append(robot)
    elif final_x > half_width and final_y < half_height:
        quadrants[3].append(robot)
    else:
        quadrants[None].append(robot)

safety_factor = len(quadrants[0]) * len(quadrants[1]) * len(quadrants[2]) * len(quadrants[3])
# pt1: 229839456
print(safety_factor)

def print_grid(positions):
    for j in range(height):
        for i in range(width):
            if (i, j) in positions:
                print('#', end='')
            else:
                print('.', end='')
        print()

def positions_at_tick(robots, tick):
    positions = []
    for robot in robots:
        positions.append(((robot['px'] + tick * robot['vx']) % width, (robot['py'] + tick * robot['vy']) % height))
    return positions

for i in range(10000):
    for robot in robots:
        final_x = (robot['px'] + ticks * robot['vx']) % width
        final_y = (robot['py'] + ticks * robot['vy']) % height

    if len({position for position in positions_at_tick(robots, i)}) == len(robots):
        print(i)
        print_grid(positions_at_tick(robots, i))
# part 2: 7138
