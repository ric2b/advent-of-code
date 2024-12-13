import re

example = '''\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
'''

input = example.splitlines()

with open('inputs/day_13.txt', 'r') as input_file:
    input = input_file.read().splitlines()

raw_machines = [input[i:i+3] for i in range(0, len(input), 4)]

cost_a, cost_b = 3, 1
machines = []
for raw_machine in raw_machines:
    raw_button_a, raw_button_b, raw_prize = raw_machine
    machines.append({
        'button_a': tuple(map(int, re.search(r'Button A: X\+(\d+), Y\+(\d+)', raw_button_a).groups())),
        'button_b': tuple(map(int, re.search(r'Button B: X\+(\d+), Y\+(\d+)', raw_button_b).groups())),
        'prize': tuple(map(int, re.search(r'Prize: X=(\d+), Y=(\d+)', raw_prize).groups())),
    })

costs = []
for machine in machines:
    a_x, a_y = machine['button_a']
    b_x, b_y = machine['button_b']
    p_x, p_y = machine['prize']

    a_n = (p_x*b_y - p_y*b_x) / (a_x*b_y - a_y*b_x)
    b_n = (a_x*p_y - a_y*p_x) / (a_x*b_y - a_y*b_x)

    if not (a_n.is_integer() and b_n.is_integer() and (0 <= a_n <= 100 and 0 <= b_n <= 100)):
        continue

    costs.append(cost_a*a_n + cost_b*b_n)

# pt1: 29023
print(int(sum(costs)))

costs = []
for machine in machines:
    a_x, a_y = machine['button_a']
    b_x, b_y = machine['button_b']
    p_x, p_y = map(lambda x: 10000000000000 + x, machine['prize'])

    a_n = (p_x*b_y - p_y*b_x) / (a_x*b_y - a_y*b_x)
    b_n = (a_x*p_y - a_y*p_x) / (a_x*b_y - a_y*b_x)

    if not (a_n.is_integer() and b_n.is_integer() and (0 <= a_n and 0 <= b_n)):
        continue

    costs.append(cost_a*a_n + cost_b*b_n)

# pt2: 96787395375634
print(int(sum(costs)))
