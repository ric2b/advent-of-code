import re

example = '''\
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
'''

example_2 = '''\
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
'''

# input = example
# input = example_2

with open('inputs/day_03.txt', 'r') as input_file:
    input = input_file.read()

matches_mul = list(re.finditer(r'mul\((\d+),(\d+)\)', input))
muls = [match.groups() for match in matches_mul]

# pt1: 159892596
print(sum([int(a) * int(b) for a, b in muls]))

matches_do_dont = re.finditer(r'do\(\)|don\'t\(\)', input)

enabled_spans = [[0, None]]
for match in matches_do_dont:
    if match.group() == "do()" and enabled_spans[-1][1] is not None:
        enabled_spans.append([match.end(), None])
    elif match.group() == "don't()" and enabled_spans[-1][1] is None:
        enabled_spans[-1][1] = match.start()
enabled_spans[-1][1] = len(input)

current_span_index = 0
enabled_muls = []
for match in matches_mul:
    while match.start() > enabled_spans[current_span_index][1]:
        current_span_index += 1

    if enabled_spans[current_span_index][0] <= match.start() <= enabled_spans[current_span_index][1]:
        enabled_muls.append(match.groups())

# pt2: 92626942
print(sum([int(a) * int(b) for a, b in enabled_muls]))
