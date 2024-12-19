from functools import cache

example = '''\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
'''

input = example

with open('inputs/day_19.txt', 'r') as input_file:
    input = input_file.read()

raw_patterns, raw_designs = input.split('\n\n')
patterns = tuple(raw_patterns.split(', '))
designs = tuple(raw_designs.splitlines())

@cache
def count_solutions(design, patterns):
    if not design:
        return 0

    possible_solutions = 0
    for pattern in patterns:
        if design.startswith(pattern):
            if len(design) == len(pattern):
                possible_solutions += 1
            else:
                possible_solutions += count_solutions(design[len(pattern):], patterns)

    return possible_solutions

# pt1: 263
print(sum(1 for design in designs if count_solutions(design, patterns) > 0))

# pt2: 723524534506343
print(sum(count_solutions(design, patterns) for design in designs))
