from collections import Counter

example = '''\
3   4
4   3
2   5
1   3
3   9
3   3
'''

# input = example.splitlines()

with open('inputs/day_01.txt', 'r') as input_file:
    input = input_file.readlines()

list_a = [int(line.split()[0]) for line in input]
list_b = [int(line.split()[1]) for line in input]

sorted_pairs = zip(sorted(list_a), sorted(list_b))

distances = [abs(pair[0] - pair[1]) for pair in sorted_pairs]

# pt1: 2904518
print(sum(distances))

counter_b = Counter(list_b)
similarities = [a * counter_b[a] for a in list_a]

# pt2: 18650129
print(sum(similarities))
