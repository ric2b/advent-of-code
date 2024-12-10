from collections import Counter
from functools import cache

example = '''\
125 17
'''

# input = example.splitlines()

with open('inputs/day_11.txt', 'r') as input_file:
    input = input_file.read().splitlines()

original_stones = [int(stone) for stone in input[0].split()]

@cache
def transform(stone):
    new_stones = []
    if stone == 0:
        new_stones.append(1)
    elif len(str(stone)) % 2 == 0:
        digits = str(stone)
        first_half, second_half = digits[:len(digits) // 2], digits[len(digits) // 2:]
        new_stones.append(int(first_half))
        new_stones.append(int(second_half))
    else:
        new_stones.append(stone * 2024)
    return new_stones

def blink(stones):
    new_stones = []
    for stone in stones:
        new_stones.extend(transform(stone))
    return new_stones

# pt1: 229043
stones = original_stones
for _ in range(25):
    stones = blink(stones)

print(len(stones))

# pt2:
def blink_many(stones, n = 75):
    current_counts = dict(Counter(original_stones))

    for _blink in range(n):
        next_counts = {}
        for stone, count in current_counts.items():
            for new_stone in transform(stone):
                next_counts[new_stone] = next_counts.get(new_stone, 0) + count
        current_counts = next_counts

    return sum(current_counts.values())

# 272673043446478
print(blink_many(original_stones))
