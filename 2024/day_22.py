example = '''\
1
10
100
2024
'''

example2 = '''\
1
2
3
2024
'''

# input = example.splitlines()
# input = example2.splitlines()

with open('inputs/day_22.txt', 'r') as input_file:
    input = input_file.readlines()

def secret_sequence(secret):
    while True:
        secret = ((secret * 64) ^ secret) % 16777216
        secret = ((secret // 32) ^ secret) % 16777216
        secret = ((secret * 2048) ^ secret) % 16777216
        yield secret

buyer_seeds = [int(raw_buyer) for raw_buyer in input]
buyer_sequence_generators = [secret_sequence(seed) for seed in buyer_seeds]
buyer_sequences = [[next(generator) for _ in range(2000)] for generator in buyer_sequence_generators]

total_score = sum(buyer_sequence[1999] for buyer_sequence in buyer_sequences)

# pt1: 13429191512
print(total_score)

change_length = 4
price_change_sequences = {}
for m, buyer_sequence in enumerate(buyer_sequences):
    for i, _ in enumerate(buyer_sequence[:-change_length]):
        a, b, c, d, e = map(lambda x: x % 10, buyer_sequence[i:i+change_length+1])
        sequence = (b-a, c-b, d-c, e-d)
        if sequence not in price_change_sequences:
            price_change_sequences[sequence] = [0] * len(buyer_sequences)
        if price_change_sequences[sequence][m] == 0:
            price_change_sequences[sequence][m] = e

# pt2: 1582
print(max(sum(values) for values in price_change_sequences.values()))
