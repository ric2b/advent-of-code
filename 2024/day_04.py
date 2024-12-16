import re

example = '''\
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
'''

input = example.splitlines()

with open('inputs/day_04.txt', 'r') as input_file:
    input = input_file.read().splitlines()

def xmas_count(line):
    return len(re.findall(r'XMAS', line) + re.findall(r'SAMX', line))

def transpose(matrix):
    return [''.join(row) for row in zip(*matrix)]

def get_diagonals(matrix):
  diagonals = []
  rows, cols = len(matrix), len(matrix[0])

  # top-left to bottom-right
  for d in range(-(rows - 1), cols):
      diagonal = ''.join([matrix[i][i - d] for i in range(max(0, d), min(rows, cols + d))])
      diagonals.append(diagonal)

  # top-right to bottom-left
  for d in range(rows + cols - 1):
      diagonal = ''.join([matrix[i][d - i] for i in range(max(0, d - cols + 1), min(rows, d + 1))])
      diagonals.append(diagonal)

  return diagonals

total = 0
for line in input:
    total += xmas_count(line)

for line in transpose(input):
    total += xmas_count(line)

for line in get_diagonals(input):
    total += xmas_count(line)

# pt1: 2644
print(total)


def is_mas(line):
    return len(re.findall(r'MAS', line) + re.findall(r'SAM', line))

total = 0
for i in range(len(input)):
    for j in range(len(input[0])):
        if i > 0 and i < len(input) - 1 and j > 0 and j < len(input[0]) - 1:
            diagonal = ''.join([input[i-1][j-1], input[i][j], input[i+1][j+1]])
            neg_diagonal = ''.join([input[i+1][j-1], input[i][j], input[i-1][j+1]])
            if is_mas(diagonal) and is_mas(neg_diagonal):
                total += 1

# pt2: 1952
print(total)
