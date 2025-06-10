example = '''\
2333133121414131402
'''

input = example.splitlines()

with open('inputs/day_09.txt', 'r') as input_file:
    input = input_file.read().splitlines()

def read_image(image):
    file_id = 0
    occupied = True
    disk = []
    for digit in image:
        if occupied:
            id = file_id
            file_id += 1
        else:
            id = '.'

        disk.extend([id] * int(digit))
        occupied = not occupied
    return disk

def compact_blocks(disk):
    free_pointer = disk.index('.')

    for file_pointer in reversed(range(len(disk))):
        if disk[file_pointer] == '.':
            continue

        while free_pointer < len(disk) and disk[free_pointer] != '.':
            free_pointer += 1

        if file_pointer < free_pointer:
            break

        disk[free_pointer], disk[file_pointer] = disk[file_pointer], '.'

def checksum(disk):
    return sum(i * int(c) for i, c in enumerate(disk) if c != '.')

disk = read_image(input[0])
compact_blocks(disk)
# pt1: 6337921897505
print(checksum(disk))

def read_files(image):
    is_file = True
    i = 0
    files = []
    for raw_length in image:
        length = int(raw_length)
        if is_file:
            files.append((i, int(length)))

        i += int(length)
        is_file = not is_file
    return files

def find_free_space(disk, target_length):
    start = None
    for i in range(len(disk)):
        if disk[i] != '.':
            start = None
            continue

        if disk[i] == '.':
            if not start:
                start = i

            if (i - start) + 1 == target_length:
                return start
    return None

def compact_files(disk, files):
    for start, length in reversed(files):
        free_space_index = find_free_space(disk, length)
        if free_space_index and free_space_index < start:
            disk[free_space_index:free_space_index + length] = disk[start:start + length]
            disk[start:start + length] = ['.'] * length

disk = read_image(input[0])
compact_files(disk, read_files(input[0]))

# pt2: 6362722604045
print(checksum(disk))
