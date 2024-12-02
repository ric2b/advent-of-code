from collections import Counter

example = '''\
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
'''

example2 = '''\
48 46 47 49 51 54 56
1 1 2 3 4 5
1 2 3 4 5 5
5 1 2 3 4 5
1 4 3 2 1
1 6 7 8 9
1 2 3 4 3
9 8 7 6 7
'''

# input = example.splitlines()
# input = example2.splitlines()

with open('inputs/day_02.txt', 'r') as input_file:
    input = input_file.readlines()

def find_fault_location(report):
    direction = None
    for i, (previous, current) in enumerate(zip(report, report[1:])):
        if previous == current:
            return i
        if not (1 <= abs(current - previous) <= 3):
            return i
        if direction == 'positive' and current < previous:
            return i
        if direction == 'negative' and current > previous:
            return i

        if not direction:
            direction = 'positive' if current >= previous else 'negative'

    return None

reports = [[int(level) for level in raw_report.split()] for raw_report in input]
safe_reports = [report for report in reports if find_fault_location(report) is None]

# pt1: 213
print(len(safe_reports))

def is_safe_dampened(report):
    fault_location = find_fault_location(report)
    if fault_location is None:
        return True

    return find_fault_location(report[:fault_location - 1] + report[fault_location:]) is None \
        or find_fault_location(report[:fault_location] + report[fault_location + 1:]) is None \
        or find_fault_location(report[:fault_location + 1] + report[fault_location + 2:]) is None

safe_reports = [report for report in reports if is_safe_dampened(report)]
# unsafe_reports = [report for report in reports if not is_safe_dampened(report)]

# pt2: 285
print(len(safe_reports))
