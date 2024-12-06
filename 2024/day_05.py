import re
from xml.etree.ElementInclude import include

example = '''\
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
'''

# input = example

with open('inputs/day_05.txt', 'r') as input_file:
    input = input_file.read()

dependency_section, print_order_section = input.split('\n\n')

raw_dependencies = dependency_section.splitlines()
raw_print_orders = print_order_section.splitlines()

dependencies = {}
for raw_dependency in raw_dependencies:
    dependency, page = map(int, raw_dependency.split('|'))
    dependencies[page] = dependencies.get(page, []) + [dependency]

print_orders = [list(map(int, raw_print_order.split(','))) for raw_print_order in raw_print_orders]

valid_print_orders = []
for print_order in print_orders:
    valid = True
    printed = set()
    for page in print_order:
        for dependency in dependencies.get(page, []):
            if dependency  in print_order and dependency not in printed:
                valid = False
                break

        printed.add(page)

    if valid:
        valid_print_orders.append(print_order)

def middle(list):
    return list[len(list) // 2]

# pt1: 5509
print(sum(middle(print_order) for print_order in valid_print_orders))
