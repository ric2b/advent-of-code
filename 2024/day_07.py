from functools import cache

example = '''\
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
'''

# input = example.splitlines()

with open('inputs/day_07.txt', 'r') as input_file:
    input = input_file.readlines()

equations = []
for raw_equation in input:
    raw_result, raw_numbers = raw_equation.split(':')
    result = int(raw_result)
    raw_numbers = [int(raw_number) for raw_number in raw_numbers.strip().split()]
    equations.append((result, raw_numbers))

@cache
def operator_combinations(n, allowed_operators=('+', '*')):
    if n == 1:
        return [[allowed_operator] for allowed_operator in allowed_operators]

    combinations = []
    for sub_combination in operator_combinations(n - 1, allowed_operators=allowed_operators):
        for allowed_operator in allowed_operators:
            combinations.append([allowed_operator] + sub_combination)

    return combinations

def solve_equation(incomplete_equation, allowed_operators=('+', '*')):
    expected_result, numbers = incomplete_equation

    valid_operators = []
    for possible_operators in operator_combinations(len(numbers) - 1, allowed_operators=allowed_operators):
        possible_result = numbers[0]
        for operator, number in zip(possible_operators, numbers[1:]):
            match operator:
                case '+':
                    possible_result += number
                case '*':
                    possible_result *= number
                case '|':
                    possible_result = int(str(possible_result) + str(number))

        if possible_result == expected_result:
            valid_operators.append(possible_operators)

    return valid_operators

valid_equations_pt1 = [equation for equation in equations if solve_equation(equation)]

# pt1: 2941973819040
print(sum(result for result, numbers in valid_equations_pt1))

valid_equations_pt2 = [equation for equation in equations if solve_equation(equation, allowed_operators=('+', '*', '|'))]

# pt2: 249943041417600
print(sum(result for result, numbers in valid_equations_pt2))
