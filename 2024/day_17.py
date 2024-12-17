import re

example = '''\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
'''

example2 = '''\
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
'''

# input = example
# input = example2

with open('inputs/day_17.txt', 'r') as input_file:
    input = input_file.read()

raw_registers, raw_program = input.split('\n\n')

initial_registers = {}
for raw_register in raw_registers.splitlines():
    register, value = re.search(r'Register (\w): (\d+)', raw_register).groups()
    initial_registers[register[-1]] = int(value)

program = list(map(int, raw_program.split(': ')[1].split(',')))

def combo_operand(registers, operand):
    match operand:
        case num if num in range(0, 4): return operand
        case 4: return registers['A']
        case 5: return registers['B']
        case 6: return registers['C']
        case 7: return None

def run(registers, program):
    instruction_pointer = 0

    while instruction_pointer < len(program):
        opcode, raw_operand = program[instruction_pointer:instruction_pointer + 2]
        operand = combo_operand(registers, raw_operand)

        match opcode:
            case 0: # adv
                registers['A'] = (registers['A'] // 2 ** operand)
            case 1: # bxl
                registers['B'] ^= raw_operand
            case 2: # bst
                registers['B'] = operand % 8
            case 3: # jnz
                if registers['A'] != 0:
                    instruction_pointer = raw_operand
                    continue
            case 4: # bxc
                registers['B'] ^= registers['C']
            case 5: # out
                yield operand % 8
            case 6: # bdv
                registers['B'] = (registers['A'] // 2 ** operand) % 8
            case 7: # cdv
                registers['C'] = (registers['A'] // 2 ** operand) % 8

        instruction_pointer += 2

# pt1: 3,7,1,7,2,1,0,6,3.
print(','.join(map(str, run(initial_registers.copy(), program))))

# pt2: 37221334433268
def find_solutions(program, target_output):
    if not target_output:
        yield 0
    for sub_solution in find_solutions(program, target_output[1:]):
        for i in range(8):
            candidate = sub_solution * 8 + i
            registers = { 'A': candidate, 'B': 0, 'C': 0 }
            if list(run(registers, program)) == target_output:
                yield candidate

print(next(find_solutions(program, program)))
