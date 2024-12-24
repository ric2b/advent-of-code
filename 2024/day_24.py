import re

example = '''\
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
'''

example2 = '''\
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
'''

# input = example
# input = example2

# with open('inputs/day_24 copy.txt', 'r') as input_file:
    # input = input_file.read()

with open('inputs/day_24.txt', 'r') as input_file:
    input = input_file.read()

raw_wires, raw_gates = input.split('\n\n')

wires = { wire: bool(int(value)) for wire, value in (line.split(': ') for line in raw_wires.splitlines()) }
gates = []
for raw_gate in raw_gates.splitlines():
    in_a, gate_type, in_b, out = re.search(r'(\w{3}) (AND|OR|XOR) (\w{3}) -> (\w{3})', raw_gate).groups()
    gates.append({ 'type': gate_type, 'in': [in_a, in_b], 'out': out})

output_dependencies = { gate['out']: gate['in'] for gate in gates }
def topological_sort(output_dependencies):
    sorted_outputs = []
    visited = set()

    def visit(output):
        if output in visited:
            return
        visited.add(output)
        for dependency in output_dependencies.get(output, []):
            visit(dependency)
        sorted_outputs.append(output)

    for gate in output_dependencies.keys():
        visit(gate)

    return sorted_outputs

sorted_gate_outputs = topological_sort(output_dependencies)
sorted_gates = sorted(gates, key=lambda g: sorted_gate_outputs.index(g['out']))
# print(sorted_gates)

def simulate(initial_wires, gates):
    wires = initial_wires.copy()
    for gate in sorted_gates:
        a, b = wires[gate['in'][0]], wires[gate['in'][1]]

        if gate['type'] == 'AND':
            out = a & b
        elif gate['type'] == 'OR':
            out = a | b
        elif gate['type'] == 'XOR':
            out = a ^ b
        else:
            raise ValueError(f'Unknown gate type: {gate["type"]}')
        wires[gate['out']] = out
    return wires

def output(wires):
    raw_output = [wires[wire] for wire in reversed(sorted(wire for wire in wires.keys() if wire.startswith('z')))]
    return int(''.join(str(int(bit)) for bit in raw_output), 2)

# pt1: 51410244478064
print(output(simulate(wires, sorted_gates)))

# pt2: gst,khg,nhn,tvb,vdc,z12,z21,z33
import random

for _ in range(2**16):
    # break

    x = random.randint(0, 2**45)
    y = random.randint(0, 2**45)

    wires = {}
    x_bits = [0] * 45
    y_bits = [0] * 45
    for i, digit in enumerate(reversed(bin(x)[2:])):
        x_bits[i] = int(digit)
    for i, digit in enumerate(reversed(bin(y)[2:])):
        y_bits[i] = int(digit)

    for i in range(45):
        wires[f'x{str(i).rjust(2, '0')}'] = bool(x_bits[i])
        wires[f'y{str(i).rjust(2, '0')}'] = bool(y_bits[i])

    z = output(simulate(wires, sorted_gates))
    if x + y != z:
        print(f'{x} + {y} != {z}')
        print(f'{x + y} != {z}')
        print()
        print('44444333333333322222222221111111111          ')
        print('432109876543210987654321098765432109876543210')
        print(bin(x + y)[2:].rjust(45, '0'))
        print(bin(z)[2:].rjust(45, '0'))
        break

nodes = set()
for output, inputs in output_dependencies.items():
    nodes.update([output, *inputs])

with open('day_24.mmd', 'w') as output_file:
    output_file.write('flowchart TD\n')

    for i, gate in enumerate(sorted_gates):
        in_a, in_b, out = gate['in'][0], gate['in'][1], gate['out']
        gate_type = gate['type']

        output_file.write(f'{in_a} & {in_b} --> {out}[{gate_type} {out}]\n')
