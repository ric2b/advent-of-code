<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        const raw_instructions = raw_input.split('\n').filter(l => l !== '');
        const raw_matches = raw_instructions.map(op => op.match(/(?<op>\w+) (?<dest>\w) ?(?<src>[a-z])?(?<lit>-?\d+)?/).groups);
        return raw_matches.map(({op, dest, src, lit}) => ({ op, a: dest, b: src || Number(lit) }));
    }

    // [A]
    // add x 12
    // add y 9
    // stack =>  [A] + 9
    //
    // [B]
    // add x 12
    // add y 4
    // stack => [A] + 9, [B] + 4
    //
    // [C]
    // add x 12
    // add y 2
    // stack => [A] + 9, [B] + 4, [C] + 2
    //
    // [D]
    // add x -9
    // add y 5
    // stack => [A] + 9, [B] + 4
    // stop if [C] - 7 != [D]
    //
    // [E]
    // add x -9
    // add y 1
    // stack => [A] + 9
    // stop if [B] - 5 != [E]
    //
    // [F]
    // add x 14
    // add y 6
    // stack => [A] + 9, [F] + 6
    //
    // [G]
    // add x 14
    // add y 11
    // stack => [A] + 9, [F] + 6, [G] + 11
    //
    // [H]
    // add x -10
    // add y 15
    // stack => [A] + 9, [F] + 6
    // stop if [G] + 1 != [H]
    //
    // [I]
    // add x 15
    // add y 7
    // stack => [A] + 9, [F] + 6, [I] + 7
    //
    // [J]
    // add x -2
    // add y 12
    // stack => [A] + 9, [F] + 6
    // stop if [I] + 5 != [J]
    //
    // [K]
    // add x 11
    // add y 15
    // stack => [A] + 9, [F] + 6, [K] + 15
    //
    // [L]
    // add x -15
    // add y 9
    // stack => [A] + 9, [F] + 6
    // stop if [K] != [L]
    //
    // [M]
    // add x -9
    // add y 12
    // stack => [A] + 9,
    //     stop if [F] - 3 != [M]
    //
    // [N]
    // add x -3
    // add y 12
    // stack:
    //     stop if [A] + 6 != [N]
    //
    //     [A] = [A]
    //     [B] = [B]
    //     [C] = [C]
    //     [D] = [C] - 7
    //     [E] = [B] - 5
    //     [F] = [F]
    //     [G] = [G]
    //     [H] = [G] + 1
    //     [I] = [I]
    //     [J] = [I] + 5
    //     [K] = [K]
    //     [L] = [K]
    //     [M] = [F] - 3
    //     [N] = [A] + 6
    //
    //     [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
    const symbols = Array.from('ABCDEFGHIJKLMN');

    function part1(instructions) {
        const constraints = calculate_constraints(instructions);

        const max_values = Array(14);
        symbols.forEach((symbol, i) => {
            if (symbol === constraints[i][0]) {
                max_values[i] = 9 - Math.max(0, ...constraints.map(([s, add]) => symbol === s ? add : 0));
            } else {
                max_values[i] = max_values[symbols.indexOf(constraints[i][0])] + constraints[i][1];
            }
        });
        console.log(max_values);

        // [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
        // [3][9][9][2]  [4]  [9][8][9]  [4][9]  [9][9][6]  [9]
        // const model_number = 39924989499969;
        // const number_digits = Array.from(model_number.toString()).map(Number);
        const registers = run_alu(instructions, Array.from(max_values));
        return registers.z === 0 ? max_values.join('') : 'invalid model number';
    }

    function part2(instructions) {
        const constraints = calculate_constraints(instructions);

        const min_values = Array(14);
        symbols.forEach((symbol, i) => {
            if (symbol === constraints[i][0]) {
                min_values[i] = 1 - Math.min(0, ...constraints.map(([s, add]) => symbol === s ? add : 0));
            } else {
                min_values[i] = min_values[symbols.indexOf(constraints[i][0])] + constraints[i][1];
            }
        });
        console.log(min_values);

        // [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
        // [1][6][8][1]  [1]  [4][1][2]  [1][6]  [1][1][1]  [7]
        // const model_number = 16811412161117;
        // const number_digits = Array.from(model_number.toString()).map(Number);

        const registers = run_alu(instructions, Array.from(min_values));
        return registers.z === 0 ? min_values.join('') : 'invalid model number';
    }

    function calculate_constraints(instructions) {
        const digit_parameters = [];
        for (let i = 0; i < 14; i++) {
            const digit_instructions = instructions.slice(18 * i, 18 * i + 18);
            const z_div = digit_instructions[4].b;
            const x_add = digit_instructions[5].b;
            const y_add = digit_instructions[15].b;

            digit_parameters.push({s: symbols[i], z_div, x_add, y_add});
        }
        console.log(digit_parameters);

        const stack = [], constraints = Array(14);
        digit_parameters.forEach(({z_div, x_add, y_add}, i) => {
            if (z_div === 1) {
                stack.push([i, y_add]);
                constraints[i] = [symbols[i], null];
            } else {
                const [d, adding] = stack.pop();
                constraints[i] = [symbols[d], adding + x_add];
            }
        });
        console.log(constraints);

        return constraints;
    }

    function run_alu(instructions, input) {
        const registers = { w: 0, x: 0, y: 0, z: 0 };

        for (const {op, a, b} of instructions) {
            switch (op) {
                case 'inp':
                    if (input.length === 0) {
                        console.log('no more input');
                        return registers;
                    }
                    registers[a] = input.shift();
                    break;
                case 'add':
                    registers[a] += typeof b === 'number' ? b : registers[b];
                    break;
                case 'mul':
                    registers[a] *= typeof b === 'number' ? b : registers[b];
                    break;
                case 'div':
                    registers[a] = Math.floor(registers[a] / (typeof b === 'number' ? b : registers[b]));
                    break;
                case 'mod':
                    registers[a] %= typeof b === 'number' ? b : registers[b];
                    break;
                case 'eql':
                    registers[a] = (registers[a] === (typeof b === 'number' ? b : registers[b])) ? 1 : 0;
                    break;
            }
            // console.log(registers);
        }

        return registers;
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
