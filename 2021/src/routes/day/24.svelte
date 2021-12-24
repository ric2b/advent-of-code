<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        const raw_instructions = raw_input.split('\n').filter(l => l !== '');
        const raw_matches = raw_instructions.map(op => op.match(/(?<op>\w+) (?<dest>\w) ?(?<src>[a-z])?(?<lit>-?\d+)?/).groups);
        return raw_matches.map(({op, dest, src, lit}) => ({ op, a: dest, b: src || Number(lit) }));
    }

    // The ALU is a four-dimensional processing unit: it has integer variables w, x, y, and z. These variables all start with the value 0. The ALU also supports six instructions:
    //
    //     inp a - Read an input value and write it to variable a.
    //     add a b - Add the value of a to the value of b, then store the result in variable a.
    //     mul a b - Multiply the value of a by the value of b, then store the result in variable a.
    //     div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
    // mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
    // eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.

    function part1(instructions) {
        const digit_parameters = [];
        const symbols = Array.from('ABCDEFGHIJKLMN');
        for (let i = 0; i < 14; i++) {
            const digit_instructions = instructions.slice(18*i, 18*i+18);
            const z_div = digit_instructions[4].b;
            const x_add = digit_instructions[5].b;
            const y_add = digit_instructions[15].b;

            digit_parameters.push({ s: symbols[i], z_div, x_add, y_add });
        }
        console.log(digit_parameters);

        // [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
        // [3][9][9][2]  [4]  [9][8][9]  [4][9]  [9][9][6]  [9]
        let model_number = 39924989499969;
        // [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
        // [1][6][8][1]  [1]  [4][1][2]  [1][6]  [1][1][1]  [7]
        // let model_number = 16811412161117;
        const number_digits = Array.from(model_number.toString()).map(Number);

        const registers = run_alu(instructions, number_digits);
        console.log(registers.z)
        return model_number;
    }

    function part2(input) {
        // [A][B][C][C-7][B-5][F][G][G+1][I][I+5][K][K][F-3][A+6]
        // [1][6][8][1]  [1]  [4][1][2]  [1][6]  [1][1][1]  [7]
        let model_number = 16811412161117;
        return model_number;
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
