<script>
    function parse(raw_input) {
        return raw_input
            .split('\n')
            .map(s => s.split(' '))
            .map(([command, raw_value]) => [command, Number(raw_value)]);
    }

    function part1(commands) {
        let x = 0, depth = 0;

        commands.forEach(([command, value], _i) => {
            switch (command) {
                case 'forward':
                    x += value; break;
                case 'up':
                    depth -= value; break;
                case 'down':
                    depth += value; break;
            }
        })

        return x * depth;
    }

    function part2(commands) {
        let x = 0, depth = 0, aim = 0;

        commands.forEach(([command, value], _i) => {
            switch (command) {
                case 'forward':
                    x += value;
                    depth += aim * value;
                    break;
                case 'up':
                    aim -= value;
                    break;
                case 'down':
                    aim += value;
                    break;
            }
        })

        return x * depth;
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
