<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day02.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

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
            console.log({x, depth, aim})
        })

        return x * depth;
    }
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
