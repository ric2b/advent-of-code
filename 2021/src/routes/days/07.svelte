<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day07.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    function parse(raw_input) {
        return raw_input.split(',').map(s => Number(s));
    }

    function part1(positions) {
        const fuel_required = (positions, new_x) => positions.map(x => Math.abs(x - new_x)).reduce((a,b) => a + b);

        let optimal_fuel_required = Infinity;
        for (let i = Math.min(...positions); i <= Math.max(...positions); i++) {
            const current_fuel_required = fuel_required(positions, i);
            if (current_fuel_required < optimal_fuel_required) {
                optimal_fuel_required = current_fuel_required;
            }
        }
        return optimal_fuel_required;
    }

    function part2(input) {
        return input;
    }
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
