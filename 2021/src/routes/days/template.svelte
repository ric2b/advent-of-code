<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day0x.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    const parse = raw_input => raw_input.split('\n').filter(l => l !== '');

    function part1(input) {
        return JSON.stringify(input);
    }

    function part2(input) {
        return JSON.stringify(input);
    }
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
