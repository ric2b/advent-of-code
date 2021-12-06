<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day01.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    const parse = raw_input => raw_input.split('\n').map(s => Number(s));

    const part1 = depths => depths.filter((value, i) => depths[i+1] > value).length;
    const part2 = depths => depths.filter((value, i) => window(depths, i + 1) > window(depths, i)).length;

    const window = (depths, i) => depths.slice(i, i + 3).reduce((a, b) => a + b, 0);
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
