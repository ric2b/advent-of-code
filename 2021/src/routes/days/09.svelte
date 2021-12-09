<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day09.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split('').map(Number));

    function part1(cave_map) {
        const risk_level = (cave_map, {x, y}) => cave_map[y][x] + 1;

        const low_points = [];
        cave_map.forEach((xs, y) => {
            xs.forEach((height, x) => {
                const n_deltas = [[0, -1], [0, 1], [-1, 0], [1, 0]];
                const n_heights = n_deltas.map(([dx, dy]) => cave_map[y+dy]?.[x+dx]).filter(h => h !== undefined);

                if (n_heights.every(n_height => height < n_height)) low_points.push({x, y});
            });
        });
        return low_points.map(p => risk_level(cave_map, p)).reduce((a, b) => a + b);
    }

    function part2(input) {
        return JSON.stringify(input);
    }
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
