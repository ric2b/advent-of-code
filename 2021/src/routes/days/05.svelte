<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day05.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    function parse(raw_input) {
        return raw_input.split('\n')
            .filter(l => l !== '')
            .map(raw_line => raw_line.split(' -> '))
            .map((raw_points) => raw_points.map(raw_point => raw_point.split(',').map(Number)));
    }

    function part1(lines) {
        const point_counter = new Map();

        lines.filter(([[x1, y1], [x2, y2]]) => x1 === x2 || y1 === y2)
            .flatMap(line_to_points)
            .map(String)
            .forEach(p => point_counter.set(p, (point_counter.get(p) || 0) + 1));

        return [...point_counter.values()].filter(c => c > 1).length;
    }

    function part2(lines) {
        const point_counter = new Map();

        lines
            .flatMap(line_to_points)
            .map(String)
            .forEach(p => point_counter.set(p, (point_counter.get(p) || 0) + 1));

        return [...point_counter.values()].filter(c => c > 1).length;
    }

    function line_to_points([p1, p2]) {
        const [x1, y1] = p1, [x2, y2] = p2;

        if (x1 === x2) return range(y1, y2).map(y => [x1, y]);
        if (y1 === y2) return range(x1, x2).map(x => [x, y1]);

        if (Math.abs(x2-x1) === Math.abs(y2-y1)) {
            const xs = range(x1, x2), ys = range(y1, y2);
            return xs.map((x, i) => [x, ys[i]]);
        }

        return [];
    }

    const range = (n, m) => Array.from({length: Math.abs(m - n) + 1}, (_, i) => n + (n < m ? i : -i));
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
