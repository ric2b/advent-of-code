<p class="aoc_yellow">Part 1: {part1_result}</p>
<p class="aoc_yellow">Part 2: {part2_result}</p>

<script>
    export let raw_input;

    $: input = parse(raw_input);
	$: part1_result = part1(input);
    $: part2_result = part2(input);

    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split('').map(Number));

    function part1(cave_map) {
        const risk_level = (cave_map, {x, y}) => cave_map[y][x] + 1;
        return find_low_points(cave_map).map(p => risk_level(cave_map, p)).reduce((a, b) => a + b);
    }

    function part2(cave_map) {
        const serialize = JSON.stringify, deserialize = JSON.parse;

        const basins =  find_low_points(cave_map).map(low_point => {
            const visited = new Set(), queue = [low_point];

            for (let next_point = queue.pop(); next_point !== undefined; next_point = queue.pop()) {
                visited.add(serialize(next_point));
                neighbours(cave_map, next_point)
                    .filter(p => get_height(cave_map, p) < 9 && !visited.has(serialize(p)))
                    .map(p => queue.push(p));
            }

            return Array.from(visited).map(deserialize);
        })

        return basins.map(basin => basin.length).sort((a, b) => b - a).slice(0, 3).reduce((a, b) => a * b);
    }

    function neighbours(cave_map, {x, y}) {
        return [[0, -1], [0, 1], [-1, 0], [1, 0]]
            .map(([dx, dy]) => ({ x: x+dx, y: y+dy }))
            .filter(({x, y}) => 0 <= x && x < cave_map[0].length && 0 <= y && y < cave_map.length);
    }

    function find_low_points(cave_map) {
        return cave_map.flatMap((xs, y) => {
            return xs.map((_h, x) => ({x: x, y: y}))
                .filter(p => neighbours(cave_map, p).every(n_p => get_height(cave_map, p) < get_height(cave_map, n_p)))
        });
    }

    const get_height = (cave_map, {x, y}) => cave_map[y]?.[x];
</script>
