<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split('').map(Number));

    // Implement Djikstra
    function part1(risk_map) {
        const  height = risk_map.length, width = risk_map[0].length;
        const neighbours = ({x, y})  => {
            return [[0, -1], [0, 1], [-1, 0], [1, 0]]
                .map(([dx, dy]) => ({ x: x+dx, y: y+dy }))
                .filter(({x, y}) => 0 <= x && x < width && 0 <= y && y < height);
        }

        const costs = new Map(), unvisited = new Set();
        risk_map.forEach((line, i) => {
            line.forEach((_, j) => {
                const s_p = serialize({x: j, y: i});
                unvisited.add(s_p);
                costs.set(s_p, Infinity);
            });
        });
        
        const starting_node = {x: 0, y: 0}, goal = {x: width-1, y: height-1}, s_goal = serialize(goal);
        costs.set(serialize(starting_node), 0);

        while ([...unvisited].some(s_node => costs.get(s_node) < Infinity)) {
            const [s_current_node, current_cost] = [...unvisited].reduce(([min_s_node, min_cost], s_node) => {
                const node_cost = costs.get(s_node);
                return node_cost < min_cost ? [s_node, node_cost] : [min_s_node, min_cost];
            }, [null, Infinity]);
            
            if (s_current_node === s_goal) break;

            neighbours(deserialize(s_current_node))
                .filter(p => unvisited.has(serialize(p)))
                .forEach(p => {
                    const {x, y} = p, s_p = serialize(p);
                    const new_cost = current_cost + risk_map[y][x];

                    if (new_cost < costs.get(s_p)) costs.set(s_p, new_cost);
                });
            unvisited.delete(s_current_node);
        }

        return costs.get(s_goal);
    }

    function part2(input) {
        return 42;
        return JSON.stringify(input);
    }


    const serialize = o => JSON.stringify(o);
    const deserialize = o => JSON.parse(o);

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
