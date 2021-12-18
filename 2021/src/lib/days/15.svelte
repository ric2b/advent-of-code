<script>
    import PriorityQueue from '$lib/prority-queue';
    // const PriorityQueue = require('$lib/prority-queue.js');
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split('').map(Number));

    // Implement Djikstra
    function part1(risk_map) {
        const  height = risk_map.length, width = risk_map[0].length;
        const neighbours = ({x, y})  => {
            return [[0, -1], [0, 1], [-1, 0], [1, 0]]
                .map(([dx, dy]) => ({ x: x+dx, y: y+dy }))
                .filter(({x, y}) => 0 <= x && x < width && 0 <= y && y < height);
        }

        const costs = new Map(), pq = new PriorityQueue((a, b) => costs.get(a) < costs.get(b));
        risk_map.forEach((line, i) => {
            line.forEach((_, j) => {
                const s_p = serialize({x: j, y: i});
                costs.set(s_p, Infinity);
            });
        });
        
        const starting_node = {x: 0, y: 0}, goal = {x: width-1, y: height-1};
        const s_starting_node = serialize(starting_node), s_goal = serialize(goal);
        costs.set(s_starting_node, 0);
        pq.push(s_starting_node);

        while (!pq.isEmpty()) {
            const s_current_node = pq.pop(), current_cost = costs.get(s_current_node);

            if (s_current_node === s_goal) break;

            neighbours(deserialize(s_current_node))
                .forEach(p => {
                    const {x, y} = p, s_p = serialize(p);
                    const new_cost = current_cost + risk_map[y][x];

                    if (new_cost < costs.get(s_p)) {
                        costs.set(s_p, new_cost);
                        pq.push(s_p);
                    }
                });
        }

        return costs.get(s_goal);
    }

    function part2(input) {
        const repeats = 5, height = input.length, width = input[0].length;
        const bigger_input = [];

        for (let m = 0; m < repeats; m++) {
            for (let i = 0; i < height; i++) {
                const line = [];
                for (let n = 0; n < repeats; n++) {
                    line.push(...input[i].map(v => (v + m + n) % 10 + Math.floor((v + m + n) / 10)));
                }
                bigger_input.push(line);
            }
        }
        
        return part1(bigger_input);
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
