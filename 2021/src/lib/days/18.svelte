<script>
    function parse(raw_input) {
        const raw_numbers = raw_input.split('\n').filter(l => l !== '');
        return raw_numbers.map(JSON.parse);
    }

    function part1(numbers) {
        return sf_magnitude(numbers.reduce(sf_add));
    }

    function part2(numbers) {
        let max_magnitude = -Infinity;

        for (let i = 0; i < numbers.length; i++) {
            for (let j = i; j < numbers.length; j++) {
                const a = numbers[i], b = numbers[j];
                max_magnitude = Math.max(max_magnitude, sf_magnitude(sf_add(a, b)));
                max_magnitude = Math.max(max_magnitude, sf_magnitude(sf_add(b, a)));
            }
        }

        return max_magnitude;
    }

    function sf_add(a, b) {
        const tree = [a, b];
        const pathed_tree = build_pathed_tree(tree);
        let adjacency;
        do {
            adjacency = build_adjacency(pathed_tree);
            // console.log(JSON.stringify(simplify_tree(pathed_tree)));
        } while (sf_explode(pathed_tree, adjacency) || sf_split(pathed_tree, adjacency));

        return simplify_tree(pathed_tree);
    }

    function sf_explode(pathed_tree, adjacency, path = '') {
        const node = tree_node(pathed_tree, path);

        if(!Array.isArray(node.v)) return false;

        if (path.length === 4) {
            const [a, b] = node.v; // exploding elements are always regular numbers
            const i = adjacency.findIndex(path => path === a.p);
            const path_prev = adjacency[i-1], path_next = adjacency[i+2];

            if (path_prev) tree_node(pathed_tree, path_prev).v += a.v;
            if (path_next) tree_node(pathed_tree, path_next).v += b.v;
            node.v = 0

            return true;
        }

        return sf_explode(pathed_tree, adjacency, path + 'L') || sf_explode(pathed_tree, adjacency, path + 'R')
    }

    function sf_split(pathed_tree, adjacency, path = '') {
        const node = tree_node(pathed_tree, path);

        if (Array.isArray(node.v)) {
            return sf_split(pathed_tree, adjacency, path + 'L') || sf_split(pathed_tree, adjacency, path + 'R');
        }

        if (node.v < 10) return false;

        node.v = [{ p: node.p + 'L', v: Math.floor(node.v/2) }, { p: node.p + 'R', v: Math.ceil(node.v/2) }];
        return true;
    }

    function tree_node(tree, path) {
        switch (path[0]) {
            case 'L':
                return tree_node(tree.v[0], path.slice(1));
            case 'R':
                return tree_node(tree.v[1], path.slice(1));
            default:
                return tree;
        }
    }

    function build_pathed_tree(sf_number, path = '') {
        const value = Array.isArray(sf_number) ?
            [build_pathed_tree(sf_number[0], path + 'L'), build_pathed_tree(sf_number[1], path + 'R')]
            : sf_number
        return { p: path, v: value };
    }

    function simplify_tree(pathed_tree) {
        return pathed_tree.v.map(c => Array.isArray(c.v) ? simplify_tree(c) : c.v);
    }

    function build_adjacency(tree, adjacency = []) {
        if (Array.isArray(tree.v)) {
            tree.v.forEach(c => build_adjacency(c, adjacency));
        } else {
            adjacency.push(tree.p);
        }
        return adjacency;
    }

    function sf_magnitude(sf_number) {
        return sf_number
            .map(sf_n => Array.isArray(sf_n) ? sf_magnitude(sf_n) : sf_n)
            .reduce((a, b) => 3*a + 2*b);
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
