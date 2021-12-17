<script>
    function parse(raw_input) {
        const lines = raw_input.split('\n').filter(l => l !== '');
        const dots = lines.filter(l => !l.startsWith('fold')).map(l => l.split(',').map(Number)).map(([x, y]) => ({x, y}));
        const folds = lines.filter(l => l.startsWith('fold')).map(l => {
            const [_, direction, raw_value] = l.match(/([xy])=(\d+)/);
            return { direction, value: Number(raw_value) };
        });
        return {dots, folds};
    }

    function part1({dots, folds}) {
        const fold = folds[0];
        const max_dim = { x: Math.max(...dots.map(({x, y}) => x)), y: Math.max(...dots.map(({x, y}) => y)) };
        // const max_height = Math.max(...dots.map(({x, y}) => y));
        // const dots_index = new Set(dots.map(JSON.stringify));

        const below_fold = dots.filter(p => p[fold['direction']] < fold['value']);
        const above_fold = dots.filter(p => p[fold['direction']] > fold['value']);
        // debugger;
        const transformed_above_fold = above_fold.map(({x, y})=> {
            const new_p = {x, y};
            new_p[fold['direction']] = Math.abs(new_p[fold['direction']] - fold['value'] - (max_dim[fold['direction']] - fold['value'])); 
            return new_p;
        });
        
        //   01234567890   
        // 0 #.##..#..#.
        // 1 #...#......
        // 2 ......#...#
        // 3 #...#......
        // 4 .#.#..#.###
        // 5 ...........
        // 6 ...........

        //   01234567890   
        // 0 #.##..#..#.
        // 1 #...#......
        // 2 ......#...#
        // 3 #...#......
        // 4 .#.#..#.###
        // 5 ...........
        // 6 ...........

        [
            {"x":0,"y":3},
            {"x":10,"y":4},
            {"x":6,"y":0},
            {"x":4,"y":1},
            {"x":3,"y":4},
            {"x":3,"y":0},
            {"x":8,"y":4},
            {"x":9,"y":0},
            {"x":6,"y":4},
            {"x":0,"y":0},
            {"x":9,"y":4},
            {"x":4,"y":3},
            {"x":6,"y":2},
            {"x":0,"y":1},
            {"x":10,"y":2},
            {"x":1,"y":4},
            {"x":2,"y":0},
            {"x":8,"y":4}
        ]
        const new_dots = below_fold.concat(transformed_above_fold);

        return [...new Set(new_dots.map(JSON.stringify))].length;
    }

    function part2(input) {
        return JSON.stringify(input);
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
