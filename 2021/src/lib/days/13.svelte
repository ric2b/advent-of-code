<p>{part2_result}</p>

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

        const below_fold = dots.filter(p => p[fold['direction']] < fold['value']);
        const above_fold = dots.filter(p => p[fold['direction']] > fold['value']);

        const transformed_above_fold = above_fold.map(({x, y})=> {
            const new_p = {x, y};
            new_p[fold['direction']] = Math.abs(new_p[fold['direction']] - fold['value'] - (max_dim[fold['direction']] - fold['value'])); 
            return new_p;
        });

        const new_dots = below_fold.concat(transformed_above_fold);

        return [...new Set(new_dots.map(JSON.stringify))].length;
    }

    function part2({dots, folds}) {
        let current_dots = dots;

        folds.forEach(fold => {
            const max_dim = { x: Math.max(...current_dots.map(({x, y}) => x)), y: Math.max(...current_dots.map(({x, y}) => y)) };

            const below_fold = current_dots.filter(p => p[fold['direction']] < fold['value']);
            const above_fold = current_dots.filter(p => p[fold['direction']] > fold['value']);

            const transformed_above_fold = above_fold.map(({x, y})=> {
                const new_p = {x, y};
                new_p[fold['direction']] = fold['value'] - Math.abs(new_p[fold['direction']] - fold['value']); 
                return new_p;
            });

            current_dots = below_fold.concat(transformed_above_fold);
        });

        const max_x = Math.max(...current_dots.map(({x, y}) => x)), max_y = Math.max(...current_dots.map(({x, y}) => y));
        const paper = Array.from({ length: max_y + 1 }, () => Array.from({ length: max_x + 1 }, () => '⬛'));
        current_dots.forEach(({x, y}) => paper[y][x] = '⬜');

        return paper.map(l => l.join('')).join('\n');
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>