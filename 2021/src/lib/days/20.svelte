<!-- <div class="grid">
    {#each final_image_render as line}
        {line.join('')}<br>
    {/each}
</div> -->


<script>
    // let final_image_render;

    function parse(raw_input) {
        const lines = raw_input.split('\n');
        return { algorithm: lines[0].split(''), image: lines.slice(1).filter(l => l !== '').map(l => l.split('')) }
    }

    function part1(input) {
        return enhance(input, 2);
    }

    function part2(input) {
        return enhance(input, 50);
    }

    function enhance({ algorithm, image }, n) {
        let background = '.';
        let opposite_from_background = new Set();

        image.forEach((line, i) => line.forEach((c, j) => {
            if (c !== background) opposite_from_background.add(JSON.stringify({x: j, y: i}));
        }));

        for (let steps = 0; steps < n; steps++) {
            const {xs, ys} =  image_edges(opposite_from_background);
            const new_background = background === '.' ? algorithm[0] : algorithm[511];
            // const new_background = background;
            const new_opposite_from_background = new Set();

            for (let y = ys[0]; y <= ys[1]; y++) {
                for (let x = xs[0]; x <= xs[1]; x++) {
                    const square_bits = square({x, y}).map(p => {
                        const is_opposite_from_background = opposite_from_background.has(JSON.stringify(p));
                        return background === '.' ? is_opposite_from_background : !is_opposite_from_background;
                    }).map(is_lit => is_lit ? '1' : '0').join('');

                    const new_pixel_state = algorithm[parseInt(square_bits, 2)];
                    if (new_pixel_state !== new_background) new_opposite_from_background.add(JSON.stringify({x, y}));
                }
            }

            background = new_background;
            opposite_from_background = new_opposite_from_background;
        }

        const {xs, ys} = image_edges(opposite_from_background);
        const final_image = []
        for (let y = ys[0]; y <= ys[1]; y++) {
            const line = []
            final_image.push(line);
            for (let x = xs[0]; x <= xs[1]; x++) {
                const is_opposite_from_background = opposite_from_background.has(JSON.stringify({x, y}));
                const is_lit = background === '.' ? is_opposite_from_background : !is_opposite_from_background;
                line.push(is_lit ? '#' : '.'); 
            }
        }

        // final_image_render = final_image;
        
        return opposite_from_background.size;
    }

    function image_edges(opposite_from_background) {
        let min_x = Infinity, max_x = -Infinity, min_y = Infinity, max_y = -Infinity;
        for (const {x, y} of [...opposite_from_background].map(JSON.parse)) {
            min_x = Math.min(min_x, x);
            max_x = Math.max(max_x, x);
            min_y = Math.min(min_y, y);
            max_y = Math.max(max_y, y);
        }
        return { xs: [min_x - 2, max_x + 2], ys: [min_y - 2, max_y + 2] };
    }

    function square({x, y}) {
        return [[-1, -1], [0, -1], [1, -1], [-1, 0], [0, 0], [1, 0], [-1, 1], [0, 1], [1, 1]]
            .map(([dx, dy]) => ({ x: x+dx, y: y+dy }));
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
