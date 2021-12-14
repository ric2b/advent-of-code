<div class="grid">
    {#each input as line}
    <div class="line">
        {#each line as energy_level}
        <input class="cell energy_{energy_level}" value="{energy_level}"/>
        {/each}
    </div>
    <br>
    {/each}
</div>

<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split('').map(Number));

    function part1(energy_levels) {
        const max_steps = 100;
        let flash_count = 0;

        const tick = ({x, y}) => {
            if (energy_levels[y][x] < 0) return; // already flashed

            energy_levels[y][x] += 1;

            if (energy_levels[y][x] > 9) {
                flash_count += 1;
                energy_levels[y][x] = -1;
                neighbours(energy_levels, {x, y}).forEach(p => tick(p));
                // energy_levels[y][x] = 0;
            }
        }

        for (let step = 0; step < max_steps; step++) {
            for (const [y, line] of energy_levels.entries()) {
                for (const [x, level] of line.entries()) {
                    tick({x, y});
                }
            }

            for (const [y, line] of energy_levels.entries()) {
                for (const [x, level] of line.entries()) {

                    if (energy_levels[y][x] < 0) energy_levels[y][x] = 0;
                }
            }
        }

        return flash_count;
    }

    function part2(input) {
        return JSON.stringify(input);
    }

    function neighbours(grid, {x, y}) {
        return [[-1, -1], [0, -1], [1, -1], [-1, 0], [1, 0], [-1, 1], [0, 1], [1, 1]]
            .map(([dx, dy]) => ({ x: x+dx, y: y+dy }))
            .filter(({x, y}) => 0 <= x && x < grid[0].length && 0 <= y && y < grid.length);
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>

<style>
    .cell {
        /* these styles will let the divs line up next to each other
           while accepting dimensions */
        display: block;
        float: left;

        width: 10px;
        height: 10px;

        /* a small margin to separate the blocks */
        /*margin-right: 5px;*/
        margin: 3px;
    }

    .grid .cell.energy_-1 { background: white; }
    .grid .cell.energy_0 { background-color: #000000; }
    .grid .cell.energy_1 { background-color: #111111; }
    .grid .cell.energy_2 { background-color: #222222; }
    .grid .cell.energy_3 { background-color: #333333; }
    .grid .cell.energy_4 { background-color: #444444; }
    .grid .cell.energy_5 { background-color: #555555; }
    .grid .cell.energy_6 { background-color: #666666; }
    .grid .cell.energy_7 { background-color: #777777; }
    .grid .cell.energy_8 { background-color: #888888; }
    .grid .cell { background-color: #999999; }
</style>