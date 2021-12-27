<table>
    <tbody>
    {#each prev_grid as line}
        <tr class="line">
            {#each line as value} <td class="cell {value}"></td> {/each}
        </tr>
    {/each}
    </tbody>
</table>

<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        return raw_input.split('\n').filter(l => l !== '').map(l => l.split(''));
    }

    let prev_grid;

    function part1(grid) {
        const width = grid[0].length, height = grid.length;
        prev_grid = grid;
        let new_grid;

        for (let steps = 1; steps < 1000; steps++) {
            console.log(steps);

            new_grid = Array.from({ length: height }, () => Array.from({ length: width }, () => '.'));

            prev_grid.forEach((line, i) => {
                line.forEach((value, j) => {
                    if (value === '>') {
                        if (prev_grid[i][(j + 1) % width] === '.') {
                            new_grid[i][(j + 1) % width] = '>';
                        } else {
                            new_grid[i][j] = '>';
                        }
                    }
                });
            });

            prev_grid.forEach((line, i) => {
                line.forEach((value, j) => {
                    if (value === 'v') {
                        if (prev_grid[(i + 1) % height][j] !== 'v' && new_grid[(i + 1) % height][j] === '.') {
                            new_grid[(i + 1) % height][j] = 'v';
                        } else {
                            new_grid[i][j] = 'v';
                        }
                    }
                });
            });

            if (JSON.stringify(prev_grid) === JSON.stringify(new_grid)) return steps;
            prev_grid = new_grid;
        }
    }

    function part2(input) {
        return 'Not Applicable';
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>

<style>
    table {
        width: 500px;
        height: 500px;
        border-collapse: collapse;
        border: 2px solid #ffff66;
    }
    td, th {
        padding: 1px;
        width: 1px;
        border: 1px solid #10101a;
    }

    .\> {
        background-color: green;
        color: green;
    }

    .v {
        background-color: red;
        color: red;
    }
</style>
