<article>
    <h2>--- Day 7: The Treachery of Whales ---</h2>

    <p class="aoc_yellow">Part 1: {part1_result}</p>
    <p class="aoc_yellow">Part 2: {part2_result}</p>
</article>

<script>
    function parse(raw_input) {
        return raw_input.split(',').map(s => Number(s));
    }

    function part1(positions) {
        const fuel_required = (positions, new_x) => positions.map(x => Math.abs(x - new_x)).reduce((a,b) => a + b);

        const median_position = positions.sort((a,b) => a-b)[Math.floor(positions.length / 2)];
        return fuel_required(positions, median_position);
    }

    function part2(positions) {
        const fuel_required = (positions, new_x) => positions.map(x => {
            const distance = Math.abs(x - new_x);
            return distance*(distance+1) / 2
        }).reduce((a, b) => a + b);

        let optimal_fuel_required = Infinity;
        for (let i = Math.min(...positions); i <= Math.max(...positions); i++) {
            const current_fuel_required = fuel_required(positions, i);
            if (current_fuel_required < optimal_fuel_required) {
                optimal_fuel_required = current_fuel_required;
            }
        }
        return optimal_fuel_required;
    }

    export let raw_input;

    $: input = parse(raw_input);
	$: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
