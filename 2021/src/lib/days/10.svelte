<article>
    <h2>--- Day 10: Syntax Scoring ---</h2>

    <p class="aoc_yellow">Part 1: {part1_result}</p>
    <p class="aoc_yellow">Part 2: {part2_result}</p>
</article>

<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '');

    function part1(input) {
        return JSON.stringify(input);
    }

    function part2(input) {
        return JSON.stringify(input);
    }

    export let raw_input;

    $: input = parse(raw_input);
	$: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
