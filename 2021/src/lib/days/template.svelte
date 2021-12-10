<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '');

    function part1(input) {
        return JSON.stringify(input);
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
