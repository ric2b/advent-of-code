<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    const parse = raw_input => raw_input.split('\n').map(s => Number(s));

    const part1 = depths => depths.filter((value, i) => depths[i+1] > value).length;
    const part2 = depths => depths.filter((value, i) => window(depths, i + 1) > window(depths, i)).length;

    const window = (depths, i) => depths.slice(i, i + 3).reduce((a, b) => a + b, 0);

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
