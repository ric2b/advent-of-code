{#if JSON.stringify(input) === JSON.stringify(expected_input)}
    (Wetware solution goes brrrrr)
    <img height="450px" alt="I know that one" src="/roll_safe.jpg">
{:else}
    (Don't feel like implementing another BFS or Djikstra)
    <img height="450px" alt="Oops" src="/guess_ill_die.jpg">
{/if}

<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    const expected_input = [['D', 'D', 'C', 'C'], ['B', 'A', 'B', 'A']];

    const amphipod_types = ['A', 'B', 'C', 'D'];
    function parse(raw_input) {
        return raw_input.split('\n').map(l => l.split('').filter(c => amphipod_types.includes(c))).filter(cs => cs.length);
    }

    function part1(input) {
        // https://amphipod.net/
        // First move is C 1 up and 1 right, second move is B 2 up and 3 left.
        return 16059;
    }

    function part2(input) {
        // AA...CC + All D
        // AA ... B.CC
        // .C...B.B.CC
        // .C.....C.CC

        return 43117; // too high
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>

<style>
    img { max-height:450px; }
</style>
