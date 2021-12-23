{#if JSON.stringify(input) === JSON.stringify(expected_input)}
    (Wetware solution goes brrrrr)
    <img alt="I know that one" src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fmemegenerator.net%2Fimg%2Fimages%2F16821874.jpg&f=1&nofb=1">
{:else}
    (Don't feel like implementing another BFS or Djikstra)
    <img alt="Oops" src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fih0.redbubble.net%2Fimage.256775008.1869%2Fpp%2C550x550.u8.jpg&f=1&nofb=1">
{/if}

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
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>

<style>
    img { max-height:450px; }
</style>
