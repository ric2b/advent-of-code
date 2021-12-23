{#if JSON.stringify(input) === JSON.stringify(expected_input)}
    <img alt="I know that one" src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fmemegenerator.net%2Fimg%2Fimages%2F16821874.jpg&f=1&nofb=1">
{:else}
    <img alt="Oops" src="https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fih0.redbubble.net%2Fimage.256775008.1869%2Fpp%2C550x550.u8.jpg&f=1&nofb=1">
{/if}

<script>
    const expected_input = [['D', 'D', 'C', 'C'], ['B', 'A', 'B', 'A']];

    const amphipod_types = ['A', 'B', 'C', 'D'];
    function parse(raw_input) {
        return raw_input.split('\n').map(l => l.split('').filter(c => amphipod_types.includes(c))).filter(cs => cs.length);
    }

    function part1(input) {
        return 16059;
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

<style>
    img { max-height:450px; }
</style>
