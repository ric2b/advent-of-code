<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        return raw_input.split('\n').filter(s => s !== '').map(m => m.split('').map(d => Number(d)));
    }

    function part1(diagnostics) {
        const gamma_bits = common_bits(diagnostics);
        const epsilon_bits = gamma_bits.map(bit => bit === 0 ? 1 : 0);

        const gamma = parseInt(gamma_bits.join(''), 2);
        const epsilon = parseInt(epsilon_bits.join(''), 2);

        return gamma * epsilon
    }

    function part2(diagnostics) {
        let o2_candidates = diagnostics, co2_candidates = diagnostics;

        for (let i of Array(diagnostics[0].length).keys()) {
            o2_candidates = o2_candidates.filter(diagnostic => diagnostic[i] === common_bits(o2_candidates)[i])
            if (co2_candidates.length > 1) {
                co2_candidates = co2_candidates.filter(diagnostic => diagnostic[i] !== common_bits(co2_candidates)[i])
            }
        }

        const o2_generator_rating = parseInt(o2_candidates[0].join(''), 2);
        const co2_scrubber_rating = parseInt(co2_candidates[0].join(''), 2);

        return o2_generator_rating * co2_scrubber_rating;
    }

    function common_bits(diagnostics) {
        const one_counts = Array(diagnostics[0].length).fill(0);
        diagnostics.forEach(diagnostic => diagnostic.forEach((value, i) => one_counts[i] += value));
        return one_counts.map(one_count => one_count >= diagnostics.length / 2 ? 1 : 0);
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
