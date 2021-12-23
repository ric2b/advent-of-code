<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    const chunk_chars = { ')': '(', ']': '[', '}': '{', '>': '<'};

    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split(''));

    function part1(input) {
        return input.map(score_subsystem).reduce((a, b) => a + b);
    }

    function part2(input) {
        const completion_points = { '(': 1, '[': 2, '{': 3, '<': 4 }

        const scores = input.filter(score => score_subsystem(score) === 0).map(subsystem => {
            const chunk_queue = []
            for (const c of subsystem) {
                if (Object.values(chunk_chars).includes(c)) {
                    chunk_queue.push(c);
                } else if (Object.keys(chunk_chars).includes(c)) {
                    chunk_queue.pop()
                }
            }
            return chunk_queue.reverse().reduce((score, c) => score * 5 + completion_points[c], 0);
        });

        return scores.sort((a, b) => a - b)[Math.floor(scores.length / 2)];
    }

    function score_subsystem(subsystem) {
        const chunk_queue = [], error_points = { ')': 3, ']': 57, '}': 1197, '>': 25137 }

        for (const c of subsystem) {
            if (Object.values(chunk_chars).includes(c)) {
                chunk_queue.push(c);
            } else if (Object.keys(chunk_chars).includes(c)) {
                if (chunk_queue.pop() !== chunk_chars[c]) return error_points[c];
            }
        }
        return 0;
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
