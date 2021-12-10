<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => l.split(''));

    function part1(input) {
        const chunk_chars = { ')': '(', ']': '[', '}': '{', '>': '<'};
        const error_points = { ')': 3, ']': 57, '}': 1197, '>': 25137 }
        const score = subsystem => {
            const chunk_queue = [];
            for (const c of subsystem) {
                if (Object.values(chunk_chars).includes(c)) {
                    chunk_queue.push(c);
                } else if (Object.keys(chunk_chars).includes(c)) {
                    if (chunk_queue.pop() !== chunk_chars[c]) return error_points[c];
                }
            }
        };

        const error_scores = input.map(score).filter(Boolean);

        return error_scores.reduce((a, b) => a + b);
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
