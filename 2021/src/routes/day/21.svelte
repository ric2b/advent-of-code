<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => Number(l.match(/starting position: (\d+)/)[1]));

    function part1(initial_positions) {
        const player_positions = [...initial_positions];
        const scores = [0, 0];

        let die_rolls = 0;
        for (let i = 0; scores.every(s => s < 1000); i = ++i % initial_positions.length) {
            const position = player_positions[i];

            const roll_values = [die_rolls++, die_rolls++, die_rolls++].map(roll => 1 + (roll % 100));
            // console.log(`rolls ${roll_values}`)

            const moves_forward = roll_values.reduce((a, b) => a + b);
            // console.log(`moves forward ${moves_forward}`)

            const lands_on = (position + moves_forward) % 10;
            player_positions[i] = lands_on;
            // console.log(`lands on ${lands_on}`)

            scores[i] += lands_on === 0 ? 10 : lands_on;
            // console.log(`scores: ${scores}`)
        }

        return Math.min(...scores) * die_rolls;
    }

    function part2(initial_positions) {
        const winning_score = 21
        const known_universes = new Map();

        // const possible_rolls = [3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9];
        const roll_counts = [[3, 1], [4, 3], [5, 6], [6, 7], [7, 6], [8, 3], [9, 1]];

        const wins = ({ p1_playing, positions, scores }) => {
            if (scores.some(s => s >= winning_score)) return scores[0] > scores[1] ? [1, 0] : [0, 1];

            const serialized_state = JSON.stringify({ p1_playing, positions, scores });
            const cached_win_counts = known_universes.get(serialized_state);
            if (cached_win_counts) return cached_win_counts;

            const further_win_counts = roll_counts.map(([roll, count]) => {
                const player_index = p1_playing ? 0 : 1;

                const new_positions = [...positions];
                const new_scores = [...scores];

                const lands_on = (new_positions[player_index] + roll) % 10;
                new_scores[player_index] += lands_on === 0 ? 10 : lands_on;
                new_positions[player_index] = lands_on

                return wins({ p1_playing: !p1_playing, positions: new_positions, scores: new_scores }).map(n => n * count);
            }).reduce(([p1w1, p2w1], [p1w2, p2w2]) => [p1w1 + p1w2, p2w1 + p2w2]);

            known_universes.set(serialized_state, further_win_counts);
            return further_win_counts;
        }

        return Math.max(...wins({ p1_playing: true, positions: [...initial_positions], scores: [0, 0] }));
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
