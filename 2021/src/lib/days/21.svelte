<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(l => Number(l.match(/starting position: (\d+)/)[1]));

    function part1(initial_positions) {
        const player_positions = initial_positions;
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

        // expect 745 * 993 = 739785
        return Math.min(...scores) * die_rolls;
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
