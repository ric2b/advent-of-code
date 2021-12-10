<script>
    function parse(raw_input) {
        return raw_input.split(',').map(s => Number(s));
    }

    function part1(timers) {
        let prev_timers = timers;
        for(let i = 0; i < 80; i++) {
            const new_timers = [];

            for (const current_timer of prev_timers) {
                if (current_timer <= 0) {
                    new_timers.push(6); // reset self timer
                    new_timers.push(8); // creates new lanternfish
                } else {
                    new_timers.push(current_timer - 1);
                }
            }

            prev_timers = new_timers;
        }

        return prev_timers.length;
    }

    function part2(timers) {
        const days_left = 256;

        const generation_counter = Array(9).fill(0);
        for (const timer of timers) { generation_counter[timer] += 1; }

        for (let i = 0; i < days_left; i++) {
            const reproducing = generation_counter.shift();
            generation_counter[6] += reproducing
            generation_counter.push(reproducing)
        }

        return generation_counter.reduce((a,b) => a + b);
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
