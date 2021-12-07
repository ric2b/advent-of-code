<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day06.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

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
        // const days_left = 256;
        const days_left = 18; // expected 5934
        let total = 0;

        // const memoize = new Map();

        // for (const current_timer of [1]) {
        //     const descendents = direct_descendents(current_timer, days_left);
        //
        //     // let family_total = 0;
        //     // for (let i = descendents; i > 0; i--) {
        //     //     family_total += family_total + direct_descendents(8, days_left - born_on_day(current_timer, i));
        //     // }
        //     let family_total = Array(descendents).fill().reduce((acc, _, i) => {
        //         return acc + direct_descendents(8, days_left - born_on_day(current_timer, i));
        //     }, 0)
        //     family_total += 1 + descendents
        //
        //     total += family_total;
        // }
        // debugger
        // return total;
        //
        // timers.reduce((acc, timer) => acc + family_total(timer, days_left), 0)
        //
        // .reduce((a,b) => a + b)
        console.log(family_total(1, days_left)) // 7 -> 7
        console.log(family_total(2, days_left)) // 6 -> 5
        console.log(family_total(3, days_left)) // 5 -> 5
        console.log(family_total(3, days_left)) // 5 -> 5
        console.log(family_total(4, days_left)) // 5 -> 4

        return timers.reduce((acc, timer) => acc + family_total(timer, days_left), 0);
    }

    function family_total(delay, days_left) {
        const n_descendents = direct_descendents(delay, days_left);
        if (n_descendents === 0) return 1;
        return 1 + Array(n_descendents).fill().map((_, i) => family_total(8, days_left - born_on_day(delay, i))).reduce((a,b) => a + b);
    }

    function direct_descendents(delay, days_left) {
        if (delay >= days_left) return 0;
        return 1 + Math.floor((days_left - delay) / 7);
    }

    const born_on_day = (delay, n)  => delay + n * 7 + 1;
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
