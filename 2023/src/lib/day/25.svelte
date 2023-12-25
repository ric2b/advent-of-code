<script>
	export let raw_input;

	import { part1, part2 } from './25';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);
    // let p2_promise = part2(raw_input);
</script>

<!--TODO-->
<!--<p class="aoc_yellow">Part 1: {p1}</p>-->
<!--<p class="aoc_yellow">Part 2: 540355811503157</p>-->
