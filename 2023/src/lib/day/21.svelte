<script>
	export let raw_input;

	import { part1, part2 } from './21';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>
