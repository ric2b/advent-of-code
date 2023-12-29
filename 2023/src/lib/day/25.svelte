<script>
	export let raw_input;

	import { part1 } from './25';

	$: p1 = part1(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
