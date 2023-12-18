<script>
	export let raw_input;

	import { part1, part2, part_1_grid } from './18';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);

	$: grid = part_1_grid(raw_input).map(l => l.join(''));
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>

<div class="grid">
	{#each grid as row}
		<div class="row">{row}</div>
	{/each}
</div>

<style>
	.grid {
		font-family: monospace;
		font-size: 0.5ch;
		line-height: 1ch;
		white-space: pre;
		color: red;
	}

	.row {
	}
</style>
