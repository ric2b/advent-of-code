<script>
	export let raw_input;

	import { part1, part2 } from './17';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);

	// $: grid = part_1_grid(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>

<!-- TODO -->
<!--<div class="grid">-->
<!--	{#each grid as row}-->
<!--		<div class="row">{row}</div>-->
<!--	{/each}-->
<!--</div>-->

<!--<style>-->
<!--	.grid {-->
<!--		font-family: monospace;-->
<!--		font-size: 1ch;-->
<!--		line-height: 1ch;-->
<!--		white-space: pre;-->
<!--		color: red;-->
<!--	}-->

<!--	.row {-->
<!--	}-->
<!--</style>-->
