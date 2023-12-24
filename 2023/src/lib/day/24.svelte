<script>
	export let raw_input;

	import { part1, part2 } from './24';

	$: p1 = part1(raw_input);
	// $: p2 = part2(raw_input);
    // let p2_promise = part2(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: 540355811503157</p>
<!-- TODO: Get Z3 running on the browser -->

<!--{#await part2(raw_input)}-->
<!--    <p>Part 2: ...</p>-->
<!--{:then p2}-->
<!--    <p class="aoc_yellow">Part 2: {p2}</p>-->
<!--&lt;!&ndash;    <p class="aoc_yellow">Part 2: {p2}</p>&ndash;&gt;-->
<!--{:catch someError}-->
<!--    System error: {someError.message}-->
<!--{/await}-->
