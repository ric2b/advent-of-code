<script>
	export let data;

	$: day = data.day;
</script>

<nav>
	<a href={day > 1 ? day - 1 : null}>[Previous Day]</a>
	<a href={day < data.titles.length ? day + 1 : null}>[Next Day]</a>
</nav>

<!--<slot />-->

<article>
	<h2>--- Day {day} ---</h2>

<!--	<p class:aoc_yellow={$part1_result !== '...'}>Part 1: {$part1_result}</p>-->
<!--	<p class:aoc_yellow={$part2_result !== '...'}>Part 2: {$part2_result}</p>-->

	<slot />
</article>