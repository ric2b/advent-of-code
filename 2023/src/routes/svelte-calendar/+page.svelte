<script>
	export let data;

	$: padded_day = data.day.toString().padStart(2, '0');
</script>

<ul>
	{#each data.titles as title, i}
		<li><a href={`/svelte-calendar/day/${i + 1}`}>Day {i + 1}: {title}</a></li>
	{/each}
</ul>
