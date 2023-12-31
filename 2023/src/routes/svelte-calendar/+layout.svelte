<script>
	export let data;

	$: padded_day = data.day.toString().padStart(2, '0');

	$: source_link =
		data.day > 0
			? `${data.repo_link}/blob/master/2023/src/lib/svelte-calendar/day/${padded_day}.svelte`
			: `${data.repo_link}/tree/master/2023/src/lib/svelte-calendar`;
</script>

{#if data.day}
	<title>Solution - Day {data.day}</title>
{:else}
	<title>Solutions - Advent of Svelte 2023</title>
{/if}

<header>
	<h2><a href="/svelte-calendar">Advent of Svelte 2023</a></h2>
</header>

<slot />
