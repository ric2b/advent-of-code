<script>
	export let data;

	$: component = data.svelte_calendar_components[data.day - 1];
	$: input = data.input;
	$: raw_input = JSON.stringify(data.input);
</script>

{#if component}
	<svelte:component this={component} {input} />

	<details>
		<summary>Input</summary>
		<textarea rows="20" cols="80" bind:value={raw_input}></textarea>
	</details>
{:else}
	<p>Not implemented yet</p>
	<img src="https://cataas.com/cat/sad" alt="sad cat" />
{/if}
