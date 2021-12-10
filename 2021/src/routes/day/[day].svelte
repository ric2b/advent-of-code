<script context="module">
    import { assets } from '$app/paths';
    export async function load({ page, fetch }) {
        const url = `${assets}/inputs/day${page.params.day}.txt`;
        const res = await fetch(url);

		if (res.ok) return { props: { raw_input: await res.text() } };

		return { status: res.status, error: new Error(`Could not load ${url}`) };
	}
</script>

<!-- TODO: Load and render the components dynamically -->

<script>
    import { page } from '$app/stores';
    import Day01 from '$lib/days/01.svelte';
    import Day02 from '$lib/days/02.svelte';
    import Day03 from '$lib/days/03.svelte';
    import Day04 from '$lib/days/04.svelte';
    import Day05 from '$lib/days/05.svelte';
    import Day06 from '$lib/days/06.svelte';
    import Day07 from '$lib/days/07.svelte';
    import Day08 from '$lib/days/08.svelte';
    import Day09 from '$lib/days/09.svelte';
    import Day10 from '$lib/days/10.svelte';

    export let raw_input;
    // $: day_number = Number($page.params.day);
</script>

<!--<nav>-->
<!--  <a href="{String(day_number-1).padStart(2, '0')}">[Previous Day]</a>-->
<!--  <a href="{String(day_number+1).padStart(2, '0')}">[Next Day]</a>-->
<!--</nav>-->

{#if $page.params.day === '01'} <Day01 {raw_input}/> {/if}
{#if $page.params.day === '02'} <Day02 {raw_input}/> {/if}
{#if $page.params.day === '03'} <Day03 {raw_input}/> {/if}
{#if $page.params.day === '04'} <Day04 {raw_input}/> {/if}
{#if $page.params.day === '05'} <Day05 {raw_input}/> {/if}
{#if $page.params.day === '06'} <Day06 {raw_input}/> {/if}
{#if $page.params.day === '07'} <Day07 {raw_input}/> {/if}
{#if $page.params.day === '08'} <Day08 {raw_input}/> {/if}
{#if $page.params.day === '09'} <Day09 {raw_input}/> {/if}
{#if $page.params.day === '10'} <Day10 {raw_input}/> {/if}

<details>
  <summary>Change input</summary>
  <textarea rows="20" cols="80" bind:value={raw_input}></textarea>
</details>
