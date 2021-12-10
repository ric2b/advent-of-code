<script context="module">
    import { assets } from '$app/paths';
    export async function load({ page, fetch }) {
        const metadata_url = `${assets}/metadata.json`;
        const input_url = `${assets}/inputs/day${page.params.day}.txt`;
        const responses = await Promise.all([fetch(metadata_url), fetch(input_url)]);

        for (const res of responses) {
          if (!res.ok) return { status: res.status, error: new Error(`Could not load ${input_url}`) };
        }

        const [metadata_res, input_res] = responses;
        return { props: { metadata: await metadata_res.json(), raw_input: await input_res.text() } }
	}
</script>

<!-- TODO: Load and render the components dynamically -->

<script>
    import { page } from '$app/stores';
    // for (let i = 1; i <= 10, i++) {
    //   import(`$lib/days/${String(i).padStart(2, '0')}.svelte`);
    // }
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

    export let metadata;
    export let raw_input;
    export let part1_result = '...';
    export let part2_result = '...';

    $: day_number = Number($page.params.day);
</script>

<title>Day {day_number} Solution</title>

<!-- TODO: Try to not depend on external: https://github.com/sveltejs/kit/issues/552 -->
<nav>
  <a rel="external" sveltekit:prefetch href="{day_number > 1 ? String(day_number-1).padStart(2, '0') : '/'}">[Previous Day]</a>
  <a rel="external" sveltekit:prefetch href="{day_number < 10 ? String(day_number+1).padStart(2, '0') : '/'}">[Next Day]</a>
<!--  <a sveltekit:prefetch href="{day_number > 1 ? String(day_number-1).padStart(2, '0') : '/'}">[Previous Day]</a>-->
<!--  <a sveltekit:prefetch href="{day_number < 10 ? String(day_number+1).padStart(2, '0') : '/'}">[Next Day]</a>-->
</nav>

<article>
    <h2>--- {metadata.titles[day_number - 1]} ---</h2>

    <p class:aoc_yellow={part1_result !== '...'}>Part 1: {part1_result}</p>
    <p class:aoc_yellow={part1_result !== '...'}>Part 2: {part2_result}</p>

    {#if $page.params.day === '01'} <Day01 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '02'} <Day02 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '03'} <Day03 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '04'} <Day04 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '05'} <Day05 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '06'} <Day06 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '07'} <Day07 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '08'} <Day08 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '09'} <Day09 {raw_input} bind:part1_result bind:part2_result /> {/if}
    {#if $page.params.day === '10'} <Day10 {raw_input} bind:part1_result bind:part2_result /> {/if}

    <details>
      <summary>Change input</summary>
      <textarea rows="20" cols="80" bind:value={raw_input}></textarea>
    </details>
</article>
