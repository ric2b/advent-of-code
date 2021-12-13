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

        const day_components = [], metadata = await metadata_res.json(), raw_input = await input_res.text();

        for (let i = 1; i <= metadata.titles.length; i++) {
            day_components.push((await import(`/src/lib/days/${String(i).padStart(2, '0')}.svelte`)).default);
        }

        return { props: { day_components, metadata, raw_input } };
	}
</script>

<script>
    import { page } from '$app/stores';
    export let day_components;
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
  <!-- <a sveltekit:prefetch href="{day_number > 1 ? String(day_number-1).padStart(2, '0') : '/'}">[Previous Day]</a> -->
  <!-- <a sveltekit:prefetch href="{day_number < 10 ? String(day_number+1).padStart(2, '0') : '/'}">[Next Day]</a> -->
</nav>

<article>
    <h2>--- {metadata.titles[day_number - 1]} ---</h2>

    <p class:aoc_yellow={part1_result !== '...'}>Part 1: {part1_result}</p>
    <p class:aoc_yellow={part2_result !== '...'}>Part 2: {part2_result}</p>

    <svelte:component this={day_components[day_number-1]} {raw_input} bind:part1_result bind:part2_result />

    <details>
      <summary>Change input</summary>
      <textarea rows="20" cols="80" bind:value={raw_input}></textarea>
    </details>
</article>
