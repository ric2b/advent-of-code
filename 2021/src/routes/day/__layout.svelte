<script context="module">
    import { assets } from '$app/paths';
    import { writable } from "svelte/store";
    export async function load({ page, fetch }) {
        const day = page.path.split('/').slice(-1)[0];
        const metadata_url = `${assets}/metadata.json`;
        const input_url = `${assets}/inputs/day${day}.txt`;

        const responses = await Promise.all([fetch(metadata_url), fetch(input_url)]);
        for (const res of responses) {
            if (!res.ok) return { status: res.status, error: new Error(`Could not load ${input_url}`) };
        }

        const [metadata_res, input_res] = responses;
        const metadata = await metadata_res.json(), initial_raw_input = await input_res.text();

        const raw_input = writable(initial_raw_input);
        const { set: set_part1_result, ...part1_result } = writable('...');
        const { set: set_part2_result, ...part2_result } = writable('...');

        return {
            props: { raw_input, part1_result, part2_result, metadata },
            stuff: { set_part1_result, set_part2_result, raw_input }
        };
    }
</script>

<script>
    import { page } from '$app/stores';
    $: day = Number($page.path.split('/').slice(-1)[0]);
    export let metadata;
    export let raw_input;
    export let part1_result;
    export let part2_result;
</script>

<title>Day {day} Solution</title>

<nav>
     <a sveltekit:prefetch href="{day > 1 ? String(day-1).padStart(2, '0') : '/'}">[Previous Day]</a>
     <a sveltekit:prefetch href="{day < metadata.titles.length ? String(day+1).padStart(2, '0') : '/'}">[Next Day]</a>
</nav>

<article>
    <h2>--- {metadata.titles[day - 1]} ---</h2>

    <p class:aoc_yellow={$part1_result !== '...'}>Part 1: {$part1_result}</p>
    <p class:aoc_yellow={$part2_result !== '...'}>Part 2: {$part2_result}</p>

    <slot />

    <details>
        <summary>Change input</summary>
        <textarea rows="20" cols="80" bind:value={$raw_input}></textarea>
    </details>
</article>
