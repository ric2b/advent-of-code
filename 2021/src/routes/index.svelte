<!-- TODO:
    - More code sharing between pages
    - Viz for days: 2, 4, 5, 8, 9
-->
<title>2021 Solutions</title>

<ul>
    {#each metadata.titles as title, i}
        <li><a sveltekit:prefetch href="day/{String(i+1).padStart(2, '0')}">{title}</a></li>
    {/each}
</ul>

<script context="module">
    import { assets } from '$app/paths';
    export async function load({ fetch }) {
        const url = `${assets}/metadata.json`;
        const res = await fetch(url);

		if (res.ok) return { props: { metadata: await res.json() } };

		return { status: res.status, error: new Error(`Could not load ${url}`) };
	}
</script>

<script>
    export let metadata;
</script>
