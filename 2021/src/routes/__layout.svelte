<header>
    <h1><a href="/">Advent of Code 2021</a></h1>

    <a href="{source_link}">[Source Code]</a>
</header>

<slot></slot>

<script>
    import { page } from '$app/stores';
    const repo_link = 'https://github.com/ric2b/advent-of-code';
    $: source_link = $page.params.day ?
        `${repo_link}/blob/master/2021/src/lib/days/${String(Number($page.params.day)).padStart(2, '0')}.svelte`
        : `${repo_link}/tree/master/2021`;
</script>
