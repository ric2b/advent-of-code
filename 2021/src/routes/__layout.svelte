<header>
    <h1><a href="/">Advent of Code 2021</a></h1>

    <a href="{source_link}">[Source Code]</a>
</header>

<slot></slot>

<script>
    import { page } from '$app/stores';
    const repo_link = 'https://github.com/ric2b/advent-of-code';
    $: day = Number($page.path.split('/').slice(-1)[0]);
    $: source_link = day > 0 ?
        `${repo_link}/blob/master/2021/src/routes/day/${day}.svelte`
        : `${repo_link}/tree/master/2021`;
</script>
