<script>
    export let data;

    $: padded_day = data.day.toString().padStart(2, '0');

    $: source_link = data.day > 0 ?
        `${data.repo_link}/blob/master/2023/src/lib/day/${padded_day}.svelte`
        : `${data.repo_link}/tree/master/2023`;
</script>

<title>2023 Solutions</title>

<header>
    <h1><a href="/">Advent of Code 2023</a></h1>

    <a href={source_link}>[Source Code]</a>
</header>

<slot></slot>
