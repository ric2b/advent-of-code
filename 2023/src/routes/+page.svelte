<script>
    export let data;
</script>

<title>2023 Solutions</title>

<ul>
    {#each data.titles as title, i}
        <li><a href={`day/${i+1}`}>Day {i+1}: {title}</a></li>
    {/each}
</ul>
