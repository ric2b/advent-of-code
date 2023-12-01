<script>
    export let data;
</script>

<title>2023 Solutions</title>

<ul>
    {#each data.components.slice(1) as day_component, i}
        <li><a href={`day/${i+1}`}>Day {i+1}</a></li>
    {/each}
</ul>
