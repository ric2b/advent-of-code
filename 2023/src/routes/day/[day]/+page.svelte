<script>
    export let data

    const day = data.day
</script>

<nav>
    <a href={ day > 1 ? day - 1 : null}>[Previous Day]</a>
    <a href={ day < data.components.length - 1 ? day + 1 : null}>[Next Day]</a>
</nav>

{#if day in data.components}
    <svelte:component this={data.components[day]} />
{:else}
    <p>Day not implemented yet</p>
    <img src="https://cataas.com/cat/sad" alt="sad cat" />
{/if}
