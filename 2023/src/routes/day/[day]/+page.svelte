<script>
    export let data

    const component = data.components[data.day - 1]
</script>

{#if component}
    <svelte:component this={component} raw_input={data.input} />
<!--    <svelte:component this={data.components[day]} raw_input='bla'/>-->
{:else}
    <p>Not implemented yet</p>
    <img src="https://cataas.com/cat/sad" alt="sad cat" />
{/if}