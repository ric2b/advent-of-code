<script>
    export let data

    const day = data.day
</script>

<nav>
    <a href={ day > 1 ? day - 1 : null}>[Previous Day]</a>
    <a href={ day < data.titles.length - 1 ? day + 1 : null}>[Next Day]</a>
</nav>

<slot></slot>
