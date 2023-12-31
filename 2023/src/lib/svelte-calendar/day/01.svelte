<script>
    export let input;

    let tallies = new Map();

    $: initialize_tallies(input);

    function initialize_tallies(input) {
        input.forEach(({name, tally}) => tallies.set(name, Number(tally)));
        tallies = tallies;
    }

    function change_by(name, value) {
        tallies.set(name, tallies.get(name) + value);
        tallies = tallies;
    }

    let name;
    let tally;

    function set_entry() {
        tallies.set(name, tally);
        tallies = tallies;
    }
</script>

<div>
    <label>Name: <input type="text" id="new_entry_name" name="Name" placeholder="Name" bind:value={name}></label>
    <label>Tally: <input type="number" id="new_entry_tally" name="Tally" placeholder="0" bind:value={tally}></label>
    <button on:click={set_entry}>Set</button>
</div>


<ul>
    {#each tallies.entries() as [name, tally] (name)}
        <li>
            <h3>{name}: {tally}</h3>
            <div>
                <button on:click={() => change_by(name, +1)}>+</button>
                <button on:click={() => change_by(name, -1)}>-</button>
            </div>
        </li>
    {/each}
</ul>