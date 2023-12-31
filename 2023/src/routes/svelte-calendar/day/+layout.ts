export async function load({ fetch, params }) {
    const day = Number(params.day);
    const padded_day = day.toString().padStart(2, '0');

    return {
        input: (await fetch(`/inputs/svelte/${padded_day}.json`)).json()
    };
}
