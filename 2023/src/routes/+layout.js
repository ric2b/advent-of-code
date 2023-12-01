import { readdirSync } from 'fs';

export async function load({ params }) {
    const repo_link = 'https://github.com/ric2b/advent-of-code';

    const days = readdirSync(`./src/lib/day`)
        .filter(file => /\d{1,2}\.svelte/.test(file))
        .map(file => Number(/(\d{1,2})\.svelte/.exec(file)[1]))

    const components = [];

    for (const day of days) {
        const padded_day = day.toString().padStart(2, '0')
        components[day] = (await import(`$lib/day/${padded_day}.svelte`)).default;
    }

    return {
        repo_link: 'https://github.com/ric2b/advent-of-code',
        day: Number(params.day),
        components: components,
    };
}
