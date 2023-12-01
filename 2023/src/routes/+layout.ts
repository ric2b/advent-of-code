export async function load({ params }) {
	const titles = ['Trebuchet?!'];

	const components = [];
	for (const [day] of titles.entries()) {
		const padded_day = (day + 1).toString().padStart(2, '0');
		components[day] = (await import(`$lib/day/${padded_day}.svelte`)).default;
	}

	return {
		repo_link: 'https://github.com/ric2b/advent-of-code',
		titles,
		components,
		day: Number(params.day)
	};
}
