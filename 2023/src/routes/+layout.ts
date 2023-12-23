export async function load({ params }) {
	const titles = [
		'Trebuchet?!',
		'Cube Conundrum',
		'Gear Ratios',
		'Scratchcards',
		'If You Give A Seed A Fertilizer',
		'Wait For It',
		'Camel Cards',
		'Haunted Wasteland',
		'Mirage Maintenance',
		'Pipe Maze',
		'Cosmic Expansion',
		'Hot Springs',
		'Point of Incidence',
		'Parabolic Reflector Dish',
		'Lens Library',
		'The Floor Will Be Lava',
		'Clumsy Crucible',
		'Lavaduct Lagoon',
		'Aplenty',
		'Pulse Propagation',
		'Step Counter',
		'Sand Slabs',
		'A Long Walk'
	];

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
