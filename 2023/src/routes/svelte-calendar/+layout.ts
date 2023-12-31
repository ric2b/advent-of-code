export async function load({ params }) {
	const titles = [
		'Naughty or Nice',
	];

	return {
		repo_link: 'https://github.com/ric2b/advent-of-code',
		titles,
		day: Number(params.day)
	};
}
