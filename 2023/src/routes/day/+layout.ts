export async function load({ fetch, params }) {
	const day = Number(params.day);
	const padded_day = day.toString().padStart(2, '0');

	return {
		input: await (await fetch(`/inputs/${padded_day}.txt`)).text()
	};
}
