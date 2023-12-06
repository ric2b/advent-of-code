export function part1(raw_input: string): number {
	return raw_input
		.trim()
		.split('\n')
		.map((s) => {
			const [winning_side, card_side] = s.split(':')[1].split('|');

			const winning_numbers = [...winning_side.match(/\d+/g)].map(Number);
			const card_numbers = [...card_side.match(/\d+/g)].map(Number);

			const intersection = card_numbers.filter(function (x) {
				return winning_numbers.indexOf(x) >= 0;
			});

			const card_score = intersection.length > 0 ? 2 ** (intersection.length - 1) : 0;
			return card_score;
		})
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	const card_scores: number[] = raw_input
		.trim()
		.split('\n')
		.map((s) => {
			const [winning_side, card_side] = s.split(':')[1].split('|');

			const winning_numbers = [...winning_side.match(/\d+/g)].map(Number);
			const card_numbers = [...card_side.match(/\d+/g)].map(Number);

			const intersection = card_numbers.filter(function (x) {
				return winning_numbers.indexOf(x) >= 0;
			});

			return intersection.length;
		});

	const card_copies: number[] = card_scores.map(() => 1);

	card_scores.forEach((score, card_i) => {
		const copies = card_copies[card_i];
		for (let i = card_i + 1; i <= card_i + score; i++) {
			card_copies[i] += copies;
		}
	});

	return card_copies.reduce((a, b) => a + b);
}
