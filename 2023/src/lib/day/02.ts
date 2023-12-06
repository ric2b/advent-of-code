export function part1(raw_input: string): number {
	const bag_counts = {
		red: 12,
		green: 13,
		blue: 14
	};

	return raw_input
		.trim()
		.split('\n')
		.map((raw_game) => {
			const game_id = Number(/Game (\d+):/g.exec(raw_game)[1]);

			const counts = [...raw_game.matchAll(/(?<n>\d+) (?<color>red|green|blue)/g)]
				.map((match) => match.groups)
				.reduce((result, currentObject) => {
					const n = Number(currentObject.n);
					if (!result[currentObject.color] || result[currentObject.color] < n) {
						result[currentObject.color] = n;
					}

					return result;
				}, {});

			const game_possible = Object.entries(bag_counts).every(
				([color, count]) => counts[color] <= count
			);

			return game_possible ? game_id : 0;
		})
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	return raw_input
		.trim()
		.split('\n')
		.map((raw_game) => {
			const min_counts = [...raw_game.matchAll(/(?<n>\d+) (?<color>red|green|blue)/g)]
				.map((match) => match.groups)
				.reduce((result, currentObject) => {
					const n = Number(currentObject.n);
					if (!result[currentObject.color] || result[currentObject.color] < n) {
						result[currentObject.color] = n;
					}

					return result;
				}, {});

			return min_counts['red'] * min_counts['green'] * min_counts['blue'];
		})
		.reduce((a, b) => a + b);
}
