export function part1(raw_input: string): number {
	return raw_input
		.trim()
		.split('\n')
		.map((row) => {
			const [springs, raw_checksum] = row.split(' ');
			const checksum = raw_checksum.split(',').map(Number);

			return valid_variations(springs, checksum);
		})
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	return raw_input
		.trim()
		.split('\n')
		.map((row) => {
			const [springs, raw_checksum] = row.split(' ');
			const checksum = raw_checksum.split(',').map(Number);

			return valid_variations(
				new Array(5).fill(springs).join('?'),
				new Array(5).fill(checksum).flat()
			);
		})
		.reduce((a, b) => a + b);
}

function keyFromArgs(...args: (string | string[] | number | number[] | undefined)[]): string {
	return args.map((arg) => (Array.isArray(arg) ? arg.join(',') : arg?.toString())).join(';');
}

function memoize(fn) {
	const cache: Map<string, number> = new Map();

	return (...args) => {
		const key = keyFromArgs(...args);
		if (!cache.has(key)) {
			cache.set(key, fn(...args));
		}
		return cache.get(key);
	};
}

const valid_variations = memoize(
	(springs: string, groups: number[], current_group?: number): number => {
		const groups_complete = groups.length == 0 && !current_group;
		if (groups_complete) {
			return springs.includes('#') ? 0 : 1;
		}

		if (!groups_complete && !(springs.includes('#') || springs.includes('?'))) {
			return 0;
		}

		if (springs.length == 0) {
			return groups_complete ? 1 : 0; // valid if all groups have been covered, else invalid
		}

		switch (springs[0]) {
			case '.':
				return current_group == undefined || current_group == 0
					? valid_variations(springs.slice(1), groups)
					: 0;
			case '#':
				if (current_group == undefined) {
					return groups[0] > 0
						? valid_variations(springs.slice(1), groups.slice(1), groups[0] - 1)
						: valid_variations(springs.slice(1), groups.slice(1)); // new group
				} else if (current_group == 0) {
					return 0;
				} else {
					return valid_variations(springs.slice(1), groups, current_group - 1); // keep matching current group
				}
			case '?':
				const working_spring_variations = valid_variations(
					['.', ...springs.slice(1)],
					groups,
					current_group
				);
				const broken_spring_variations = valid_variations(
					['#', ...springs.slice(1)],
					groups,
					current_group
				);
				return working_spring_variations + broken_spring_variations;
		}

		throw new Error('Bug found');
	}
);
