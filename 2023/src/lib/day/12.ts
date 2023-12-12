interface location {
	row: number;
	col: number;
}

export function part1(raw_input: string): number {
	const spring_rows: string[] = raw_input.trim().split('\n').map(row => {
		const [springs, raw_checksum] = row.split(' ')
		const checksum = raw_checksum.split(',').map(Number)

		return {springs, checksum}
	});

	const valid_variations = spring_rows.flatMap(({springs, checksum}) => {
		return create_variations(springs).filter(v => valid_variation(checksum, v))
	})

	return valid_variations.length
}

export function part2(raw_input: string, expansion_factor: number = 1_000_000): number {
	const universe: string[] = raw_input.trim().split('\n');

	return 2;
}

function create_variations(springs: string): string[][] {
	const pattern = [...springs]
	const variation_count = 2**(pattern.filter(c => c == '?').length)

	const variations = []
	for (let i = 0; i < variation_count; i++) {
		const variation = []
		let current_bit = 0;
		for (let j = 0; j < pattern.length; j++) {
			if (pattern[j] == '?') {
				variation.push(((i >> current_bit) & 1) ? '.' : '#');
				current_bit++;
			} else {
				variation.push(pattern[j]);
			}
		}
		variations.push(variation)
	}
	return variations;
}

function valid_variation(checksum: number[], variation: string[]) {
	const group_counts = []
	let current_group_count = 0
	for (let i = 0; i < variation.length; i++) {
		const char = variation[i];
		if (char == '#') {
			current_group_count++;
		} else if (current_group_count > 0) {
			group_counts.push(current_group_count);
			current_group_count = 0;
		}
	}

	if(current_group_count > 0) {
		group_counts.push(current_group_count)
	}

	return checksum.join(',') == group_counts.join(',');
}