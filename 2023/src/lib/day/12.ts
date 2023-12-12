export function part1(raw_input: string): number {
	return raw_input.trim().split('\n').map(row => {
		const [springs, raw_checksum] = row.split(' ')
		const checksum = raw_checksum.split(',').map(Number)

		return valid_variations(springs, checksum)
	}).reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	return raw_input.trim().split('\n').map(row => {
		const [springs, raw_checksum] = row.split(' ')
		const checksum = raw_checksum.split(',').map(Number)

		return valid_variations(
			new Array(5).fill(springs).join('?'),
			new Array(5).fill(checksum).flat(),
		)
	}).reduce((a, b) => a + b);
}

const memo: Map<string, number> = new Map();

function valid_variations(springs: string, groups: number[], current_group?: number): number {
	const key = key_from_state(springs, groups, current_group);
	if (memo.has(key)) { return memo.get(key) }
	const record = v => {
		memo.set(key, v);
		return v
	}

	const groups_complete = groups.length == 0 && (current_group == undefined || current_group == 0);
	if (groups_complete) {
		return record(springs.includes('#') ? 0 : 1);
	}

	if (!groups_complete && !(springs.includes('#') || springs.includes('?'))) {
		return record(0);
	}

	if (springs.length == 0) {
		return record(groups_complete ? 1 : 0); // valid if all groups have been covered, else invalid
	}

	switch (springs[0]) {
		case '.':
			return record(current_group == undefined || current_group == 0 ? valid_variations(springs.slice(1), groups) : 0);
		case '#':
			if (current_group == undefined) {
				return record(groups[0] > 0 ? valid_variations(springs.slice(1), groups.slice(1), groups[0] - 1) : valid_variations(springs.slice(1), groups.slice(1))); // new group
			} else if (current_group == 0) {
			 	return record(0);
			} else {
				return record(valid_variations(springs.slice(1), groups, current_group - 1)); // keep matching current group
			}
		case '?':
			const working_spring_variations = valid_variations(['.', ...springs.slice(1)], groups, current_group);
			const broken_spring_variations = valid_variations(['#', ...springs.slice(1)], groups, current_group);
			return record(working_spring_variations + broken_spring_variations);
	}

	throw new Error('Bug found');
}

function key_from_state(springs: string, groups: number[], current_group?: number): string {
	return `${springs};${groups.join(',')};${current_group}`;
}
