export function part1(raw_input: string): number {
	const patterns = raw_input.trim().split('\n\n').map(p => p.split('\n'));

	return patterns.map(pattern => {
		const mirror_column = find_mirror_line(transpose(pattern));
		const mirror_line = find_mirror_line(pattern);
		return 100 * mirror_line + mirror_column;
	}).reduce((a, b) => a+b);
}

export function part2(raw_input: string): number {
	const lines = raw_input.trim().split('\n');

	return 2;
}

function find_mirror_line(pattern: string[]): number {
	const candidate_mirror_lines = []
	for (let i = 0; i < pattern.length; i ++) {
		if(pattern[i] == pattern[i+1]) {
			candidate_mirror_lines.push(i)
		}
	}

	const mirror_line = candidate_mirror_lines.filter(possible_mirror_line => {
		for (let offset = 0; possible_mirror_line-offset >= 0 && possible_mirror_line+offset+1 < pattern.length; offset++) {
			if (pattern[possible_mirror_line - offset] != pattern[possible_mirror_line+offset+1]) {
				return false;
			}
		}

		return true;
	})

	if (mirror_line.length == 0 ) {
		return 0;
	}

	if (mirror_line.length > 1) {
		throw new Error('Bug found');
	}

	return mirror_line[0] + 1;
}

function transpose(pattern: string[]): string[] {
	return Object.keys(pattern[0]).map(col => {
		return pattern.map(row => row[col]).join('');
	});
}
