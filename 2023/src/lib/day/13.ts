export function part1(raw_input: string): number {
	const patterns = raw_input
		.trim()
		.split('\n\n')
		.map((p) => p.split('\n'));

	return patterns
		.map((pattern) => {
			const mirror_column = find_mirror_line(transpose(pattern));
			const mirror_line = find_mirror_line(pattern);
			return 100 * mirror_line + mirror_column;
		})
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	const patterns = raw_input
		.trim()
		.split('\n\n')
		.map((p) => p.split('\n'));

	return patterns
		.map((pattern) => {
			const mirror_column = find_mirror_line(transpose(pattern), true);
			const mirror_line = find_mirror_line(pattern, true);
			return 100 * mirror_line + mirror_column;
		})
		.reduce((a, b) => a + b);
}

function different_character_count(s1: string, s2: string): number {
	let count = 0;

	for (let i = 0; i < s1.length; i++) {
		if (s2[i] != s1[i]) {
			count++;
		}
	}

	return count;
}

function find_mirror_line(pattern: string[], allow_smudge: boolean = false): number {
	const candidate_mirror_lines = [];
	for (let i = 0; i < pattern.length - 1; i++) {
		const difference_count = different_character_count(pattern[i], pattern[i + 1]);
		if (difference_count == 0 || (difference_count == 1 && allow_smudge)) {
			candidate_mirror_lines.push(i);
		}
	}

	const mirror_line = candidate_mirror_lines.filter((possible_mirror_line) => {
		let smudge_used = false;
		for (
			let offset = 0;
			possible_mirror_line - offset >= 0 && possible_mirror_line + offset + 1 < pattern.length;
			offset++
		) {
			const line_a = pattern[possible_mirror_line - offset];
			const line_b = pattern[possible_mirror_line + offset + 1];
			const difference_count = different_character_count(line_a, line_b);

			if (difference_count > 1) {
				return false;
			}

			if (difference_count == 1) {
				if (smudge_used || !allow_smudge) {
					return false;
				} else {
					smudge_used = true;
				}
			}
		}

		if (allow_smudge && !smudge_used) {
			return false;
		}

		return true;
	});

	if (mirror_line.length > 1) {
		throw new Error('Bug found');
	}

	return mirror_line.length == 0 ? 0 : mirror_line[0] + 1;
}

function transpose(pattern: string[]): string[] {
	return Object.keys(pattern[0]).map((col) => {
		return pattern.map((row) => row[col]).join('');
	});
}
