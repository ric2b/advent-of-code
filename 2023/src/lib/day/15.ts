export function part1(raw_input: string): number {
	return raw_input
		.trim()
		.split(',')
		.map(hash)
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	const boxes: Map<string, number>[] = Array.from({ length: 256 }, () => new Map());

	raw_input
		.trim()
		.split(',')
		.forEach((step) => {
			const [, label, operation, focal_length] =
				/(?<label>[a-z]+)(?<operation>[=-])(?<focal_lenght>\d)?/.exec(step);

			switch (operation) {
				case '-':
					boxes[hash(label)].delete(label);
					break;
				case '=':
					boxes[hash(label)].set(label, focal_length);
					break;
				default:
					throw new Error('bug');
			}
		});

	const focusing_powers = boxes.map((box, box_index) => {
		return [...box.entries()]
			.map(([label, focal_length], i) => (box_index + 1) * (i + 1) * focal_length)
			.reduce((a, b) => a + b, 0);
	});

	return focusing_powers.reduce((a, b) => a + b);
}

function hash(s: string) {
	return s.split('').reduce((current, char) => {
		current += char.charCodeAt(0);
		current *= 17;
		current %= 256;
		return current;
	}, 0);
}
