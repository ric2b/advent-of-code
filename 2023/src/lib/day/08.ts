export function part1(raw_input: string): number {
	const [raw_instructions, , ...raw_locations] = raw_input.trim().split('\n');

	const instructions = [...raw_instructions.match(/([LR])/g)];

	const locations = new Map();
	raw_locations.map((raw_location) => {
		const [, location, left, right] = /(\w+) = \((\w+), (\w+)\)/g.exec(raw_location);
		locations.set(location, [left, right]);
	});

	let current_location = 'AAA';
	let step = 0;

	while (current_location != 'ZZZ') {
		const current_instruction = instructions[step % instructions.length];

		const neighbours = locations.get(current_location);
		current_location = current_instruction == 'L' ? neighbours[0] : neighbours[1];

		step++;
	}

	return step;
}

export function part2(raw_input: string): number {
	const [raw_instructions, , ...raw_locations] = raw_input.trim().split('\n');

	const instructions = [...raw_instructions.match(/([LR])/g)];

	const locations = new Map();
	raw_locations.map((raw_location) => {
		const [, location, left, right] = /(\w+) = \((\w+), (\w+)\)/g.exec(raw_location);
		locations.set(location, [left, right]);
	});

	const current_locations = [...locations.keys()].filter((k) => k.endsWith('A'));
	const loop_lengths = new Array(current_locations.length);

	for (let i = 0; i < current_locations.length; i++) {
		let current_location = current_locations[i];
		let step = 0;

		while (!current_location.endsWith('Z')) {
			const current_instruction = instructions[step % instructions.length];

			const neighbours = locations.get(current_location);
			current_location = current_instruction == 'L' ? neighbours[0] : neighbours[1];

			step++;
		}

		loop_lengths[i] = step;
	}

	return lcmArray(loop_lengths);
}

function lcmArray(numArray) {
	return numArray.reduce((a, b) => (a * b) / gcd(a, b), 1);
}

function gcd(a, b) {
	return b === 0 ? a : gcd(b, a % b);
}
