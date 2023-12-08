export function part1(raw_input: string): number {
	const [raw_instructions, , ...raw_locations] = raw_input.trim().split('\n');

	const instructions = [...raw_instructions.match(/([LR])/g)]

	const locations = new Map();
	raw_locations.map(raw_location => {
		const [, location, left, right] = /(\w+) = \((\w+), (\w+)\)/g.exec(raw_location)
		if (locations.has(location)) { 1/0 }
		locations.set(location, [left, right])
	})

	let current_location = 'AAA'
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

	const instructions = [...raw_instructions.match(/([LR])/g)]

	const locations = new Map();
	raw_locations.map(raw_location => {
		const [, location, left, right] = /(\w+) = \((\w+), (\w+)\)/g.exec(raw_location)
		if (locations.has(location)) { 1/0 }
		locations.set(location, [left, right])
	})

	let current_locations = [...locations.keys()].filter(k => k.endsWith('A'))
	let step = 0;

	while (!current_locations.every(l => l.endsWith('Z'))) {
		const current_instruction = instructions[step % instructions.length];
		const neighbour_index = current_instruction == 'L' ? 0 : 1

		for(let i = 0; i < current_locations.length; i++) {
			current_locations[i] = locations.get(current_locations[i])[neighbour_index];
		}

		step++;
	}

	return step;
}
