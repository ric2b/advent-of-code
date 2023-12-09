export function part1(raw_input: string): number {
	const sequences = raw_input
		.trim()
		.split('\n')
		.map((raw_sequence) => {
			return [...raw_sequence.match(/-?\d+/g)].map(Number);
		});

	return sequences
		.map(calculate_derivatives)
		.map(calculate_next_value)
		.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	const sequences = raw_input
		.trim()
		.split('\n')
		.map((raw_sequence) => {
			return [...raw_sequence.match(/-?\d+/g)].map(Number);
		});

	return sequences
		.map((s) => s.reverse())
		.map(calculate_derivatives)
		.map(calculate_next_value)
		.reduce((a, b) => a + b);
}

function calculate_next_value(sequence_derivatives) {
	let derivative = 0;
	for (let i = sequence_derivatives.length - 2; i >= 0; i--) {
		derivative = last(sequence_derivatives[i]) + derivative;
	}

	return derivative;
}

function calculate_derivatives(sequence) {
	const differences = [sequence];

	do {
		const last_derivative = last(differences);
		const new_derivative = [];

		for (let i = 0; i < last_derivative.length - 1; i++) {
			new_derivative.push(last_derivative[i + 1] - last_derivative[i]);
		}

		differences.push(new_derivative);
	} while (!last(differences).every((n) => n == 0));

	return differences;
}

function last(array) {
	return array[array.length - 1];
}
