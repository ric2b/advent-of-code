interface possible_part {
	value: number,
	line: number;
	column: number;
	n_length: number;
}

interface symbol_location {
	line: number;
	column: number;
}

export function part1(raw_input: string): number {
	const possible_parts: possible_part[] = []
	const symbol_locations: symbol_location[] = []

	raw_input.trim().split('\n').forEach((s, line) => {
		for (const match of [...s.matchAll(/\d+/g)]) {
			const value = Number(match[0])
			const n_length = match[0].length
			const column: number = <number>match.index
			possible_parts.push({value, line, column, n_length })
		}

		for (const match of [...s.matchAll(/[^\d.]/g)]) {
			const column: number = <number>match.index
			symbol_locations.push({line, column})
		}
	});

	const parts = possible_parts.filter(possible_part => {
		const adjacent_to_symbol = symbol_locations.some(symbol_location => {
			const matches_columns = ((symbol_location.column >= possible_part.column - 1) && (symbol_location.column <= possible_part.column + possible_part.n_length));
			const matches_lines = ((symbol_location.line >= possible_part.line - 1) && (symbol_location.line <= possible_part.line + 1));

			return matches_lines && matches_columns;
		});

		return adjacent_to_symbol;
	});

	return parts.map(part => part.value).reduce((a, b) => a + b, 0);
}

export function part2(raw_input: string): number {
	return 2
}
