interface location {
	row: number;
	col: number;
}

export function part1(raw_input: string): number {
	return part2(raw_input, 2);
}

export function part2(raw_input: string, expansion_factor: number = 1): number {
	const universe: string[] = raw_input.trim().split('\n');

	const empty_rows = empty_row_indexes(universe);
	const empty_columns = empty_column_indexes(universe);

	const galaxy_locations = [];
	for (let row = 0; row < universe.length; row++) {
		for (let col = 0; col < universe[0].length; col++) {
			if (universe[row][col] == '#') {
				galaxy_locations.push({ row, col });
			}
		}
	}

	const distances: number[] = [];
	for (let i = 0; i < galaxy_locations.length; i++) {
		for (let j = i + 1; j < galaxy_locations.length; j++) {
			const distance = expanded_manhattan_distance(
				empty_rows,
				empty_columns,
				expansion_factor,
				galaxy_locations[i],
				galaxy_locations[j]
			);

			distances.push(distance);
		}
	}

	return distances.reduce((a, b) => a + b);
}

// export function filtered_grid(raw_input: string): string[] {
// 	const grid: string[] = raw_input.trim().split('\n');
//
// 	const start_row = grid.findIndex((s) => s.includes('S'));
// 	const start_col = grid[start_row].indexOf('S');
// 	const start_location: location = { row: start_row, col: start_col };
//
// 	const distances = dijkstra(grid, start_location);
//
// 	grid[start_row] = grid[start_row].replace('S', start_type(grid, start_location));
//
// 	const filtered_grid = grid.map((grid_row, row) => {
// 		return Array.from(grid_row)
// 			.map((char, col) => {
// 				if (!distances.has(key({ row, col }))) {
// 					return ' ';
// 				}
//
// 				switch (char) {
// 					case 'J':
// 						return '┘';
// 					case 'L':
// 						return '└';
// 					case 'F':
// 						return '┌';
// 					case '7':
// 						return '┐';
// 					case '-':
// 						return '─';
// 					case '|':
// 						return '│';
// 				}
// 			})
// 			.join('');
// 	});
//
// 	return filtered_grid;
// }

function expanded_manhattan_distance(
	empty_rows,
	empty_cols,
	expansion_factor,
	a: location,
	b: location
): number {
	const empty_rows_between = empty_rows.filter(
		(row) => row > Math.min(a.row, b.row) && row < Math.max(a.row, b.row)
	).length;
	const empty_cols_between = empty_cols.filter(
		(col) => col > Math.min(a.col, b.col) && col < Math.max(a.col, b.col)
	).length;
	const vertical_expansion = expansion_factor * empty_rows_between - empty_rows_between;
	const horizontal_expansion = expansion_factor * empty_cols_between - empty_cols_between;

	return (
		Math.abs(a.row - b.row) + Math.abs(a.col - b.col) + vertical_expansion + horizontal_expansion
	);
}

function empty_row_indexes(universe: string[]): number[] {
	const empty_rows = [];

	for (let row = 0; row < universe.length; row++) {
		if (!universe[row].includes('#')) {
			empty_rows.push(row);
		}
	}

	return empty_rows;
}

function empty_column_indexes(universe: string[]): number[] {
	const empty_columns = [];

	for (let col = 0; col < universe[0].length; col++) {
		let empty_column = true;
		for (let row = 0; row < universe.length; row++) {
			if (universe[row][col] == '#') {
				empty_column = false;
				break;
			}
		}
		if (empty_column) {
			empty_columns.push(col);
		}
	}

	return empty_columns;
}
