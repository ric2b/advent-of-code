export function part1(raw_input: string): number {
	const grid = raw_input
		.trim()
		.split('\n')
		.map(l => l.split(''));

	roll_north(grid);

	return load_north(grid);
}

export function part2(raw_input: string): number {
	return 2;
}

function load_north(grid: string[][]): number {
	return grid
		.reverse()
		.map((row, i) => (i+1) * row.filter(c => c == 'O').length)
		.reduce((a, b) => a + b);
}

function roll_north(grid: string[][]): void {
	const free_rows: (number|undefined)[] = grid[0].map(char => char == '.' ? 0 : undefined);

	for (let row = 1; row < grid.length; row++) {
		for (let col = 0; col < grid[0].length; col++) {
			const free_row = free_rows[col];
			switch (grid[row][col]) {
				case '#':
					free_rows[col] = undefined;
					break;
				case '.':
					if (free_row == undefined) {
						free_rows[col] = row;
					}
					break;
				case 'O':
					if (free_row != undefined) {
						grid[free_row][col] = 'O';
						grid[row][col] = '.';
						for (let i = free_row + 1; i <= row; i++) {
							if (grid[i][col] == '.') {
								free_rows[col] = i;
								break;
							}
						}
					}
			}
		}
	}
}