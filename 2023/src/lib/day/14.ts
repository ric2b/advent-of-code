export function part1(raw_input: string): number {
	const grid = raw_input
		.trim()
		.split('\n')
		.map(l => l.split(''));

	roll_north(grid);

	return load_north(grid);
}

export function part2(raw_input: string): number {
	const grid = raw_input
		.trim()
		.split('\n')
		.map(l => l.split(''));

	const total_loops = 1_000_000_000;
	const previously_seen: Map<string, number> = new Map();
	for (let i = 0; i < total_loops; i++) {
		for(let j = 0; j < 4; j++) {
			roll_north(grid);
			rotate_clockwise(grid);
		}

		const hash = grid_hash(grid)
		if (previously_seen.has(hash)) {
			const cycle_length: number = i - previously_seen.get(hash);
			i = total_loops - ((total_loops - i) % cycle_length)
		}
		else {
			previously_seen.set(hash, i);
		}
	}

	return load_north(grid);
}

function grid_hash(grid: string[][]): string {
	const rock_positions = []
	for (let i = 0; i < grid.length; i++) {
		for (let j = 0; j < grid[0].length; j++) {
			if(grid[i][j] == 'O') {
				rock_positions.push(`${i},${j}`);
			}
		}
	}
	return rock_positions.join(';');
}

function rotate_clockwise(grid: string[][]): void {
	const n = grid.length;
	const x = Math.floor(n/ 2);
	const y = n - 1;
	for (let i = 0; i < x; i++) {
		for (let j = i; j < y - i; j++) {
			const k = grid[i][j];
			grid[i][j] = grid[y - j][i];
			grid[y - j][i] = grid[y - i][y - j];
			grid[y - i][y - j] = grid[j][y - i]
			grid[j][y - i] = k
		}
	}
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