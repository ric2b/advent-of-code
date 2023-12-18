const turns: Map<string, string> = new Map([
	['UR', '┌'],
	['LD', '┌'],
	['UL', '┐'],
	['RD', '┐'],
	['DL', '┘'],
	['RU', '┘'],
	['DR', '└'],
	['LU', '└'],
])

export function part1(raw_input: string): number {
	let location: Vector = new Vector(0, 0);
	let previous_direction = 'U';

	const perimeter: Map<string, string> = new Map();
	raw_input
		.trim()
		.split('\n')
		.forEach(line => {
			const [, direction, raw_distance, color] = /(?<direction>[UDLR]) (?<distance>\d+) \((?<color>[#\w]+)\)/.exec(line);
			const distance = Number(raw_distance);

			let section_end;
			switch (direction) {
				case 'U':
					section_end = new Vector(location.x, location.y - distance);
					break;
				case 'D':
					section_end = new Vector(location.x, location.y + distance);
					break;
				case 'L':
					section_end = new Vector(location.x - distance, location.y);
					break;
				case 'R':
					section_end = new Vector(location.x + distance, location.y)
					break;
				default:
					throw new Error('bug');
			}

			switch (direction) {
				case 'U':
				case 'D':
					for (let y = Math.min(location.y, section_end.y); y <=  Math.max(location.y, section_end.y); y++) {
						perimeter.set(Vector.key(location.x, y), '│');
					}
					break;
				case 'L':
				case 'R':
					for (let x = Math.min(location.x, section_end.x); x <=  Math.max(location.x, section_end.x); x++) {
						perimeter.set(Vector.key(x, location.y), '─');
					}
					break;
				default:
					throw new Error('bug');
			}

			const curve_id = `${previous_direction}${direction}`;
			perimeter.set(location.key, turns.get(curve_id));
			location = section_end;
			previous_direction = direction;
		});

	perimeter.set(location.key, turns.get('UR'));

	const grid = perimeter_to_grid(perimeter);

	// grid.forEach(line => console.log(line.join('')));

	return count_points_inside_loop(grid) + perimeter.size;
}

export function part2(raw_input: string): number {
	return 2
}

// export function part_1_grid(raw_input: string): string[] {
// 	const grid = raw_input
// 		.trim()
// 		.split('\n')
// 		.map((line) => line.split(''));
//
// 	const initial_beam = new Beam(new Vector(-1, 0), new Vector(1, 0));
// 	bounce_beam(grid, initial_beam).forEach((location) => {
// 		const [x, y] = location.split(',').map(Number);
// 		grid[y][x] = '*';
// 	});
//
// 	return grid.map((row) => row.map((c) => (c == '*' ? c : ' ')).join(''));
// }

class Vector {
	public readonly x: number;
	public readonly y: number;
	public readonly key: string;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
		this.key = Vector.key(x, y);
	}

	add(other: Vector): Vector {
		return new Vector(this.x + other.x, this.y + other.y);
	}

	static key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Vector {
		const [x, y] = key.split(',').map(Number);
		return new Vector(x, y);
	}
}

function count_points_inside_loop(grid: string[][]): number {
	let inside_loop = false;
	let counter = 0;

	for (let row = 0; row < grid.length; row++) {
		let row_counter = 0;
		for (let col = 0; col < grid[0].length; col++) {
			switch (grid[row][col]) {
				case '│':
				case '┘':
				case '└':
					inside_loop = !inside_loop;
					break;
				case '┌':
				case '┐':
				case '─':
					// no change, we're "riding the top" of the tubes so there's no crossing over
					break;
				case ' ':
					if (inside_loop) {
						row_counter++;
					}
					break;
				default:
					throw new Error('bug');
			}
		}
		counter += row_counter;
	}

	return counter;
}

function perimeter_to_grid(perimeter: Map<string, string>): string[][] {
	let [min_height, min_width, max_height, max_width] = [Infinity, Infinity, -Infinity, -Infinity];
	perimeter.forEach((char, location_key) => {
		const l = Vector.from_key(location_key);
		if (min_height > l.y) { min_height = l.y; }
		if (max_height < l.y) { max_height = l.y; }
		if (min_width > l.x) { min_width = l.x; }
		if (max_width < l.x) { max_width = l.x; }
	})

	const grid: string[][] = Array.from({ length: max_height + 1 - min_height }, () => Array.from({ length: max_width + 1 - min_width }).fill(' '));
	perimeter.forEach((char, location_key) => {
		const location = Vector.from_key(location_key);
		grid[location.y - min_height][location.x - min_width] = char;
	})

	return grid;
}
