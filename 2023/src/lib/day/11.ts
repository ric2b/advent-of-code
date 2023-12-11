interface location {
	row: number;
	col: number;
}

export function part1(raw_input: string): number {
	const grid: string[] = raw_input.trim().split('\n');

	const expanded_universe = expand(grid)

	const galaxy_locations = []
	for (let row = 0; row < expanded_universe.length; row++) {
		for (let col = 0; col < expanded_universe[0].length; col++) {
			if (expanded_universe[row][col] == '#') {
				galaxy_locations.push({row, col})
			}
		}
	}

	const distances: number[] = []
	for(let i = 0; i < galaxy_locations.length; i++) {
		for(let j = i + 1; j < galaxy_locations.length; j++) {
			distances.push(manhattan_distance(galaxy_locations[i], galaxy_locations[j]));
		}
	}

	// const distances = dijkstra(grid, start_location);
	// return Math.max(...distances.values());
	return distances.reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	return 2;
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

function manhattan_distance(a: location, b: location): number {
	return Math.abs(a.row - b.row) + Math.abs(a.col - b.col);
}

function expand(grid: string[]): string[] {

	const empty_columns = new Array(grid[0].length).fill(true);
	// TODO: might be slow to not iterate by row
	for (let col = 0; col < grid[0].length; col++) {
		for (let row = 0; row < grid.length; row++) {
			if (grid[row][col] == '#') {
				empty_columns[col] = false;
				break;
			}
		}
	}

	const horizontally_expanded = []
	for (let row = 0; row < grid.length; row++) {
		horizontally_expanded.push([...grid[row]].flatMap((char, col) => empty_columns[col] ? [char, char] : [char]).join(''));
	}

	return horizontally_expanded.flatMap(row => row.includes('#') ? row : [row, row]);
}

function dijkstra(grid: string[], start: location) {
	const unvisited: Map<string, number> = new Map();
	const distances: Map<string, number> = new Map();

	distances.set(key(start), 0);
	unvisited.set(key(start), 0);

	while (unvisited.size > 0) {
		const [node_key] = Array.from(unvisited.entries()).reduce((keyOfMinValue, [key, value]) => {
			return value < unvisited.get(keyOfMinValue) ? key : keyOfMinValue;
		});
		unvisited.delete(node_key);
		const [row, col] = node_key.split(',').map(Number);
		const node = { row, col };

		const distanceFromStart = distances.get(node_key) + 1;
		for (const neighbor of getNeighbors(grid, node)) {
			const neighbor_key = key(neighbor);
			if (distances.has(neighbor_key)) {
				if (distanceFromStart < distances.get(neighbor_key)) {
					distances.set(neighbor_key, distanceFromStart);
				}
			} else {
				unvisited.set(neighbor_key, distanceFromStart);
				distances.set(neighbor_key, distanceFromStart);
			}
		}
	}

	return distances;
}

function key(location: location): string {
	return `${location.row},${location.col}`;
}

function getNeighbors(grid: string[], location: location): location[] {
	let candidates: location[];
	switch (grid[location.row][location.col]) {
		case '|':
			candidates = [
				{ row: location.row - 1, col: location.col },
				{ row: location.row + 1, col: location.col }
			];
			break;
		case '-':
			candidates = [
				{ row: location.row, col: location.col - 1 },
				{ row: location.row, col: location.col + 1 }
			];
			break;
		case 'L':
			candidates = [
				{ row: location.row - 1, col: location.col },
				{ row: location.row, col: location.col + 1 }
			];
			break;
		case 'J':
			candidates = [
				{ row: location.row - 1, col: location.col },
				{ row: location.row, col: location.col - 1 }
			];
			break;
		case '7':
			candidates = [
				{ row: location.row + 1, col: location.col },
				{ row: location.row, col: location.col - 1 }
			];
			break;
		case 'F':
			candidates = [
				{ row: location.row + 1, col: location.col },
				{ row: location.row, col: location.col + 1 }
			];
			break;
		case 'S':
			candidates = [];
			if (['|', '7', 'F'].includes(grid[location.row - 1]?.[location.col]))
				candidates.push({ row: location.row - 1, col: location.col });
			if (['|', 'J', 'L'].includes(grid[location.row + 1]?.[location.col]))
				candidates.push({ row: location.row + 1, col: location.col });
			if (['-', 'F', 'L'].includes(grid[location.row]?.[location.col - 1]))
				candidates.push({ row: location.row, col: location.col - 1 });
			if (['-', '7', 'J'].includes(grid[location.row]?.[location.col + 1]))
				candidates.push({ row: location.row, col: location.col + 1 });
			break;
		default:
			candidates = [];
	}

	return inside_grid(grid, candidates);
}

function inside_grid(grid: string[], locations: location[]): location[] {
	const rows = grid.length;
	const cols = grid[0].length;
	return locations.filter((l) => l.row >= 0 && l.row < rows && l.col >= 0 && l.col < cols);
}
