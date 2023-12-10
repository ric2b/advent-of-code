import {vi} from "vitest";

interface location {
	row: number,
	col: number
}

export function part1(raw_input: string): number {
	const grid: string[] = raw_input.trim().split('\n');

	const start_row = grid.findIndex(s => s.includes('S'));
	const start_col = grid[start_row].indexOf('S');
	const start_location: location = {row: start_row, col: start_col};

	const distances= dijkstra(grid, start_location)
	return Math.max(...distances.values())
}

export function part2(raw_input: string): number {
	const grid: string[] = raw_input.trim().split('\n');

	const start_row = grid.findIndex(s => s.includes('S'));
	const start_col = grid[start_row].indexOf('S');
	const start_location: location = {row: start_row, col: start_col};

	const distances= dijkstra(grid, start_location)

	grid[start_row] = grid[start_row].replace('S', start_type(grid, start_location))

	return count_points_inside_loop(grid, new Set(distances.keys()));
}

function count_points_inside_loop(grid: string[], loop_sections: Set<string>): number {
	let inside_loop = false;
	let counter = 0;

	for(let row = 0; row < grid.length; row++) {
		for(let col = 0; col < grid[0].length; col++) {
			if(loop_sections.has(key({row, col}))) {
				switch (grid[row][col]) {
					case '-':
						break;
					case '|':
						inside_loop = !inside_loop
						break;
					case 'J':
						inside_loop = !inside_loop
						break;
					case 'L':
						inside_loop = !inside_loop
						break;
					case 'F':
						// no change, we're "riding the top" of the tubes so there's no crossing over
						break;
					case '7':
						// no change, we're "riding the top" of the tubes so there's no crossing over
						break;
				}
			} else if (inside_loop) {
				counter++;
			}
		}
	}

	return counter;
}

function start_type(grid: string[], location: location): '-'|'|'|'J'|'L'|'F'|'7' {
	let candidates = [];
	if (['|', '7', 'F'].includes(grid[location.row - 1]?.[location.col])) candidates.push({row: location.row - 1, col: location.col});
	if (['|', 'J', 'L'].includes(grid[location.row + 1]?.[location.col])) candidates.push({row: location.row + 1, col: location.col});
	if (['-', 'F', 'L'].includes(grid[location.row]?.[location.col - 1])) candidates.push({row: location.row, col: location.col - 1});
	if (['-', '7', 'J'].includes(grid[location.row]?.[location.col + 1])) candidates.push({row: location.row, col: location.col + 1});

	if (candidates.every(c => c.row == location.row)) return '-';
	if (candidates.every(c => c.col == location.col)) return '|';
	if (candidates[0].row < location.row && candidates[1].col < location.col) return 'J';
	if (candidates[0].row < location.row && candidates[1].col > location.col) return 'L';
	if (candidates[0].row > location.row && candidates[1].col < location.col) return '7';
	if (candidates[0].row > location.row && candidates[1].col > location.col) return 'F';

	throw new Error("Bug found");
}

function dijkstra(grid: string[], start: location) {
	const unvisited: Map<string, number> = new Map();
	const distances: Map<string, number> = new Map();

	distances.set(key(start), 0);
	unvisited.set(key(start), 0);

	while (unvisited.size > 0) {
		const [node_key, ] = Array.from(unvisited.entries()).reduce((keyOfMinValue, [key, value]) => {
			return value < unvisited.get(keyOfMinValue) ? key : keyOfMinValue
		});
		unvisited.delete(node_key);
		const [row, col] = node_key.split(',').map(Number);
		const node = {row, col};

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
			candidates = [{row: location.row - 1, col: location.col}, {row: location.row + 1, col: location.col}];
			break;
		case '-':
			candidates = [{row: location.row, col: location.col - 1}, {row: location.row, col: location.col + 1}];
			break;
		case 'L':
			candidates = [{row: location.row - 1, col: location.col}, {row: location.row, col: location.col + 1}];
			break;
		case 'J':
			candidates = [{row: location.row - 1, col: location.col}, {row: location.row, col: location.col - 1}];
			break;
		case '7':
			candidates = [{row: location.row + 1, col: location.col}, {row: location.row, col: location.col - 1}];
			break;
		case 'F':
			candidates = [{row: location.row + 1, col: location.col}, {row: location.row, col: location.col + 1}];
			break;
		case 'S':
			candidates = [];
			if (['|', '7', 'F'].includes(grid[location.row - 1]?.[location.col])) candidates.push({row: location.row - 1, col: location.col});
			if (['|', 'J', 'L'].includes(grid[location.row + 1]?.[location.col])) candidates.push({row: location.row + 1, col: location.col});
			if (['-', 'F', 'L'].includes(grid[location.row]?.[location.col - 1])) candidates.push({row: location.row, col: location.col - 1});
			if (['-', '7', 'J'].includes(grid[location.row]?.[location.col + 1])) candidates.push({row: location.row, col: location.col + 1});
			break;
		default:
			candidates = [];
	}

	return inside_grid(grid, candidates);
}

function inside_grid(grid: string[], locations: location[]): location[] {
	const rows = grid.length
	const cols = grid[0].length
	return locations.filter(l => l.row >= 0 && l.row < rows && l.col >= 0 && l.col < cols);
}