export function part1(raw_input: string): number {
	const grid: number[][] = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split('').map(Number));

	const start = new Location(0, 0);
	const reached_from = dijkstra(grid, start);
	const destination = new Location(grid[0].length - 1, grid.length - 1);

	let path_heat_loss = 0;
	let previous = destination;
	while (previous.key != start.key) {
		const heat_loss = previous.grid_value(grid)
		console.log({y: previous.y, x: previous.x, heat_loss})
		path_heat_loss += heat_loss;
		previous = reached_from.get(previous.key).node;
	}

	return path_heat_loss;
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

type Direction = 'N' | 'S' | 'E' | 'W';

class Location {
	public readonly x: number;
	public readonly y: number;
	public readonly key: string;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
		this.key = Location.build_key(x, y);
	}

	grid_value(grid: number[][]): number {
		return grid[this.y][this.x];
	}

	neighbors(grid: number[][]): Array<[Location, Direction]> {
		const valid_neighbors: Array<[Location, Direction]> = [
			[new Location(this.x, this.y - 1), 'N'],
			[new Location(this.x, this.y + 1), 'S'],
			[new Location(this.x + 1, this.y), 'E'],
			[new Location(this.x - 1, this.y), 'W'],
		].filter(([l, d]: [Location, Direction]) => {
			return l.y >= 0 && l.y < grid.length && l.x >= 0 && l.x < grid[0].length;
		});

		return valid_neighbors;
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number)
		return new Location(x, y);
	}
}

interface ReachedFrom {
	node: Location;
	direction: Direction;
	steps: number;
}

function dijkstra(grid: number[][], start: Location): Map<string, ReachedFrom> {
	const reached_from: Map<string, ReachedFrom> = new Map([[start.key, { node: new Location(-1, -1), direction: 'E', steps: 0 }]]);

	const distances: Map<string, number> = new Map();
	grid.forEach((row, y) => row.forEach((_, x) => distances.set(Location.build_key(x, y), Infinity)));

	const unvisited: Set<string> = new Set();
	grid.forEach((row, y) => row.forEach((_, x) => unvisited.add(Location.build_key(x, y))));

	distances.set(start.key, 0);

	while (unvisited.size > 0) {
		const current =  Location.from_key(min_distance_unvisited(distances, unvisited));
		unvisited.delete(current.key);

		const node_reached_from = reached_from.get(current.key)

		for (const [neighbor, direction] of current.neighbors(grid).filter(([neighbor, ]) => unvisited.has(neighbor.key))) {
			const distance_to_neighbor = distances.get(current.key) + neighbor.grid_value(grid);
			const steps = direction == node_reached_from.direction ? node_reached_from.steps + 1 : 1

			if (steps <= 3 && distance_to_neighbor < distances.get(neighbor.key)) {
				distances.set(neighbor.key, distance_to_neighbor);
				reached_from.set(neighbor.key, { node: current, direction, steps });
			}
		}
	}

	return reached_from;
}

function min_distance_unvisited(distances: Map<string, number>, unvisited: Set<string>): string {
	let [min_key, min_distance]: [string, number] = ['', Infinity];

	unvisited.forEach(key => {
		const distance: number = distances.get(key);
		if (distance < min_distance) {
			[min_key, min_distance] = [key, distance];
		}
	});

	return min_key;
}
