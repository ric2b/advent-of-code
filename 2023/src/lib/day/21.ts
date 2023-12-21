export function part1(raw_input: string, step_limit: number = 64): number {
	const grid: number[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	let start;
	for(let i = 0; i < grid.length; i++) {
		for(let j = 0; j < grid[0].length; j++) {
			if(grid[i][j] == 'S') {
				start = new Location(j, i);
				break;
			}
		}
	}

	const graph = build_graph(grid, start);

	const reachable_at_step_limit = new Set(bfs(graph, start, step_limit).map(n => n.key));

	return reachable_at_step_limit.size;
}

export function part2(raw_input: string, step_limit: number = 64): number {
	const grid: number[][] = raw_input.trim().split('\n').map((line) => line.split(''));
	const side = grid.length;

	const expanded_grid = Array.from(Array(grid.length * 5), (_, i) => {
		const row = i % grid.length;
		return grid[row].concat(...grid[row], ...grid[row], ...grid[row], ...grid[row]);
	});

	const starts = [];
	for(let i = 0; i < expanded_grid.length; i++) {
		for(let j = 0; j < expanded_grid[0].length; j++) {
			if(expanded_grid[i][j] == 'S') {
				starts.push(new Location(j, i));
			}
		}
	}

	// Q: how many squares at 26501365 steps?

	// row and column for S are completely empty, so the center of other regions can be reached in width steps!
	// walking on one axis, can reach: 1 + (2 * 26501365 / 131) = 404601 regions (2 * because walking in both direction)
	// leftover steps is 26501365 % 131 = 65
	// RHa = 202300 regions reached in one axis means 404601**2 / 2 (rhombus area) regions reached in both axis = 81850984600 regions

	// Rp = each full region has X empty plots
	// final spaces that can be reached are RHa * Rp + permiter region count * reachable in 65 steps from edge

	// 01234
	// 56789
	// 01234
	// 56789
	// 01234
	const start = starts[12]
	const graph = build_graph(expanded_grid, start);

	const goal_steps = 26501365;
	const mod = goal_steps % side;

	const y0 = new Set(bfs(graph, start, mod).map(n => n.key)).size
	const y1 = new Set(bfs(graph, start, mod + side).map(n => n.key)).size
	const y2 = new Set(bfs(graph, start, mod + 2*side).map(n => n.key)).size
	// const [y0, y1, y2] = [3877, 34674, 96159]

	// https://en.wikipedia.org/wiki/Lagrange_polynomial#Example
	// Lagrange's Interpolation formula for ax^2 + bx + c with x=[0,1,2] and y=[y0,y1,y2]
	// f(x) = (x^2-3x+2) * y0/2 - (x^2-2x)*y1 + (x^2-x) * y2/2

	// x = 0
	// c = 2*y0/2
	// c = y0

	// x = 1
	// a1^2 + b1 + c = (1-3+2) * y0/2 - (1-2)*y1 + (1-1) * y2/2
	// a + b + c = y1
	// a + b + y0 = y1
	// a + b = y1 - y0
	// b = (y1 - y0) - a
	// b = (y1 - y0) - (y0/2 - y1 + y2/2)
	// b = y1 - y0 - y0/2 + y1 - y2/2
	// b = -3y0/2 + 2y1 - y2/2

	// x = 2
	// 4a + 2b + c = (2^2-3*2+2) * y0/2 - (2^2-2*2)*y1 + (2^2-2) * y2/2
	// 4a + 2b + c = (4-6+2) * y0/2 - (4-4)*y1 + (4-2) * y2/2
	// 4a + 2b + y0 = y2
	// 4a + 2b = y2 - y0
	// 2a + b = y2/2 - y0/2
	// 2a + (y1 - y0) - a = y2/2 - y0/2
	// a + (y1 - y0) = y2/2 - y0/2
	// a = y2/2 - y0/2 - (y1 - y0)
	// a = y2/2 - y0/2 - y1 + y0
	// a = y0/2 - y1 + y2/2

	/////////////////////////
	// a = y0/2 - y1 + y2/2
	// b = -3y0/2 + 2y1 - y2/2
	// c = y0

	const coeff_a = y0/2 - y1 + y2/2;
	const coeff_b = -3*(y0/2) + 2*y1 - y2/2;
	const coeff_c = y0;
	// const [coeff_a, coeff_b, coeff_c] = [15344, 15453, 3877];

	const N= (goal_steps - mod) / side
	return  coeff_a * N**2 + (coeff_b * N) + coeff_c;
}

class Location {
	public readonly x: number;
	public readonly y: number;
	public readonly key: string;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
		this.key = Location.build_key(x, y);
	}

	neighbors(grid: string[][]): Location[] {
		const grid_height = grid.length;
		const grid_width = grid[0].length;

		return [
			new Location(this.x, this.y - 1),
			new Location(this.x, this.y + 1),
			new Location(this.x + 1, this.y),
			new Location(this.x - 1, this.y),
		].filter(l => {
			return l.y >= 0 && l.y < grid_height && l.x >= 0 && l.x < grid_width && grid[l.y][l.x] != '#';
		})
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number)
		return new Location(x, y);
	}
}

function bfs(graph: Map<string, Location[]>, start: Location, step_limit: number, visited: Set<string> = new Set(), steps: number = 1): Location[] {
	if (steps > step_limit || visited.has(`${start.key};${steps}`)) {
		return [];
	}

	visited.add(`${start.key};${steps}`);
	const neighbors: Location[] = graph.get(start.key);

	if (steps == step_limit) {
		return neighbors;
	}

	return neighbors.flatMap(n => bfs(graph, n, step_limit, visited,steps + 1));
}

function build_graph(grid: number[][], start: Location): Map<string, Location[]> {
	const graph: Map<string, Location[]> = new Map();

	const queue = [start];
	let steps = 0;

	while (queue.length > 0) {
		const location = queue.shift();

		const neighbors = location.neighbors(grid);
		if (!graph.has(location.key)) {
			graph.set(location.key, neighbors);
			queue.push(...neighbors)
		}
	}

	return graph;
}
