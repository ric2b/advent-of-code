export function part1(raw_input: string): number {
	const grid: number[][] = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split('').map(Number));

	const start_location = new Location(0, 0);
	const goal_location = new Location(grid[0].length - 1, grid.length - 1);

	console.log('building graph');
	const graph = new Graph(grid, start_location, goal_location);
	console.log('calculating minimum distances');
	const distances = dijkstra(grid, graph, start_location);

	let lowest_heat_loss = Infinity;
	distances.forEach((heat_loss, node_key) => {
		if (node_key.startsWith(goal_location.key) && heat_loss < lowest_heat_loss) {
			lowest_heat_loss = heat_loss;
		}
	});

	return lowest_heat_loss;
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


function dijkstra(grid: number[][], graph: Graph, start_location: Location): Map<NodeId, number> {
	const unvisited: Set<NodeId> = new Set(graph.nodes.keys());
	const distances: Map<NodeId, number> = new Map();
	unvisited.forEach(node_key => distances.set(node_key, Infinity));

	const start_node = new Node(start_location, 'E', 0);
	distances.set(start_node.key, 0);

	while (unvisited.size > 0) {
		const current_node_key: NodeId =  min_distance_unvisited(unvisited, distances);
		unvisited.delete(current_node_key);

		for (const neighbor of graph.edges.get(current_node_key)) {
			const distance_to_neighbor = distances.get(current_node_key) + neighbor.location.grid_value(grid);

			if (distance_to_neighbor < distances.get(neighbor.key)) {
				distances.set(neighbor.key, distance_to_neighbor);
			}
		}
	}

	return distances;
}

function min_distance_unvisited(unvisited: Set<NodeId>, distances: Map<NodeId, number>): NodeId {
	let [min_key, min_distance]: [NodeId, number] = ['', Infinity];

	unvisited.forEach(key => {
		const distance: number = distances.get(key);
		if (distance < min_distance) {
			[min_key, min_distance] = [key, distance];
		}
	});

	return min_key;
}

class Graph {
	public readonly nodes: Map<NodeId, Node>;
	public readonly edges: Map<NodeId, Node[]>;

	constructor(grid: number[][], start_location: Location, goal_location: Location) {
		this.nodes = new Map();
		this.edges = new Map();

		const visited: Set<NodeId> = new Set();
		const queue: Node[] = [new Node(start_location, 'E', 0)];

		while (queue.length > 0) {
			const current_node: Node = queue.shift();
			visited.add(current_node.key);
			const neighbors = current_node.location.key === goal_location.key ? [] : current_node.neighbors(grid);

			this.nodes.set(current_node.key, current_node);
			this.edges.set(current_node.key, neighbors);
			neighbors.forEach(neighbor => {
				if(!visited.has(neighbor.key)) {
					queue.push(neighbor);
				}
			});
		}
	}
}

type Direction = 'N' | 'S' | 'E' | 'W';
type NodeId = string;
class Node {
	public readonly location: Location;
	public readonly last_direction: Direction;
	public readonly steps: number;
	public readonly key: NodeId;

	constructor(location: Location, last_direction: Direction, steps: number) {
		this.location = location;
		this.last_direction = last_direction;
		this.steps = steps;
		this.key = this.build_key();
	}

	neighbors(grid: number[][]): Node[] {
		return this
			.possible_directions()
			.map(direction => {
				const neighbor_location = this.location.neighbor(direction);
				const steps = direction === this.last_direction ? this.steps + 1 : 1;
				return new Node(neighbor_location, direction, steps);
			})
			.filter(node => node.location.inside_grid(grid));
	}

	possible_directions(): Direction[] {
		const directions: Direction[] = this.steps < 3 ? [this.last_direction] : [];
		switch (this.last_direction) {
			case 'N':
			case 'S':
				directions.push('E', 'W');
				break;
			case 'E':
			case 'W':
				directions.push('N', 'S');
				break;
		}
		return directions;
	}

	build_key() {
		return `${this.location.key}:${this.last_direction}:${this.steps}`;
	}

	static from_key(key: string): Node {
		const [location_key, direction, raw_steps] = key.split(':');
		return new Node(Location.from_key(location_key), direction, Number(raw_steps));
	}
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

	inside_grid(grid: number[][]): boolean {
		return this.x >= 0 && this.x < grid[0].length && this.y >= 0 && this.y < grid.length;
	}

	grid_value(grid: number[][]): number {
		return grid[this.y][this.x];
	}

	neighbor(direction: Direction): Location {
		switch (direction) {
			case "N": return new Location(this.x, this.y - 1);
			case "S": return new Location(this.x, this.y + 1);
			case "E": return new Location(this.x + 1, this.y);
			case "W": return new Location(this.x - 1, this.y);
		}
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number)
		return new Location(x, y);
	}
}
