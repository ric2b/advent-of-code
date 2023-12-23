export function part1(raw_input: string): number {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	const start = new Location(grid.at(0).findIndex(char => char == '.'), 0);
	const end = new Location(grid.at(-1).findIndex(char => char == '.'), grid.length - 1);

	return longest_path(grid, start, end);
}

export function part1graph(raw_input: string): number {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	const graph = new Graph(grid, true);
	graph.compact();

	const start: Node = new Node(new Location(grid.at(0).findIndex(char => char == '.'), 0));
	const end: Node = new Node(new Location(grid.at(-1).findIndex(char => char == '.'), grid.length - 1));

	return longest_path_graph(graph, start, end);
}

export function part2(raw_input: string): number {
	const grid: string[][] = raw_input.trim().split('\n').map((line) => line.split(''));

	const graph = new Graph(grid);
	graph.compact();

	const start: Node = new Node(new Location(grid.at(0).findIndex(char => char == '.'), 0));
	const end: Node = new Node(new Location(grid.at(-1).findIndex(char => char == '.'), grid.length - 1));

	return longest_path_graph(graph, start, end);
}

function longest_path_graph(graph: Graph, start: Node, end: Node): number {
	const cost: Map<string, number> = new Map([[start.key, 0]]);
	const reached_from: ([NodeId, Set<NodeId>])[] = [[start.key, new Set()]];

	while (reached_from.length > 0) {
		const [current_node_key, path_node_keys] = reached_from.pop();

		if (current_node_key == end.key) {
			continue;
		}

		for (const edge of graph.edges.get(current_node_key)) {
			if (path_node_keys.has(edge.node.key)) {
				continue;
			}

			const neighbor_cost = cost.get(current_node_key) + edge.cost;

			if (!cost.has(edge.node.key) || neighbor_cost > cost.get(edge.node.key)) {
				cost.set(edge.node.key, neighbor_cost);

				const neighbor_path = new Set([...path_node_keys, current_node_key]);
				reached_from.unshift([edge.node.key, neighbor_path]);
			}
		}
	}

	return cost.get(end.key);
}

function longest_path(grid: string[][], start: Location, end: Location, slippery: boolean = true): number {
	const cost: Map<string, number> = new Map([[start.key, 0]]);
	const reached_from: ([string, string[]])[] = [[start.key, []]];

	while (reached_from.length > 0) {
		const [current_location_key, location_reached_from] = reached_from.pop();
		const current_location = Location.from_key(current_location_key);

		if (current_location_key == end.key) {
			continue;
		}

		for (const neighbor of current_location.neighbors(grid, slippery)) {
			const neighbor_cost = cost.get(current_location_key) + 1;

			if (location_reached_from.includes(neighbor.key)) {
				continue;
			}

			if (!cost.has(neighbor.key) || neighbor_cost > cost.get(neighbor.key)) {
				cost.set(neighbor.key, neighbor_cost);

				const neighbor_path = [...location_reached_from, neighbor.key];
				reached_from.unshift([neighbor.key, neighbor_path]);
			}
		}
	}

	return cost.get(end.key);
}

type Edge = { node: Node, cost: number };
type NodeId = string;
class Node {
	public readonly location: Location;
	public readonly value: unknown;
	public readonly key: NodeId;

	constructor(location: Location, value: unknown) {
		this.location = location;
		this.value = value;
		this.key = location.key;
	}
}

class Graph {
	public readonly nodes: Map<NodeId, Node>;
	public readonly edges: Map<NodeId, Edge[]>;

	constructor(grid: string[][], slippery: boolean = false) {
		this.nodes = new Map();
		this.edges = new Map();
		grid.forEach((row, y) => {
			row.forEach((cell, x) => {
				if (cell != '#') {
					const node = new Node(new Location(x, y), cell);
					this.nodes.set(node.key, node);
				}
			})
		});

		for (const node of this.nodes.values()) {
			const neighbors: Node[] = node.location.neighbors(grid, slippery).map(l => this.nodes.get(l.key));
			this.edges.set(node.key, neighbors.map(n => ({ node: n, cost: 1 })));
		}
	}

	compact() {
		for (const node_key of this.edges.keys()) {
			const node_edges: Edge[] = this.edges.get(node_key);
			if (node_edges.length === 2) {
				const [to_a, to_b] = node_edges;
				const [from_a, from_b] = node_edges.map(to_edge => {
					return this.edges.get(to_edge.node.key).find(from_edge => from_edge.node.key === node_key)
				});
				if (from_a) {
					from_a.node = to_b.node;
					from_a.cost += to_b.cost;
				}
				if (from_b) {
					from_b.node = to_a.node;
					from_b.cost += to_a.cost;
				}

				this.nodes.delete(node_key);
				this.edges.delete(node_key);
			}
		}
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

	neighbors(grid: string[][], slippery: boolean = false): Location[] {
		const grid_height = grid.length;
		const grid_width = grid[0].length;

		if (slippery) {
			switch (grid[this.y][this.x]) {
				case '^': return [new Location(this.x, this.y - 1)];
				case 'v': return [new Location(this.x, this.y + 1)];
				case '>': return [new Location(this.x + 1, this.y)];
				case '<': return [new Location(this.x - 1, this.y)];
			}
		}

		return [
			new Location(this.x, this.y - 1),
			new Location(this.x, this.y + 1),
			new Location(this.x + 1, this.y),
			new Location(this.x - 1, this.y),
		].filter(l => {
			return l.y >= 0 && l.y < grid_height && l.x >= 0 && l.x < grid_width && grid[l.y][l.x] != '#';
		});
	}

	static build_key(x: number, y: number): string {
		return `${x},${y}`;
	}

	static from_key(key: string): Location {
		const [x, y] = key.split(',').map(Number);
		return new Location(x, y);
	}
}
